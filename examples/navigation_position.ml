(* 

Copyright © 2019 United States Government as represented by the Administrator of the 
National Aeronautics and Space Administration.  All Rights Reserved.

Author: Kit Siu, Heber Herencia-zapana
Date: 2018-01-22

*)

#use "top.ml";;

(* ----------------------------------------- *)
(* === Big-C functions === *)
(* ----------------------------------------- *)

(* utility functions *)
let rec istr_gen i =
  match i with
    | "1" -> ["i1"]
    | _   -> List.append ["i"^i]
      (istr_gen (string_of_int ((int_of_string i)-1)));;

let rec ostr_gen i =
  match i with
    | "1" -> ["o1"]
    | _   -> List.append ["o"^i]
      (ostr_gen (string_of_int ((int_of_string i)-1)));;

let cross2 x y =
  List.concat
    (List.map ~f:(fun a -> (List.map ~f:(fun b -> a ^ b) y)) x) ;;

let rec cross l = 
  match l with
      [] -> []
    | [x] -> x
    | x::(y::xs) ->
      cross ((cross2 x y)::xs) ;;

let string_of_int_zeros l z =
  let s = string_of_int z in
  let len = String.length s in
  let n1 = l-len in
  let n = if n1>0 then n1 else 0 in
  (String.make n '0') ^ s ;;

let rec num_to_list_ ?(acc=[]) l z =
  if (z<=0)
  then acc
  else num_to_list_ ~acc:((string_of_int_zeros l z)::acc) l (z - 1) ;;

let num_to_list ?(acc=[]) z =
  num_to_list_ ~acc (String.length (string_of_int z)) z ;;

let rec cprod x y =
  List.concat
    (List.map ~f:(fun a ->
      (List.map ~f:(fun b -> (a, b)) y)) x) ;;

let rec clistprod x y =
  List.concat
    (List.map ~f:(fun a ->
      (List.map ~f:(fun b -> [a; b]) y)) x) ;;

let stringlist_2_intlist slist =
  List.map ~f:(fun s -> int_of_string s) slist;;

(* "Big-C" functions *)

(* 
   The C-generators for an IRU and ADC take three arguments
   
   cn: the name of the component 
   fl: a list of the form [(f1,p1);(f2,p2)] where:
   - f1 is the name of the basic event corresponding to a ued failure
   - f2 is the name of the basic event corresponding to an loa failure
   - p1 is a pair consisting of the failure rate and exposure time for a ued failure
   - p2 is a pair consisting of the failure rate and exposure time for an loa failure
   al: the constant [[]] (not used, but needed for type checking)

*)

let xirugen cn fl al =
  let [(f1,p1);(f2,p2)] = if fl=[] then [("",(0.0,0.0));("",(0.0,0.0))] else fl in
  {name         = cn; 
   faults       = ["ued";"loa"]; 
   input_flows  = []; 
   input_metad  = []; 
   basic_events = [f1;f2];
   event_info   = [p1;p2];
   output_flows = ["o1"]; 
   output_metad = []; 
   formulas     = [(["o1"; "ued"],F[f1]);
                   (["o1"; "loa"],F[f2])];
   use_with     = [];
   generator    = "xriugen";} ;;

let xadcgen cn fl al =
  let [(f1,p1);(f2,p2)] = if fl=[] then [("",(0.0,0.0));("",(0.0,0.0))] else fl   in
  {name         = cn; 
   faults       = ["ued";"loa"]; 
   input_flows  = []; 
   input_metad  = []; 
   basic_events = [f1;f2];
   event_info   = [p1;p2];
   output_flows = ["o1"]; 
   output_metad = [];
   formulas     = [(["o1"; "ued"],F[f1]);
                   (["o1"; "loa"],F[f2])];
   use_with     = [];     
   generator    = "xadcgen";} ;;


(* 
   The C-generator for a DCM takes three arguments
   
   cn: the name of the component 
   fl: a list of the form [(f1,p1);(f2,p2)] where:
   - f1 is the name of the basic event corresponding to a ued failure
   - f2 is the name of the basic event corresponding to an loa failure
   - p1 is a pair consisting of the failure rate and exposure time for a ued failure
   - p2 is a pair consisting of the failure rate and exposure time for an loa failure
   al: is of the form [[a]] where a is a string corresponding to a positive integer.
   a corresponds to the number of inputs/outputs to the DCM.

   The DCM is pass-through. 
*)

let xdcmgen cn fl al =
  let [(f1,p1);(f2,p2)] = if fl=[] then [("",(0.0,0.0));("",(0.0,0.0))] else fl in
  let [[a1String]] = if al=[] then [["0"]] else al in
  let a1 = int_of_string a1String in
  let faults = ["ued";"loa"] in
  let iflows = cross [["i"]; num_to_list a1] in
  let oflows = cross [["o"]; num_to_list a1] in
  let forms_out = cprod oflows faults in
  let i_of_o x = "i" ^ String.sub x 1 ((String.length x) -1) in
  let f_of_flt x = if x="ued" then f1 else f2 in
  let forms = List.map ~f:(fun (o,flt) ->
    ([o;flt],Or[F[(i_of_o o);flt];F[(f_of_flt flt)]])) forms_out in
  {name         = cn; 
   faults       = ["ued";"loa"]; 
   input_flows  = iflows;  
   input_metad  = []; 
   basic_events = [f1;f2];
   event_info   = [p1;p2];
   output_flows = oflows;
   output_metad = []; 
   formulas     = forms;
   use_with     = [];         
   generator = "xdcmgen";} ;;

(* 
   The C-generator for a Switch takes three arguments
   
   cn: the name of the component 
   fl: a list of the form [(f1,p1);(f2,p2)] where:
   - f1 is the name of the basic event corresponding to a ued failure
   - f2 is the name of the basic event corresponding to an loa failure
   - p1 is a pair consisting of the failure rate and exposure time for a ued failure
   - p2 is a pair consisting of the failure rate and exposure time for an loa failure
   al: is of the form [[a1; ...; an]] where ai is a string corresponding to a positive integer.
   ai corresponds to the number of inputs/outputs from source i.

*)

let xswitchgen cn fl al =
  let [(f1,p1);(f2,p2)] = if fl=[] then [("",(0.0,0.0));("",(0.0,0.0))] else fl in
  let [al1] = if al=[] then [["0"]] else al in
  let al1_int = List.map ~f:(int_of_string) al1 in
  let faults = ["ued";"loa"] in
  let num_inputs = List.reduce_exn ~f:(+) al1_int in
  let iflows = cross [["i"]; num_to_list num_inputs] in
  let oflows = cross [["o"]; num_to_list num_inputs] in
  let forms_out = cprod oflows faults in
  let i_of_o x = "i" ^ String.sub x 1 ((String.length x) -1) in
  let f_of_flt x = if x="ued" then f1 else f2 in
  let forms = List.map ~f:(fun (o,flt) ->
    ([o;flt],Or[F[(i_of_o o);flt];F[(f_of_flt flt);flt]])) forms_out in
  {name         = cn; 
   faults       = faults; 
   input_flows  = iflows;  
   input_metad  = []; 
   basic_events = [f1;f2];
   event_info   = [p1;p2];
   output_flows = oflows;
   output_metad = []; 
   formulas     = forms;
   use_with     = [];     
   generator    = "xswitchgen"} ;;

let xswitchgen_crc cn fl al =
  let [(f1,p1);(f2,p2)] = if fl=[] then [("",(0.0,0.0));("",(0.0,0.0))] else fl in
  let [al1] = if al=[] then [["0"]] else al in 
  let al1_int = List.map ~f:(int_of_string) al1 in
  let faults = ["ued";"loa"] in
  let num_inputs = List.reduce_exn ~f:(+) al1_int in
  let iflows = cross [["i"]; num_to_list num_inputs] in
  let oflows = cross [["o"]; num_to_list num_inputs] in
  let forms_out = cprod oflows faults in
  let i_of_o x = "i" ^ String.sub x 1 ((String.length x) -1) in
  let f_of_flt x = if x="ued" then f1 else f2 in
  let forms = List.map ~f:(fun (o,flt) ->
    if flt="ued" 
    then ( ([o;flt],Or[F[(i_of_o o);flt];And[F[(f_of_flt flt)]; F[("crc32_flt")]]] ) ) 
    else ( ([o;flt],Or[F[(i_of_o o);flt];F[(f_of_flt flt)]]) ) )
    forms_out in
  {name         = cn; 
   faults       = faults; 
   input_flows  = iflows;  
   input_metad  = []; 
   basic_events = [f1;f2;"crc32_flt"];
   event_info   = [p1;p2;(2.**(-32.),1.0)];
   output_flows = oflows;
   output_metad = []; 
   formulas     = forms;	 
   use_with     = [];         
   generator    = "xswitchgen"} ;;

(* 
   The C-generator for an FMA App takes three arguments
   
   cn: the name of the component 
   fl: a list of the form [(f1,p1);(f2,p2)] where:
   - f1 is the name of the basic event corresponding to a ued failure
   - f2 is the name of the basic event corresponding to an loa failure
   - p1 is a pair consisting of the failure rate and exposure time for a ued failure
   - p2 is a pair consisting of the failure rate and exposure time for an loa failure
   al: is of the form [[a1; ...; an] [b1; ...; bn] [c1; ...; cn] [d1; ...; dn]] 
   where ai, bi, ci are strings corresponding to positive integers and di are booleans.
   ai corresponds to number of replicas of source i and
   bi corresponds to the number of channels for each replica of source i.
   ci corresponds to the number of N_of votes of ai (e.g., N_of(2, [sen1; sen2; sen3]),
      where ci=2, ai=3.
   di corresponds to channel voting option of ai (e.g., [true; false] means channel vote
      the a1 sensors, don't do channel voting on the a2 sensors.
*)

let xfmagen cn fl al =
  let [(f1,p1);(f2,p2)] = if fl=[] then [("",(0.0,0.0));("",(0.0,0.0))] else fl in
  let faults = ["ued"; "loa"] in
  let [s;k;sv;cv] = if al=[] then [["0"];["0"];["0"];["0"];] else al in
  let s_int = List.map ~f:(int_of_string) s in
  let k_int = List.map ~f:(int_of_string) k in
  let sv_int = List.map ~f:(int_of_string) sv in
  let num_inputs = List.reduce_exn ~f:(+)
    (List.map2_exn ~f:( * ) s_int k_int) in
  let input_flows = cross [["i"]; num_to_list num_inputs] in
  let num_sources = num_to_list (List.length s) in
  let replica_channel_pairs = List.map2_exn ~f:(fun x y ->
    clistprod (num_to_list x) (num_to_list y))
    s_int k_int in
  let source_replica_channel_list = List.concat
    (List.map2_exn ~f:(fun x y ->
      List.map ~f:(fun z -> List.cons x z) y)
       num_sources replica_channel_pairs) in
  let input_metad = List.zip_exn input_flows source_replica_channel_list in
  let output_flows = ["o1"] in
  let output_metad = List.map ~f:(fun x -> (x, ["0";"0"; (String.suffix cn 1)])) output_flows in
  
  (* Heber's code 10/9/2017 *)
  let list_ele a l  = List.map l ~f:(fun k->List.append [a] [k]) 
  and list_ele1 a l  = List.map l ~f:(fun k->List.append [a] k) 
  and mklist a = List.range 1 a ~stop:`inclusive in
  let mklist_s_k i s k = List.map ~f:(fun x-> list_ele1 i x) (List.map (mklist s) ~f:(fun x->list_ele x (mklist k))) in  
  let mList ls lk = List.concat (List.map (s_k_list1 (mklist (List.length ls)) ls lk) ~f:(fun k -> let [a;b;c] = k in [mklist_s_k a b c])) 
  (* mList: given s and k gives a list of sub-lists, where the sub-list has list elements of length 3 and
  the first 2 components are the same and the last element has all the possible values of k  *)
  and string_afy l = List.map ~f:(fun l -> List.map ~f:(fun l -> List.map ~f:(fun l -> List.map ~f:(fun il -> string_of_int il) l) l) l) l in
  let innerts_meta = string_afy (mList (stringlist_2_intlist s) (stringlist_2_intlist k))
  (* innerts_meta:  is a mList where its inputs are string list and the output is a list of sublist of strings *)
  and replace_mdata x = List.map ~f:(fun z -> 
    let (c, _) = List.find_exn
      ~f:(fun (a,b) -> List.equal String.equal b z ) input_metad in c)
    x in
  let innerts = List.map ~f:(fun l -> (List.map ~f:(replace_mdata) l)) innerts_meta in
  let pairs l1 l2 = List.map2_exn ~f:(fun x y -> List.cons x [y]) l1 l2 in
  let pairs1 l1 l2 = List.map2_exn ~f:(fun x y -> List.cons x y ) l1 l2 in
  let mList_voting s k sc sv = List.map2_exn ~f:(fun x y-> (x,y) ) (pairs sc sv) innerts in
  (* mList_voting for UED: tuple of list of sc  and sv with masterList*)
  let mList_voting1 s k sc sv = List.map2_exn ~f:(fun x y-> (x,y) ) (pairs1 s (pairs sc sv)) innerts in
  (* mList_voting1 for LOA: tuple of list of s, sc  and sv with masterList*)
  let str2Fued slist = List.map ~f:(fun x -> F[(x);"ued"]) slist in
  let str2Floa slist = List.map ~f:(fun x -> F[(x);"loa"]) slist in
  let fm_ued sv_cv_l = 
    let ([sv;cv],sll) = sv_cv_l in  
      let l = List.map ~f:(fun x -> str2Fued x) sll in N_of ( (int_of_string sv), List.map l ~f:(fun k-> if cv = "true" then (And k) else (Or k)))
  and fm_loa sv_cv_l = 
    let ([s;sv;cv],sll) = sv_cv_l in  
      let l = List.map ~f:(fun x -> str2Floa x) sll in N_of ( ((int_of_string s - int_of_string sv) + 1), List.map l ~f:(fun k-> if cv = "true" then (Or k) else (And k))) in
  let fm_ued_Voting [s; v; sc; sv] = List.map (mList_voting (stringlist_2_intlist s) (stringlist_2_intlist k) sc sv) ~f:(fun k-> fm_ued k) 
  and fm_loa_Voting [s; v; sc; sv] = List.map (mList_voting1 s k sc sv) ~f:(fun k-> fm_loa k) in
  let ued_formula_core = fm_ued_Voting [s; k; sv; cv] 
  and loa_formula_core = fm_loa_Voting [s; k; sv; cv] in
  let ued_formula = (["o1"; "ued"],Or(F[(f1)]::ued_formula_core)) in
  let loa_formula = (["o1"; "loa"],Or(F[(f2)]::loa_formula_core)) in
  {name = cn;
   faults = faults;
   input_flows = input_flows;
   input_metad = input_metad;
   basic_events = [f1;f2];
   event_info   = [p1;p2];
   output_flows = output_flows;
   output_metad = output_metad;
   formulas     = [ued_formula; loa_formula];
   use_with     = [xirugen; xadcgen]; 
   generator    = "xfmagen"} ;;

(* ----------------------------------------- *)
(* === Big-C helper functions === *)
(* ----------------------------------------- *)

(*
"Big-C" helper functions 
   The following are argument list generators for each Big-C function.
   In order to use these helper functions, they all have to take the same types 
   and return the same types.
   Below is from Heber, 8/7/2017
   Below is from Heber, updated 10/3/2017 
*)


(* Helper function for DCMs *)
(* KS 10/10/17 - modified to make individual DCMs for each IRU 
   This still needs to be fixed, because IRU is assumed to be sensor #2. 
let xdcmgen_al s k a sv cv =[([a] ,f (List.nth ( dcm s k) 0))];; *)
(* let xdcmgen_al n s k a sv cv = List.map ~f:(fun x -> ([a], [x])) (dcm_single s k);; *)
(* add an element a to each list of a list*)
  let add_ele a l = List.map l ~f:(fun k -> List.append [a] k);;
  (*add_ele 2 [[1;1;1];[1;2];[2;1];[3]];;*)
  
  let genNstep n gen_dcm = List.concat (List.map (List.range 1 n  ~stop:`inclusive) ~f:(fun k -> add_ele k (gen_dcm (n - k))));; 
 (* generate combinations of all the numbers that add a number*)
 let rec gen_dcm n =
  match n with
   | 0 -> [[]]  
   | 1 -> [[1]]
   | 2 -> List.append (add_ele 1 (gen_dcm 1)) (add_ele 2 (gen_dcm 0))
   | 3 -> List.append (List.append (add_ele 1 (gen_dcm 2)) (add_ele 2 (gen_dcm 1)) ) (add_ele 3 (gen_dcm 0)) 
   | n -> genNstep n gen_dcm;;  
  
 let filterlessThanN l n = List.filter l ~f:(fun k->List.length k <= n);;
 let intListToListstring l = List.map l ~f:(fun k-> List.map k ~f:(fun a -> [[string_of_int a]]));; 
 (*gen_dcmBounded generate combination of all the  numbers lists that add number of inputs and these number lists have length less or
 equal to ncomp*)
 let gen_dcmBounded ninp ncomp = intListToListstring (filterlessThanN (gen_dcm ninp) ncomp)  ;;
 (* gen_dcmBounded 10 3;;*)
(*  xdcmgen_al generate combinations of all the DCM's that add an specific number of inputs
xdcmgen_al has 2 inputs one is the number of inputs to DCM  and the other the number of DCM   *)

let xdcmgen_al n = gen_dcmBounded n 3;;

(* a dummy helper function. This is going to phase out *)
let xdummygen_al n = [ [ [[]] ] ];;

(* ----------------------------------------- *)
(* ====== from END-USER ====== *)
(* ----------------------------------------- *)

let user_probTargets = 
  [
   {target = "FMA"; prob = ("loa", 1e-07)};
   {target = "FMA"; prob = ("ued", 1e-07)};
   (* {target = "A664sw"; prob = ("ued", 1e-07)} *)
  ];;
 
let usables = 
  [
    ("FMA", xfmagen, xdummygen_al, (1,1), true, [("uedflt",(2.0e-10,1.0)); ("loaflt",(3.0e-5,1.0))]);
    ("A664sw", xswitchgen_crc, xdummygen_al, (1,2), true, [("uedflt",(1.0e-6,1.0)); ("loaflt",(1.0e-5,1.0))]);
    ("DCM", xdcmgen, xdcmgen_al, (2,2), true, [("uedflt",(1.0e-7,1.0)); ("loaflt",(1.0e-5,1.0))]); 
    ("ADC", xadcgen, xdummygen_al, (1,1), true, [("uedflt",(2.0e-8,1.0)); ("loaflt",(2.0e-5,1.0))]);
    ("IRU", xirugen, xdummygen_al, (2,2), true, [("uedflt",(1.0e-6,1.0)); ("loaflt",(1.0e-5,1.0))]);
  ];; 

let user_connectables = 
  [
    {connectable = "FMA";
     connectable_inputs = ["A664sw"];    
     connectable_outputs = ["A664sw"]};
    {connectable = "A664sw";
     connectable_inputs = ["ADC"; "DCM"];
     connectable_outputs = ["FMA"]};
    {connectable = "ADC";
     connectable_inputs = [];
     connectable_outputs = ["A664sw"]};
    {connectable = "DCM";
     connectable_inputs = ["IRU"];
     connectable_outputs = ["FMA"]}; 
    {connectable = "IRU";
     connectable_inputs = [];
     connectable_outputs = ["DCM"]};
  ];; 

(* ----------------------------------------- *)
(* === Run Validation Checks === *)
(* ----------------------------------------- *)

(*B1*)
checkNamesUnique usables;;
(*B2*)
checkMinMax usables;;
(*B3*)
checkConnectNames usables user_connectables;;

(* ----------------------------------------- *)
(* === user call to synthesis algorithm  === *)
(* ----------------------------------------- *)

(* create blank hash tables to be filled with solutions *)
let myh_sol_autolib = Hashtbl.Poly.create ();;
let myh_sol_autoxlib = Hashtbl.Poly.create ();;
let myh_sol_automdl = Hashtbl.Poly.create ();;
let myh_sol_prob = Hashtbl.Poly.create ();;

syn_arch usables user_connectables user_probTargets myh_sol_autolib myh_sol_autoxlib myh_sol_automdl myh_sol_prob;;

(* --- some post data analysis --- *)
let arch_res_h = Hashtbl.Poly.create ();;
syn_results user_probTargets arch_res_h myh_sol_prob 0;;


let arch_num = 1571 in
dot_gen_show_funct_res myh_sol_autolib myh_sol_automdl arch_res_h arch_num ("navpos_"^string_of_int arch_num ^".gv");;
