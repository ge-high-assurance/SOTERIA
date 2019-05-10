(* 

Copyright © 2019 United States Government as represented by the Administrator of the 
National Aeronautics and Space Administration.  All Rights Reserved.

Author: Kit Siu
Date: 2018-01-22

*)

open Core ;;
open FaultTree ;;
open Qualitative ;;
open Quantitative ;;
open Modeling ;;
open FaultTreeSynthesis ;;


(* ----------------------------------------- *)
(* ===== synthesis algorithm TYPES     ===== *)
(* ----------------------------------------- *)

(* LIBRARY-OWNER TYPES *)
(* modified component type, for now *)

type xcomponent = 
    {name         : string;
     faults       : string list;
     input_flows  : string list;
     input_metad  : (string * string list) list; 
     basic_events : string list;
     event_info   : (float * float) list;
     output_flows : string list; 
     output_metad : (string * string list) list;
     formulas     : (string list) cformula list;
     use_with     : (string -> (string * (float * float)) list -> string list list -> xcomponent) list; 
     generator    : string;
    };;

(* END-USER TYPES *)
type xprobTargets =
    {target : string; 
     prob   : (string * float);
    };;

type xconnectables =
    {connectable        : string; 
     connectable_inputs : string list; 
     connectable_outputs: string list;
    };;

type parameters = 
    {n_order   : string list;
     s_order   : string list;
     k_order   : string list;
     a_ranges  : string list;
     sv_ranges : (int * int) list;
     cv_ranges : bool list;
    };;

(* ----------------------------------------- *)
(* == utility for "Big-C" helper functions== *)
(* ----------------------------------------- *)

(* "Big-C" helper functions 
   The following are argument list generators for each Big-C function.
   In order to use these helper functions, they all have to take the same types and return the same types.
   Below is from Heber, 8/7/2017
   Below is from Heber, updated 10/3/2017
*)
let list_ele a l  = List.map l ~f:(fun k->List.append [a] [k]);;
let list_list l1 l = List.concat (List.map l1 ~f:(fun k-> list_ele k l));; 

let list_ele1 a l  = List.map l ~f:(fun k->List.append [a] k);;
(* list_ele1 7 (list_list [1;2;3] [4;5;6]);; *)
let ll l1 l = List.concat (List.map l1 ~f:(fun k-> list_ele1 k l));; 
 let f x =
 match x with
 Some x ->x
 | None ->[];;
 
let ele_tuple a  =
 let (a1,a2) = a in (List.range a1 a2   ~stop:`inclusive);; 
 (* Given a list of tuples the function tuple_list
    returns a list of list of integers
    tuple_list [(1,2);(2,4);(5,9)];;   
 *)
let tuple_list l = List.map l ~f:(fun k-> ele_tuple k);;
(* Given a list of list of integers the function
  combination returns a  combination of the list of integers 
  combination [[1;2;3];[4;5;6]];;
*)
 let rec combination l =
 match List.length l with
 0->[]
 |1->List.map(f (List.hd l)) ~f:(fun k-> [k]) 
 |2->list_list (f (List.hd l)) (f(List.hd (f (List.tl l))))
 |_ -> ll (f (List.hd l)) (combination (f (List.tl l)));;
 
let list_ele2 a l = List.map l ~f:(fun k-> List.append [a] [k] );; 
 let ll2 l1 l = List.concat (List.map l1 ~f:(fun k-> list_ele2 k l));; 
(*
given source, channel list parameter the function parameterConstraints
gives a list, where its element is a list of possibles source and channel values 
parameterConstraints [(1,2);(2,3)] [(1,2);(3,3)];;
parameterConstraints [(1,2)] [(1,4)];;
*) 
let parameterConstraints s k  = ll2 (combination (tuple_list s)) (combination (tuple_list k));;
(* listGreater function return true when all the entries of l1 is >= to entries of l2 
*)
let listGreater l1 l2 = List.fold ~init:true  ~f:(&&) (List.map2_exn ~f:(fun x y -> x>=y) l1 l2);;
(*filter the list where the sources values are less that voting sources values *)
let filter_sv sk s_voting = let [l1;_] = sk in List.filter (combination(tuple_list s_voting)) ~f:(fun l2 ->(listGreater l1 l2));;
let filter_s_sv sk sv = List.map (filter_sv sk sv) ~f:(fun k-> List.append sk [k]);;
(* helper for permutation function*)
let interleave x lst = 
  match lst with
  | [] -> if x = true then [[true];[false]] else [[false]] 
  | _::_ -> if x = true then (List.append [x] lst )::[List.append [false] lst ] else List.append [false] lst  ::[] ;;
(*
given a list of booleans values the permutation function
returns a list of list booleans values
*)
let rec permutations lst = 
  match  lst with
  | hd::tl -> List.concat (List.map ~f:(interleave hd) (permutations tl))
  | _ -> [lst];;
(**)
let listBool_to_string l = List.map l ~f:(fun x-> List.map x ~f:(fun k-> string_of_bool k));; 
let listInt_to_string l = List.map l ~f:(fun x -> List.map x ~f:(fun y-> List.map y ~f:(fun k-> string_of_int k)) );;

(*
listBool_to_string (permutations [false;true]);;
permutations [false;true;false;true];;
*) 

let combineL l1 l2 = List.map l1 ~f:(fun k -> (List.map l2 ~f:(fun y -> List.append k [y])));;

(*combineL (filter_sv [[2;2];[1;1]] [(1,2);(1,3)]) (filter_sv [[2;2];[1;1]] [(1,2);(1,3)]);;*)
  
(* Given source, channel and source voting  list parameters
  the function parameterConstraints_sv
  gives a list, where an element of this list is a list of posibles source
  channel and source voting  values. The source voting values are less or equal to source values 
  parameterConstraints_sv [(1,2); (5,5)] [(1,2);(1,1)] [(1,2);(5,6)];;
*)  
let parameterConstraints_sv s k sv = List.concat (List.map (parameterConstraints s k) ~f:(fun x->filter_s_sv x sv));; 

 (* 
 List.concat (combineL (listInt_to_string (parameterConstraints_sv [(1,3); (1,1)] [(1,1);(1,2)] [(1,2);(1,2)])) 
                       (listBool_to_string (permutations [false;true])));;
*)
(* Given source, channel, voting source and voting channel list parameters
 the function parameterConstraintsAll
 gives a list, where an element of this list is a list of
 posibles source, channel, voting source and voting channel values 
*)  
let parameterConstraintsAll s k sv cv = List.concat (combineL (listInt_to_string (parameterConstraints_sv s k sv)) 
                                                               (listBool_to_string (permutations cv)));;

(* -- regression test -- 
parameterConstraintsAll  [(1,2); (1,1)] [(1,2);(1,1)] [(1,3);(1,1)] [false;false];;
parameterConstraintsAll  [(1,3);(1,1);(1,2)] [(1,1);(1,2);(1,2)] [(1,2);(1,2);(1,2)] [true;false;true];;
parameterConstraintsAll  [(1,3);(1,1);(1,1);(1,1)] [(1,1);(1,2);(1,2);(1,1)] [(1,2);(1,2);(1,2);(1,3)] [false;false;false;false];;
 -- end of regress -- *)

let parameterConstraintsAllRedundancy n s k sv cv = 
        List.concat (List.map (parameterConstraintsAll s  k sv cv ) 
                    ~f:(fun k -> (List.map (ele_tuple n) ~f:(fun n-> List.append [[(string_of_int n)]] k) )));; 

(* -- regression test -- 
parameterConstraintsAllRedundancy (1,1) [(1,2); (1,1)] [(1,2);(1,1)] [(1,3);(1,1)] [false;false];;
parameterConstraintsAllRedundancy (1,1) [(1,1);(1,1);(1,1)] [(1,1);(1,3);(1,1)] [(1,2);(1,2);(1,2)] [false;false;false];;                  
parameterConstraintsAllRedundancy (2,2) [(2,2)] [(2,2)] [(1,2)] [true];;
 -- end of regress -- *)

(* from Heber 10/9/2017*)
let rec s_k_list1 s ls lk =
  match s, ls ,lk with
    | [],[],_ -> []
    | _,[],[]-> []
    | [],_,[]->[] 
    | hs::tls, hls::tlls, hlk::tllk ->
      List.append
	([List.append [hs] (List.append [hls] [hlk])])
	(s_k_list1 tls tlls tllk)  ;;

let mklist a = List.range 1 a   ~stop:`inclusive;;  

let mklist_s_k i s k =
  List.map
    (List.concat
       (List.map  (mklist s) ~f:(fun x->list_ele x (mklist k))))
    ~f:(fun x->List.append [i] x);; 
    
(* KS - modified this function so that ls and lk are both lists of strings *)
let mList ls lk =
  let i_ls = List.map ~f:(fun x -> int_of_string x) ls
  and i_lk = List.map ~f:(fun x -> int_of_string x) lk in
  List.concat
    (List.map (s_k_list1 (mklist (List.length ls)) i_ls i_lk)
       ~f:(fun k -> let [a;b;c] = k in mklist_s_k a b c));;

let pred1 i [a;_;_]=  [a] = [i] ;; 
let pred3 i [_;_;c]=  [c] = [i] ;; 
let proj pred i mlist = List.filter mlist ~f:(fun x-> (pred i x));;    

let rec last l = 
  match l with 
    | x::[] -> x
    | _::xs -> last xs
    | []    -> failwith "no element";;

let first l = match l with []-> failwith "no element" | x::[]->x | x::_ -> x;;

let my_max = function
[] -> invalid_arg "empty list"
  | x::xs -> List.fold_left ~f:(Pervasives.max) ~init:x xs;;

let ele_list f l = List.map l ~f:(fun x-> f x) ;; 

let my_max_comp nth s k = my_max (ele_list nth (mList s k)) 

let list_of_comp nth s k pred =
  List.map (mklist ( my_max_comp nth s k))
    ~f:(fun x-> (proj pred x (mList s k)));;

let from_3_2_list l = List.map l ~f:(fun x-> let [a;b;_]= x in [a;b]);;

let from_3_2_list_of_list l = List.map l
  ~f:(fun x-> removeDups (from_3_2_list x));;

let adc s k =
  f(List.nth  (from_3_2_list_of_list (list_of_comp first s k pred1)) 0);; 

let dcm s k =
  f(List.tl  (from_3_2_list_of_list (list_of_comp first s k pred1)) );; 

(* KS 10/10/17 - added this to make individual DCMs for each IRU 
   This still needs to be fixed, because IRU is assumed to be sensor #2. *)
let dcm_single s k =
  f(List.nth  (from_3_2_list_of_list (list_of_comp first s k pred1)) 1);;
  
let dd l =
  List.map (mklist (my_max (ele_list first l))) ~f:(fun x->(proj pred1 x l));;

(* KS - modified this function so that it returns a string list *)
let length_lists l = List.map (dd l) ~f:(fun x-> string_of_int (List.length x));;

(* ----------------------------------------- *)
(* == utility functions for SYN ALGORITHM == *)
(* ----------------------------------------- *)

(* make a hash table to look up "cn" -> Big-C function
   this is for a usables list where cn are in a list, cnl *)
let rec generate_cnl2funHash uL h =
  match uL with
    | [] -> h
    | hd::tl -> let (cnl,f,fal,(mn,mx),mu,fl) = hd in
                match cnl with
                  | [] -> generate_cnl2funHash tl h
                  | chd::ctl -> Hashtbl.set h ~key:chd ~data:(f,fal,(mn,mx));
                    generate_cnl2funHash (List.append [(ctl,f,fal,(mn,mx),mu,fl)] tl) h;;

(* make a hash table to look up "cn" -> Big-C function *)
let rec generate_cn2funHash uL h =
  match uL with
    | [] -> h
    | hd::tl -> let (cn,f,fal,(mn,mx),mu,fl) = hd in
                Hashtbl.set h ~key:cn ~data:(f,fal,(mn,mx),mu,fl);
                generate_cn2funHash tl h;;

(* make a hash table to look up Big-C function -> "cn" *)
let rec generate_fun2cnHash uL h =
  match uL with
    | [] -> h
    | hd::tl -> let (cn,f,_,(_,_),_,_) = hd in
                Hashtbl.set h ~key:f ~data:cn;
                generate_fun2cnHash tl h;;

(* make a hash table to look up "cn" -> connectables "cn" *)
let rec generate_connectableHash uCl h =
  match uCl with
    | [] -> h
    | hd::tl -> 
      let k = hd.connectable 
      and dl = hd.connectable_inputs in
      Hashtbl.set h ~key:k ~data:dl;
      generate_connectableHash tl h;;

(* function to identify sub-architectures from a list of usables *)
(* Sub-architectures are defined as App -> SW -> ... -> source. This function probes 
   each Big-C generators to see if they have anything in the "usewith" field of their components.
   "usewith" tells the algorithm that this is the source to use for this Big-C
   generator, therefore it is identified as the head of a sub-architecture.
*)
let rec id_subarch usables =
  match usables with
  | [] -> []
  | hd::tl -> let (cn,g,_,_,_,_) = hd in
    let comp = g cn [] [] in
    if comp.use_with = [] then id_subarch tl
    else hd::(id_subarch tl)
;;


(* -------------------------------------------- *)
(*  == utility functions for the LIBRARY    === *)
(* -------------------------------------------- *)

(* function to generate the n, s, k arguments for parameterConstraints *)
let rec generate_i myhf i_order =
  match i_order with
  | [] -> []
  | hd::tl -> 
    let (_,_, i, _, _) = Hashtbl.find_exn myhf hd in
    i::generate_i myhf tl;;

(* turn list of ints into list of strings *)
let rec intLL_2_strLL intL =
  match intL with
    | [] -> []
    | hd::tl -> (List.map hd ~f:(fun x -> string_of_int x))::(intLL_2_strLL tl);;

(* make a hash table to look up ("cn", [source; replica; channel]) -> flow number 
   for example, ("FMA", ["1"; "1"; "1"]) -> "i1" *)
let rec generate_src2flowHash cn l h = 
  match l with 
    | [] -> h
    | hd::tl -> 
      let (flow, src) = hd in
      let k = (cn, src) 
      and d = (cn, flow) in
      Hashtbl.set h ~key:k ~data:d;
      generate_src2flowHash cn tl h;;

(* make a hash table to look up ("cn", flow number) -> [source; replica; channel] 
   for example, ("FMA", "i1") -> ["1"; "1"; "1"] *)
let rec generate_flow2srcHash cn l h = 
  match l with 
    | [] -> h
    | hd::tl -> 
      let (flow, src) = hd in
      let k = (cn, flow) 
      and d = (cn, src) in
      Hashtbl.set h ~key:k ~data:d;
      generate_flow2srcHash cn tl h;;

(* function to generate the generator list from cn name list *)
let rec get_g cnL hf = 
  match cnL with
  | []->[]
  | hd::tl -> let (g,_,_,_,_) = Hashtbl.find_exn hf hd in
      g::(get_g tl hf);;

(* function to generate the MinMax list from cn name list *)
let rec get_MinMax cnL hf = 
  match cnL with
  | []->[]
  | hd::tl -> let (_,_,minmax,_,_) = Hashtbl.find_exn hf hd in
      minmax::(get_MinMax tl hf);;

(* function to generate the fault list list from cn name list *)
let rec get_fl cnL hf = 
  match cnL with
  | []->[]
  | hd::tl -> let (_,_,_,_,fl) = Hashtbl.find_exn hf hd in
      fl::(get_fl tl hf);;
      
(* function that repeats a list n times *) 
let rec duplicate_ntimes l n =
  match n with
  | 1 -> l
  | _ -> duplicate_ntimes (List.append l l) (n-1);;
  
(* function to make the range from 1 to as many times there are sources *)
let rec make_svMinMax minmaxL =
  match minmaxL with
  | [] -> []
  | hd::tl -> let (_,mx) = hd in
    (1,mx)::make_svMinMax tl;;

(* function to replace the source of the meta-data with meaningful c-name *)
let rec replace_metad_with_cn metadL sMap =
  match metadL with
  | [] -> []
  | hd::tl -> let (nm, md) = hd in 
    let i = List.hd_exn md in 
    let cn = Map.find_exn sMap (int_of_string i) in
    (nm, cn::List.tl_exn md)::replace_metad_with_cn tl sMap ;;

(* function that generates n apps w/output_metad *)
let rec generate_apps g cn fl al smap n =
  let r = int_of_string (List.hd_exn n) in
  match r with
  | 0 -> []
  | _ ->
    let ctemp = (g (cn^(string_of_int r)) fl al) in
      {name         = ctemp.name; 
       faults       = ctemp.faults;
       input_flows  = ctemp.input_flows;
       input_metad  = replace_metad_with_cn ctemp.input_metad smap;
       basic_events = ctemp.basic_events;
       event_info   = ctemp.event_info;
       output_flows = ctemp.output_flows;
       output_metad = List.zip_exn ctemp.output_flows [[cn;(string_of_int r)]];
       formulas     = ctemp.formulas;
       use_with     = ctemp.use_with;
       generator    = ctemp.generator;}  :: (generate_apps g cn fl al smap ([string_of_int (r-1)])) ;;

(* function that takes a meta-data list and parses out only the unique source-replicas *)
let rec keepUnique_sr seenL mdL =
  match mdL with
  | []->[]
  | hd::tl -> 
    let (_,[s;r;_]) = hd in
    if (List.exists ~f:(fun x -> let (_,[xs;xr;_])=x in (s=xs && r=xr)) seenL) 
    then (keepUnique_sr seenL tl)
    else hd::(keepUnique_sr (hd::seenL) tl);;

(* function that uses the app meta-data sr list to generate the sources w/output_metad *)
let rec generate_sources myhf mdL =
  match mdL with
  | []->[]
  | hd::tl ->
    let (_,[cn;r;_]) = hd in
    let (gen,_,_,_,fl) = Hashtbl.find_exn myhf cn in
    let ctemp = (gen (cn^r) fl [[]]) in
      {name         = ctemp.name; 
       faults       = ctemp.faults;
       input_flows  = ctemp.input_flows;
       input_metad  = ctemp.input_metad;
       basic_events = ctemp.basic_events;
       event_info   = ctemp.event_info;
       output_flows = ctemp.output_flows;
       output_metad = List.zip_exn ctemp.output_flows [[cn;r]];
       formulas     = ctemp.formulas;
       use_with     = ctemp.use_with;
       generator    = ctemp.generator;}  :: (generate_sources myhf tl) ;;

(* function that takes a meta-data list and splits it out into lists by channels *)
let rec split_k mdL mdL_s ik =
  match mdL with
  | [] -> mdL_s
  | _::_ -> 
    let keep = List.filter ~f:(fun x -> let (_,[_;_;k]) = x in (int_of_string k)=ik) mdL 
    and pass = List.filter ~f:(fun x -> let (_,[_;_;k]) = x in (int_of_string k)<>ik) mdL in
    split_k pass (List.append [keep] mdL_s) (ik+1);;

(* function that uses the app meta-data list to generate the switches w/output_metad *)
let rec generate_switches myhf cn mdLL =
  match mdLL with
  | []->[]
  | hd::tl ->
    let (_,[_;_;k]) = (List.hd_exn hd) in
    let (gen,_,_,_,fl) = Hashtbl.find_exn myhf cn in
    let (_,keep) = List.unzip hd in
    let ctemp = (gen (cn^k) fl [[string_of_int (List.length hd)]]) in
      {name         = ctemp.name; 
       faults       = ctemp.faults;
       input_flows  = ctemp.input_flows;
       input_metad  = List.zip_exn ctemp.input_flows keep;
       basic_events = ctemp.basic_events;
       event_info   = ctemp.event_info;
       output_flows = ctemp.output_flows;
       output_metad = List.zip_exn ctemp.output_flows keep;
       formulas     = ctemp.formulas;
       use_with     = ctemp.use_with;
       generator    = ctemp.generator;}  :: (generate_switches myhf cn tl) ;;

(* this generates a hash table from the auto generated library to go from input src->flow *)
let rec generate_src2flowHash_input lib h =
  match lib with
    | []->h
    | hd::tl ->  
      generate_src2flowHash_input tl (generate_src2flowHash hd.name hd.input_metad h);;

(* this generates a hash table from the auto generated library to go from output src->flow *)
let rec generate_src2flowHash_output lib h =
  match lib with
    | []->h
    | hd::tl -> 
      generate_src2flowHash_output tl (generate_src2flowHash hd.name hd.output_metad h);;

(* this generates a hash table from the auto generated library to go from input flow->src *)
let rec generate_flow2srcHash_input lib h =
  match lib with
    | []->h
    | hd::tl -> 
      generate_flow2srcHash_input tl (generate_flow2srcHash hd.name hd.input_metad h);;

(* this generates a hash table from the auto generated library to go from output flow->src  *)
let rec generate_flow2srcHash_output lib h =
  match lib with
    | []->h
    | hd::tl -> 
      generate_flow2srcHash_output tl (generate_flow2srcHash hd.name hd.output_metad h);;

(* a function to turn xcomponent into component so we can use our legacy fault-tree functions *)
let rec xcomponent2component lL =
  match lL with
    |[]->[]
    |hd::tl -> List.append
      [{name         = hd.name;
        faults       = hd.faults;
        input_flows  = hd.input_flows;
        basic_events = hd.basic_events;
        event_info   = hd.event_info;
        output_flows = hd.output_flows;
        formulas     = hd.formulas; } ]
      (xcomponent2component tl) ;;

(* function to form a list of numArchs list *)
let rec generate_archParamListList subarchL hf hc hf2cn =
  match subarchL with
  | [] -> []
  | hd::tl ->
    (* -- application -- *)
    let (app_cn, app_g, _, nMinMax, _, _) = hd in

    (* -- sources -- *)
    (* probe the app generator to see what sources must be used *)
    let source_gL = (app_g app_cn [] []).use_with in 
    let source_cnL = List.map ~f:(fun x -> Hashtbl.find_exn hf2cn x) source_gL 
    and source_icnL = List.mapi ~f:(fun i x -> let cn = Hashtbl.find_exn hf2cn x in (i+1, cn)) source_gL in
    let source_map = Map.of_alist_exn (module Int) source_icnL in
    let sMinMax = get_MinMax source_cnL hf in

    (* -- switches -- *)
    (* connection tells which switch is connected to the app *)
    let switch_cnL = Hashtbl.find_exn hc app_cn in
    let kMinMax = duplicate_ntimes (get_MinMax switch_cnL hf) (List.length sMinMax) in

    (* -- sources vote, sv -- *)
    let svMinMax = make_svMinMax sMinMax in

    (* -- channel vote, cv -- *)
    (* make the vote true, which will also give the false option when the combos are generated *)
    let cvMinMax = List.map ~f:(fun x -> true) kMinMax in

    (* -- sub-architecture list -- *)
    (* -- STEP 1 -- generate the n,s,k lists to iterate over *)
    let subarchList = parameterConstraintsAllRedundancy nMinMax sMinMax kMinMax svMinMax cvMinMax in
    List.map ~f:(fun x -> (source_map, x)) subarchList :: generate_archParamListList tl hf hc hf2cn;;

(* function to iterate through the sublists *)
let rec distribute ll1 l2 =
  match ll1 with
  | []->[]
  | hd::tl -> List.append (List.map ~f:(fun x -> List.append hd [x]) l2) (distribute tl l2);;
  
let generate_archParamList lL =
  let kernal = List.map ~f:(fun x -> [x]) (List.hd_exn lL) in
  List.fold_left ~init:kernal ~f:(distribute) (List.tl_exn lL);;

(* the following functions, does_n_match_s and keep_only_valid_archParams are to post 
   process the list of archParamLists. The list of archParam is only valid if 
   the n of the app matches the s of the subarch list following it, because the app
   becomes the source in the next sub-architecture. *)
let rec does_n_match_s aPL =
 match aPL with
 | [] -> true
 | _::[] -> true
 | hd::tl -> (*hd && (List.hd_exn tl) && does_n_match_s tl;;*)
   let (_, hL)  = hd 
   and (_, nL) = List.hd_exn tl in
   (List.hd_exn hL = List.nth_exn nL 1) && does_n_match_s tl;;
   
let rec keep_only_valid_archParams aPLL =
 match aPLL with
 | [] -> []
 | hd::tl -> 
   if (does_n_match_s hd) then hd :: keep_only_valid_archParams tl
   else keep_only_valid_archParams tl;;
 
(* recursive function that takes the subarch list and the subarch parameter list 
   to generate an auto library (xcomponent list) that includes components from all the subarchs *)
let rec generate_autoLibrary libL subarchL subarch_pL hf hc= 
  match subarchL with
  | [] -> libL
  | hd::tl -> 
    let (source_map, [n; s; k; sv; cv]) = List.hd_exn subarch_pL 
    and (app_cn, app_g, _, _, _, app_fl) = hd in
    (* ---- apps lib comp ---- *)
    let auto_apps = generate_apps app_g app_cn app_fl [s;k;sv;cv] source_map n in
    (* ---- sources lib comp ---- *)
    let sr_metadL = keepUnique_sr [] (List.hd_exn auto_apps).input_metad in
    (* see if the sources were already generated *)
    let (_,[cn;_;_]) = List.hd_exn sr_metadL in
    let auto_sources =
    (* if already in the library, then don't generate it again *)
    if ( List.exists ~f:(fun x -> String.is_prefix ~prefix:cn x.name) libL )
      then [] else generate_sources hf sr_metadL in
    (* ---- switches lib comp ---- *)
    let switch_cnL = Hashtbl.find_exn hc app_cn in
    let auto_switches = generate_switches hf (List.hd_exn switch_cnL) (split_k (List.hd_exn auto_apps).input_metad [] 1) in
    (* ---- auto_lib ---- *)
    let libL = List.append (List.append auto_apps (List.append auto_sources auto_switches)) libL in
    generate_autoLibrary libL tl (List.tl_exn subarch_pL) hf hc;;

(* recursive function to filter out what from the usables (uL) list is not in the auto_lib (libL)*)
let rec whatsNotGenerated uL libL =
  match uL with
  | [] -> []
  | hd::tl -> let (cn, _,_,_,_,_) = hd in
    if ( List.fold ~init:false ~f:(fun a x -> a || (String.is_prefix x.name ~prefix:cn)) libL )
    then whatsNotGenerated tl libL
    else hd::whatsNotGenerated tl libL;;
    
(* recursive function to generate the library components that is in the in-between layer.
   uL is the list of missing components, pL is a list parameters for each element of uL,
   mdL is the list of meta_data from its connectable input, and i is a counter.
   right now we can only handle when uL is of one element (see let (cn,fn,_,_,_,fl) = List.hd_exn uL
   hard-coded in every recursive call. *)

let rec generate_inbtwLibrary uL mdL pL i = 
  match pL with
  | [] -> []
  | hd::tl -> let (cn,gen,_,_,_,fl) = List.hd_exn uL in
    let ctemp = (gen (cn^(string_of_int i)) fl hd) in
    (* build the metad *)
    let numflows = List.length ctemp.input_flows in
    let (mdLuse, mdLpass) = List.split_n mdL numflows in
      {name         = ctemp.name; 
       faults       = ctemp.faults;
       input_flows  = ctemp.input_flows;
       input_metad  = List.zip_exn ctemp.input_flows mdLuse;
       basic_events = ctemp.basic_events;
       event_info   = ctemp.event_info;
       output_flows = ctemp.output_flows;
       output_metad = List.zip_exn ctemp.output_flows mdLuse;
       formulas     = ctemp.formulas;
       use_with     = ctemp.use_with;
       generator    = ctemp.generator;} :: generate_inbtwLibrary uL mdLpass tl (i+1);;

(* ----------------------------------------- *)
(* === PHASE 2: functions for the MODEL  === *)
(* ----------------------------------------- *)

let rec makeNextList h cL =
  match cL with
    | []->[]
    | hd::tl -> List.append (Hashtbl.find_exn h hd) (makeNextList h tl);; 

let findLibraryComponent cn lib =
  List.filter lib ~f:(fun x -> String.is_prefix x.name ~prefix:cn);;

let rec findLibraryComponents cnl lib =
  match cnl with
    | []->[]
    | hd::tl-> List.append (List.filter lib ~f:(fun x -> String.is_prefix x.name ~prefix:hd))
      (findLibraryComponents tl lib);;

let rec callMakeInstance cn l =
  match l with 
    | []->[]
    | hd::tl -> List.append [makeInstance ~i:cn ~c:hd.name ()] (callMakeInstance cn tl);;

let rec makeInstanceList h lib cL =
  match cL with
    | []->[]
    | hd::tl -> 
      let (cn, _) = hd 
      in List.append 
      (List.append 
         (callMakeInstance cn (findLibraryComponent cn lib)) 
         (makeInstanceList h lib tl)
      )
      (makeInstanceList h lib (makeNextList h [hd]));;

(* ---- different approach --- *)
let rec generate_modelInstances libL =
  match libL with
    | [] -> []
    | hd::tl -> List.append [makeInstance ~i:hd.name ~c:hd.name ()] (generate_modelInstances tl) ;;

let popout l po =
  List.filter l ~f:(fun x -> x<>po);;

let rec keep_eliminate keep l po found =
  match found with
    | true -> List.append keep l
    | false ->
      if l = [] then keep_eliminate keep l po true
      else let (n,c) = List.hd_exn l in
           if c = po then keep_eliminate keep (List.tl_exn l) po true 
           else keep_eliminate (List.append keep [(n,c)]) (List.tl_exn l) po false;;

let popoutFirstEncounter l po =
  keep_eliminate [] l po false;;


let rec keep_move2back keep l po found =
  match found with
    | true -> List.append keep l
    | false ->
      if l = [] then keep_move2back keep l po true
      else let (n,c) = List.hd_exn l in
           if c = po then keep_move2back keep (List.append (List.tl_exn l) [(n,c)]) po true 
           else keep_move2back (List.append keep [(n,c)]) (List.tl_exn l) po false;;

let move2backFirstEncounter l po =
  keep_move2back [] l po false;;


let rec remove_i_from_input_flows l =
  match l with
    | []->[]
    | hd::tl -> List.append (List.tl_exn (String.split hd ~on:'i'))
      (remove_i_from_input_flows tl);;

(* create_modelConnectionsList
   
   Function to make the model connection list. Takes the following 3 arguments:
   - fi = int of number of inputs
   - tinpl = (string, [string list]) list = [("A664sw1",["1";"1";"1"]);("A664sw1",["1";"2";"1"]); ...
   - foutl = (string, [string list]) list = [("ADC",["1";"1"]);("A664sw1",["1";"2"]); ...
   
   This function matches up the first 2 strings in the string list.
*)

let rec create_modelConnectionsList fi tinpl foutl =
  match tinpl with
    | []->[]
    | thd::ttl ->
      match foutl with
        | []->[]
        | fhd::ftl ->
          let (tcn, tinp) = thd 
          and (fcn, fout) = fhd in
          let t = List.sub tinp ~pos:0 ~len:2 
          and f = List.sub fout ~pos:0 ~len:2 in
          if (t = f)
          then List.append [(tcn,tinp),(fcn,fout)] 
            (create_modelConnectionsList (List.length foutl) (popoutFirstEncounter tinpl tinp) (List.append ftl [fhd]))
          else if fi > 1 then create_modelConnectionsList (fi-1) tinpl (List.append ftl [fhd]) 
          else create_modelConnectionsList (List.length foutl) ttl (List.append ftl [fhd]) ;;


(* function to go from a list of library components to a list of (name, output_flows) tuples 
   and a list of (name, input_flows) tuples *)

let rec iterateThroughFlowList n fl =
  match fl with
    | []->[]
    | hd::tl -> (n,hd)::(iterateThroughFlowList n tl);;

let rec components_2_InputFlows cl =
  match cl with
    | []->[]
    | hd::tl -> 
      List.append (iterateThroughFlowList hd.name hd.input_flows)
        (components_2_InputFlows tl);;

let rec components_2_OutputFlows cl =
  match cl with
    | []->[]
    | hd::tl -> 
      List.append (iterateThroughFlowList hd.name hd.output_flows)
        (components_2_OutputFlows tl);;

(* takes the lc_to_inp list     and converts it to/from an s_r_c list *)
let rec flowListConvert l h =
  match l with
    | [] -> []
    | hd::tl -> (Hashtbl.find_exn h hd)::(flowListConvert tl h);;

(* takes a list of library components and a list of seen library components 
   and parses out only the library components that have not been seen *)
let rec listNotSeenOnly l seenL =
  match l with
    | [] -> []
    | hd::tl ->
      if (List.exists seenL ~f:(fun x -> x=hd )) 
      then (listNotSeenOnly tl seenL)
      else hd :: (listNotSeenOnly tl seenL);;

let rec generate_modelConnections libL myhc myh_f2s_in myh_f2s_out myh_s2f_in myh_s2f_out terml seenL =
  match terml with
    | []->[]
    | _ ->
      let startl = terml in
      let terml = List.concat (List.map terml ~f:(fun x -> Hashtbl.find_exn myhc x)) in
      let lc_to = findLibraryComponents startl libL
      and lc_fm = findLibraryComponents terml libL in
      let lc_to_inp = listNotSeenOnly (components_2_InputFlows lc_to) seenL
      and lc_from_out = components_2_OutputFlows lc_fm in
      let lc_to_inp_converted = flowListConvert lc_to_inp myh_f2s_in
      and lc_from_out_converted = flowListConvert lc_from_out myh_f2s_out in
      let cl = create_modelConnectionsList (List.length lc_from_out_converted) lc_to_inp_converted lc_from_out_converted in
      List.append
        (List.map cl ~f:(fun x -> let ((i_name, i_src),(o_name, o_src)) = x 
                                  in (Hashtbl.find_exn myh_s2f_in (i_name,i_src),Hashtbl.find_exn myh_s2f_out (o_name,o_src))))
        (if lc_to_inp = [] then
        generate_modelConnections libL myhc myh_f2s_in myh_f2s_out myh_s2f_in myh_s2f_out [] (List.append lc_to_inp seenL) 
        else
        generate_modelConnections libL myhc myh_f2s_in myh_f2s_out myh_s2f_in myh_s2f_out terml (List.append lc_to_inp seenL)
        );;

(* function to generate the top_fault for the model *)

let generate_topFault lc upT =
  let (flt,_) = upT.prob in
  (lc.name, ((List.hd_exn lc.output_flows),flt) );;

(* function to generate the model *)
(*  top_fault to be filled in by the synthesis algorithm so that arch can be reused
    to evaluate different top level faults *)

let generate_model upT libL hc h_f2s_in h_f2s_out h_s2f_in h_s2f_out =
  { instances = generate_modelInstances libL;
    connections = generate_modelConnections libL hc h_f2s_in h_f2s_out h_s2f_in h_s2f_out [upT.target] [];
    top_fault = ("", F["";""]);
  };;

(* these functions are for generating the voter xcomponent *)

(* function to take a list of output flows into a list of input flows *)
let rec transform_oflow_2_iflow_wOffset oL n =
  match oL with
    | [] -> []
    | hd::tl -> let [_;flownum] = String.split ~on:'o' hd in
      ("i" ^ (string_of_int ((int_of_string flownum)+n))) :: transform_oflow_2_iflow_wOffset tl n ;;

let rec generate_iflow cl i =
  match cl with
  | [] -> []
  | hd::tl -> List.append (transform_oflow_2_iflow_wOffset hd.output_flows i) (generate_iflow tl (i+(List.length hd.output_flows)));;

let rec transform_imetad_2_ometad mL =
  match mL with
    | [] -> []
    | hd::tl -> let (_,md) = hd in md :: transform_imetad_2_ometad tl ;;

let rec generate_imetad cl =
  match cl with
    | [] -> []
    | hd::tl -> List.append (transform_imetad_2_ometad hd.output_metad) (generate_imetad tl) ;;

(*Voter options - 1 = source select, 2 = n of n vote. m of n not supported yet *)
let generate_voterComp cl v =
  let iflow = (generate_iflow cl 0) in
  if v = 1 then (* Source select *)
     let ued_form = Or (List.map ~f:(fun x -> F[x;"ued"]) iflow)
     and loa_form = And (List.map ~f:(fun x -> F[x;"loa"]) iflow) in
  {name         = "vote-ss"; 
   faults       = ["ued";"loa"]; 
   input_flows  = iflow;  
   input_metad  = List.zip_exn (generate_iflow cl 0) (generate_imetad cl); 
   basic_events = ["uedflt"; "loaflt"];
   event_info   = [(0., 1.); (0., 1.)];
   output_flows = ["o1"];
   output_metad = []; 
   formulas     = [(["o1";"ued"], ued_form); (["o1";"loa"], loa_form)];
   use_with     = [];     
   generator    = "xvotegen";}

  else (* n of n vote *)
     let ued_form = And (List.map ~f:(fun x -> F[x;"ued"]) iflow)
     and loa_form = Or (List.map ~f:(fun x -> F[x;"loa"]) iflow) in
  {name         = "vote-voted"; 
   faults       = ["ued";"loa"]; 
   input_flows  = iflow;  
   input_metad  = List.zip_exn (generate_iflow cl 0) (generate_imetad cl); 
   basic_events = ["uedflt"; "loaflt"];
   event_info   = [(0., 1.); (0., 1.)];
   output_flows = ["o1"];
   output_metad = []; 
   formulas     = [(["o1";"ued"], ued_form); (["o1";"loa"], loa_form)];
   use_with     = [];     
   generator    = "xvotegen";} ;;

(* this function generates a connection list for cl to the voter component 
   follows same philosophy as generate_modelConnections, but uses the voter meta-data 
   as a list lookup for the voter component instead of a hash table *)
let generate_voteConnects cl voterComp myh_f2s_out myh_s2f_out =
  let lc_from_out = components_2_OutputFlows cl in
  let lc_from_out_converted = flowListConvert lc_from_out myh_f2s_out
  and lc_to_inp = components_2_InputFlows [voterComp] in
  let lc_to_inp_converted = List.map lc_to_inp ~f:(fun x -> 
    let (cn,iflow) = x in 
      let inp = List.Assoc.find_exn voterComp.input_metad iflow ~equal:(=) in 
        (cn,inp)
  ) in
  let c = create_modelConnectionsList (List.length lc_from_out_converted) lc_to_inp_converted lc_from_out_converted in
  let rev_inp_md = List.map ~f:(fun x -> let (iflow,md) = x in (md,iflow)) voterComp.input_metad in
  (List.map c ~f:(fun x -> let ((i_name, i_src),(o_name, o_src)) = x 
             in ((i_name,List.Assoc.find_exn rev_inp_md i_src ~equal:(=)),
                 Hashtbl.find_exn myh_s2f_out (o_name,o_src)))) ;;

(* function to generate the top_fault for the model from the voter component *)
let generate_voterTopFault upT vstr =
  let (flt,_) = upT.prob in
  (vstr, F["o1";flt] );;

(* pairing function to generate a unique ID from 2 integers *)

let generate_archNum x y z = (* z is added a digit to the right side of the number*)
  if x = (max x y) then 10*(x*x + x + y) + z else 10*(y*y + x) + z
  ;;

(* below are functions to test the pairing function. 
   if findRepeats does not return empty lists, then the pairing function is not generating unique numbers.
   test it like this ==> findRepeats (archNumList 24 4) *)
let rec archNumListInner i ii =
  match ii with
  | 1 -> [(i, ii, generate_archNum i ii)]
  | _ -> (i, ii, generate_archNum i ii) :: archNumListInner i (ii-1) ;;
  
let rec archNumList i ii =
  match i with
  | 1 -> archNumListInner i ii
  | _ -> List.append (archNumListInner i ii) (archNumList (i-1) ii) ;;

let rec findRepeats nL =
  match nL with 
  | [] -> []
  | hd::tl -> let (_, _, h_element) = hd in
    List.filter ~f:(fun x -> let (_,_,t_element) = x in h_element=t_element) tl 
    :: findRepeats tl ;;


(***************************************************************************************)
(* Here's the loop to auto synthesize architectures *)
(***************************************************************************************)

let syn_arch usables user_connectables user_probTargets h_sol_autolib h_sol_autoxlib h_sol_automdl h_sol_prob =
  (* -- STEP 0 -- translate the end-user inputs into lookup (hash) tables: 
     hf for functions; hc for connections *)
  let myhf = Hashtbl.Poly.create ()
  and myhf2cn = Hashtbl.Poly.create () 
  and myhc = Hashtbl.Poly.create () in
  let myhf = generate_cn2funHash usables myhf
  and myhf2cn = generate_fun2cnHash usables myhf2cn
  and myhc = generate_connectableHash user_connectables myhc in

  (* -- STEP 2 -- identify the heads of the sub-architectures *)
  let subarchL = id_subarch usables in

  (* -- STEP 3 -- sort the heads in order according to a connections graph *)

  (* -- STEP 4 -- iterate through all the archParamList tuples *)
  let archParamList = generate_archParamList (generate_archParamListList subarchL myhf myhc myhf2cn) in
  let archParamList = keep_only_valid_archParams archParamList in
  let numArchs = List.length archParamList in
  for i = 1 to numArchs do

   (* Voter options - 1 = source select, 2 = n of n vote. m of n not supported yet.
    * Future update could have the user define options for allowed voting and pass it into the algo *)
   for v = 1 to 2 do 

    (* -- STEP 5 -- AUTO_LIB -- *)
    let auto_lib = generate_autoLibrary [] subarchL (List.nth_exn archParamList (i-1)) myhf myhc in

    (* -- STEP 5.1 -- the in-between layer; using helper function -- *)
    (* 1 - what are the things in usables that are not in the auto_lib *)
    let inBtwL = whatsNotGenerated usables auto_lib in
    
    (* 2 - figure out how many options for the one inBtw comp 
           right now we can only handle one element in inBtwL list
           see "let (cn,_,gen_h,_,_,_) = List.hd_exn inBtwL" hard-coded *)
    let inbtwParamList = 
      if List.is_empty inBtwL then []
      else 
        let (cn,_,gen_h,_,_,_) = List.hd_exn inBtwL in
        let con_cn = Hashtbl.find_exn myhc cn in
        let param = List.fold ~init:0 ~f:(fun a x -> 
            if (String.is_prefix x.name ~prefix:(List.hd_exn con_cn)) 
            then (a+1) else (a+0) ) auto_lib in 
        gen_h param in (* <-- this is the call to the helper function *)

    let inBtwMetad = 
      if List.is_empty inBtwL then []
      else 
        let (cn,_,_,_,_,_) = List.hd_exn inBtwL in
        let con_cn = Hashtbl.find_exn myhc cn in
        List.fold ~init:[] ~f:(fun a x -> 
            if (String.is_prefix x.name ~prefix:(List.hd_exn con_cn)) 
            then (List.append a x.output_metad) 
            else (List.append a []) ) auto_lib in
                                          
    let (_,inBtwMetad) = List.unzip inBtwMetad in

    (* 3 - then generate the in-between layer and iterate through it *)
    let numInBtw = if List.is_empty inbtwParamList then 1 else List.length inbtwParamList in
    for ii = 1 to numInBtw do
    let inbtwParam = if List.is_empty inbtwParamList then [] else List.nth_exn inbtwParamList (ii-1) in
    let inbtw_lib = generate_inbtwLibrary inBtwL inBtwMetad inbtwParam 1 in
    let auto_lib = List.append inbtw_lib auto_lib in
  
    (* -- STEP 6 -- AUTO_MODEL *) 
    (* tables based on the auto-generated lib *)
    let myh_s2f_in = Hashtbl.Poly.create () and myh_s2f_out = Hashtbl.Poly.create ()
    and myh_f2s_in = Hashtbl.Poly.create () and myh_f2s_out = Hashtbl.Poly.create () in
    let myh_s2f_in = generate_src2flowHash_input auto_lib myh_s2f_in 
    and myh_s2f_out = generate_src2flowHash_output auto_lib myh_s2f_out
    and myh_f2s_in = generate_flow2srcHash_input auto_lib myh_f2s_in
    and myh_f2s_out = generate_flow2srcHash_output auto_lib myh_f2s_out in

    (* take the hd of the user_probTargets just as an example for the first make of the auto_model *)
    let upT = List.hd_exn user_probTargets in
    let auto_model = generate_model upT auto_lib myhc myh_f2s_in myh_f2s_out myh_s2f_in myh_s2f_out in
  
    (* -- STEP 7 -- run through the list of user_probTargets *)
    let numTargs = List.length user_probTargets in
    for t = 1 to numTargs do
      let upT = List.nth_exn user_probTargets (t-1) in
      (* find all the library components having to do with the target for this iteration *)
      let cl = findLibraryComponent upT.target auto_lib in
      (* generate a new library with the voter component appended to it *)
      let voterComp = generate_voterComp cl v in
      
      (* Define a name used for voter component based on the logic it is performing.
       * Used to make diagrams more informative. Note: Instance and Component names are same.*)
      let vt_str = 
        if v = 1 then "vote-ss"
        else "vote-voted" in
      
      let auto_lib_wVoter = voterComp :: auto_lib 
      (* generate another model with the voter component in it *)
      and vote_instance = makeInstance ~i:vt_str ~c:vt_str () 
      and vote_connects = generate_voteConnects cl voterComp myh_f2s_out myh_s2f_out in
    
      let auto_model_wVoter 
      = {instances = vote_instance :: auto_model.instances;
         connections = List.append vote_connects auto_model.connections;
         top_fault = generate_voterTopFault upT vt_str } in
  
      (* calculate and print the prob from the model *)
      let auto_lib_temp = xcomponent2component auto_lib_wVoter in
      let (p,_) = probErrorCut (model_to_ftree auto_lib_temp auto_model_wVoter)
      and (flt,_) = upT.prob in
      let archnum = generate_archNum i ii v in
      printf "Arch id #%i, " archnum ;
      printf "target #%i: " t ;
      print_string (upT.target ^ ", " ^ flt ^ " prob = ");
      printf "%.6e\n" p;
      Hashtbl.set h_sol_autolib ~key:(archnum,flt) ~data:auto_lib_temp;
      Hashtbl.set h_sol_autoxlib ~key:(archnum,flt) ~data:auto_lib_wVoter;
      Hashtbl.set h_sol_automdl ~key:(archnum,flt) ~data:auto_model_wVoter;
      Hashtbl.set h_sol_prob ~key:(archnum,flt) ~data:(flt, p);

    done
    done
   done
  done
;;
