(* 

Copyright © 2019 United States Government as represented by the Administrator of the 
National Aeronautics and Space Administration.  All Rights Reserved.

Author: Mike Noorman, Kit Siu
Date: 2018-02-09

*)

open Core ;;
open Modeling ;;
open Visualization ;;

(* Diagram Generator with Results
 * *)
 

let find_res_port nodes top =  (* (node id, port id) *)
  let (top_name, out_port) = top in
  let (_, node_id, _, outputs) =
  List.find_exn nodes
  ~f:(fun x -> let (inst, _, _, _) = x in
		inst.i_name = top_name) in
  let (port_id,_) =
    List.find_exn outputs 
  ~f:(fun y -> let (_, port_name) = y in
                port_name = out_port) in
  (node_id, port_id) ;;

let res_label sol chan = 
  List.iter sol
  ~f:(fun (flt,sol) ->
          Printf.fprintf chan "\n%s=%.1e" flt sol);;

let add_results port sol chan = 
  Printf.fprintf chan "\nnode [shape=box]" ;
  Printf.fprintf chan "Result1 [label=\"Results" ;
  res_label sol chan;
  Printf.fprintf chan "\"]";
  let (node_id, port_id) = port in
  Printf.fprintf chan "%d:%d -> " node_id port_id ;
  Printf.fprintf chan "Result1[arrowhead=none style=dashed];\n\n"; 
;;

let full_funct_dot_r ?(acc = (0, [], [])) lib model chan sol =
  let (_, nodes) = gen_funct_nodes lib model.instances in
  let edges = gen_funct_edges nodes model.connections in
  dot_gen_funct_nodes nodes chan; 
(* new code *)  
  let (a,F[b;_]) = model.top_fault in (* grab top_fault info for results *)
  let top = (a,b) in (* top = (inst,out)*)
  let port = find_res_port nodes top in  (* (node id, port id) *)
  add_results port sol chan;
(* ****** *)
  Printf.fprintf chan "\n";
  dot_gen_funct_edges edges chan ;;

let dot_gen_funct_arch_chan_r
    ?(splines="true")  ?(layout="dot") ?(overlap="false") ?(chan=stdout) ~lib ~model ~sol  =
  dot_gen_funct_arch_chan_helper
    splines layout overlap chan ;
  full_funct_dot_r lib model chan sol;
  Printf.fprintf chan "}\n" ;
;;

(* Format of top fault in model: top_fault = ("vote", ("o1", "ued"))
 * Connection: (("vote", "i1"), ("FMA1", "o1")); (("FMA1", "i01"), ("A664sw2", "o1"));
*) 

let dot_gen_show_funct_file_r 
    ?(rend="svg") ?(splines="true") ?(layout="dot") ?(overlap="false")
    ?(unflatten=true) lib model sol file = 
  dot_core_gen_show_file ~rend ~splines ~layout ~overlap ~unflatten file
    (dot_gen_funct_arch_chan_r ~lib ~model ~sol) ;;

(*Generate list of results for selected architecture *)
(* format [(flt1,sol1);(flt2,sol2);...] *)
let rec build_sol_ls d res =
        match d with
        | [] -> res
        | hd::tl ->
                let (flt, sol, _) = hd in
                let res = (flt,sol) :: res in
                build_sol_ls tl res
;;

let find_sol h arch =
        let d = Hashtbl.find_exn h arch in
        build_sol_ls d [];;

(* Setup to generate a diagram*)
let dot_gen_show_funct_res lib_h mdl_h res_h arch file =
  let saved_lib = Hashtbl.find_exn lib_h (arch, "ued") in
  (* fault selected here doesn't matter for what we are doing *)
  let saved_mdl = Hashtbl.find_exn mdl_h (arch, "ued") in
  let saved_sol = find_sol res_h arch in (* List of tuples *)
  dot_gen_show_funct_file_r saved_lib saved_mdl saved_sol 
        ("navpos_"^string_of_int arch^".gv");;

(*sample calls *)
(*
let arch_num = 21 in
dot_gen_show_funct_res myh_sol_autolib myh_sol_automdl arch_res_h arch_num ("navpos_"^string_of_int arch_num ^".gv");;

let arch_num = 24 in
dot_gen_show_funct_res myh_sol_autolib myh_sol_automdl arch_res_h arch_num ("navpos_"^string_of_int arch_num ^".gv");;
*)
