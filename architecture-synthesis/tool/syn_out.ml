(* 

Copyright © 2019 United States Government as represented by the Administrator of the 
National Aeronautics and Space Administration.  All Rights Reserved.

Author: Mike Noorman, Kit Siu
Date: 2018-01-22

*)

open Core ;;
open Syn_algo ;;

(* function call to print results:
 *   syn_results <user_probTargets> <myh_sol_prob> <sort_by>
 *   
 *   inputs:
 *      <user_probTargets> = targets used in synthesis
 *      <myh_sol_prob> = hash table of synthesis results
 *      <sort_by> = int (0 for first fault, 1 for second, ... from user_probTargets)
 *)


(* Hash Tables defined, addressed by (int, str) 
 *      where int is Arch # and str is fault - "ued" or "loa" in this example
 * component Library                    -> myh_sol_autolib
 * component Library w/ metadata        -> myh_sol_autoxlib
 * model with connections               -> myh_sol_automdl
 * calculation results                  -> myh_sol_prob
 *
 * Note: cutsets and fault trees are not saved, assume they are built on demand
 *)


(* 1: Filter results that meet all criteria.  
 *      Currently setup for 2 failure conditions, but should work for more
 * 2: Calculate delta between solution and target -> (target - solution)/target
 * 3: Add the percentages, sort from lowest percentage to highest. 
 *      Arch at top of list should be the "minimal architecture" relative
 *      to safety requirements.
 * (Following not implemented)
 * 4: If no results meet both criteria, Filter results that meet one criteria
 * 5: Find lowest probability for non-compliant failure condition
 *)

(* Hash for each Arch
 * Key: Arch#
 * Values: [(fault1, prob1, percent1);(fault2, prob2, percent2);( ...)]
 *)

let rec gen_entry arch upt ent h_sol_prob = 
        match upt with
        | [] -> ent
        | hd::tl ->
                let (flt, prob) = hd.prob in
                let v = Hashtbl.find_exn h_sol_prob (arch,flt) in
                let (_, sol) = v in
                let per = (prob -. sol) /. prob in
                let ent = (flt,sol,per)::ent in 
              (*  printf "debug arch: %i, perc: %f\n" arch per; *)
                gen_entry arch tl ent h_sol_prob;;


let one_arch_hash k h upt h_sol_prob =
       let (kar, kflt) = k in
       (*grab first fault only*)
       let thd = List.hd_exn upt in
       let (hdflt,_) = thd.prob in
       if hdflt = kflt then (*prep new hash entry*)
               begin
                 let entry = gen_entry kar upt [] h_sol_prob in
                 Hashtbl.set h ~key:kar ~data: entry
               end
       else (*Do Nothing*) ();;


(* Create a list of the architectures that meet all criteria *)

let rec res_check ar d h res =
        match d with
        | [] -> res
        | hd::tl ->
                let (_, _, perc) = hd in
                if perc < 0.0 then
                        res_check ar tl h []
                else res_check ar tl h res;;

let rec res_add_flt r fl reswf = (*fl is list of (fault,solution,percent) *)
        match fl with  
        | [] -> reswf
        | hd::tl ->
                let reswf = (r, hd) :: reswf in
                res_add_flt r tl reswf;;

let res_format r h =
        if r = [] then []
        else
                let fl = Hashtbl.find_exn h (List.hd_exn r) in 
                (* r should only have one entry, but .hd just in case *)
                res_add_flt (List.hd_exn r) fl [];;
        
let rec print_res_list_2 rwf = 
        match rwf with
        | [] -> ()
        | hd::tl ->
                  let (ar,(f,sol,per)) = hd in
                  let per_con = per *. 100.0 in
                  printf "Arch: %i, Fault: %s, Res: %.2e, %.1f%%\n" 
                        ar f sol per_con;
                  print_res_list_2 tl;;

let print_res_list_rec r =
        match r with
        | [] -> ()
        | hd::_ ->
                   printf "Arch #%i, " hd;;

let rec print_sorted_res_list_rec rwf = 
        match rwf with
        | [] -> ()
        | hd::tl ->
                  let (ar,(f,sol,per)) = hd in
                  let per_con = per *. 100.0 in
                  printf "Arch: %i, Fault: %s, Res: %.2e, %.1f%%\n" 
                        ar f sol per_con;
                  print_sorted_res_list_rec tl;;

(* Option 1 *)
let print_sorted_res_list1 l s upt =
        let tar = List.nth_exn upt s in
        let (flt,_) = tar.prob in
        printf "Results sorted by fault: %s\n" flt;
        print_sorted_res_list_rec l;;

(* Option 2 *)
let rec print_sorted_res_list2_rec l =
        match l with
        | [] -> printf "\n"
        | hd::tl ->
                let (_,(f,sol,per)) = hd in
                let per_con = per *. 100.0 in
                printf "%s = %.2e (%.1f%%) " f sol per_con;
                print_sorted_res_list2_rec tl;;

let rec print_sorted_res_list2_sub ll =
        match ll with
        | [] -> ()
        | hd::tl ->
                let (ar,(_,_,_)) = List.nth_exn hd 0 in
                printf "Arch %i: " ar;
                print_sorted_res_list2_rec hd;
                print_sorted_res_list2_sub tl;;

let print_sorted_res_list2 ll s upt = 
        let tar = List.nth_exn upt s in
        let (flt,_) = tar.prob in
        printf "Results sorted by fault: %s\n" flt;
        print_sorted_res_list2_sub ll;;



let sorter x y f =
       let (_,(_,_,perc1)) = List.nth_exn x f 
       and (_,(_,_,perc2)) = List.nth_exn y f in 
       if perc1 > perc2 then 1
       else if perc1 = perc2 then 0
       else -1;;

(* Better sorter could fold the list of results for each Arch and sum the Perc figures*)

let syn_results user_pt arch_res_h sol_h sort_by =
        
        Hashtbl.iteri ~f:(fun ~key:k ~data:_ -> 
                one_arch_hash k arch_res_h user_pt sol_h) sol_h;

        let allgood_h = Hashtbl.Poly.create () in

        (* Find Architectures that meet all criteria *)
        Hashtbl.iteri ~f:(fun ~key:k ~data:v -> 
                let results = res_check k v arch_res_h [k] in 
                (* returns a one entry list if all criteria are met *)
                (* print_res_list_rec results; *)
                let res_w_flt = res_format results arch_res_h in 
                (* returns list list *)
                (*debug printing
                 * print_res_list_2 res_w_flt;
                 *)
                if res_w_flt = [] then ()
                else Hashtbl.set allgood_h ~key:k ~data: res_w_flt
        ) arch_res_h;

        let res_list = Hashtbl.fold ~f:(fun ~key:_ ~data:d acc -> d::acc) 
                allgood_h ~init:[] in
        let sorted_res_ll = List.sort ~compare:(fun x y -> sorter x y sort_by) res_list in
        (* Working Code - Option 2 *)
        print_sorted_res_list2 sorted_res_ll sort_by user_pt;;

        (* Working code - Option 1 *)
        (*
        let sorted_res = List.concat sorted_res_ll in
        print_sorted_res_list1 sorted_res sort_by upt;;
        *)

(*Sample Call *)
(*syn_results user_probTargets myh_sol_prob 0;;*)
