(* 

Copyright © 2019 United States Government as represented by the Administrator of the 
National Aeronautics and Space Administration.  All Rights Reserved.

Author: Kit Siu
Date: 2018-01-22

*)

open Core ;;
open Syn_algo ;;

(* Functions Used by multiple checks *)
let rec extractNames l = (* used by B1 and B3 *)
	match l with
	| [] -> []
	| hd::tl ->
		let (vnames, _, _, _, _, _) = hd in
		vnames :: extractNames tl;;

(****************************)
(* (A) Library Input Checks *)
(****************************)

(*****************************)
(* (B) End User Input Checks *)
(*****************************)

(* Check B1 *)
(* Verify that all the names of the usables are unique *)

(* Remove duplicate items from the sorted list*)
let rec deleteDuplicate sortedList =
      match sortedList with
      | [] -> []
      | x :: [] -> x :: []
      | x :: y :: rest ->
            if x = y then (print_string ("Check B1: Duplicate Name: " ^ x ^ "\n"); deleteDuplicate (y :: rest))
            else x :: deleteDuplicate (y :: rest);;

let checkNamesUnique l =
	let namelist = extractNames l in
	let sortednl = List.sort ~compare:(fun x y -> ~- (compare x y)) namelist in  (* Sort the list *)
	let uniquenl = deleteDuplicate sortednl in
	if (List.length namelist) = (List.length uniquenl) then 
		print_string ("Check B1: Passed\n")
	else print_string("Check B1: Failed - see above\n");;

(* Check B2 *)
(* Verify the (min,max) values: min >= 1, max > min *)
let rec extractMinMax l =
	match l with
	| [] -> []
	| hd::tl ->
		let (name,_,_,(min,max),_,_) = hd in (* "name" used to provide additional debug information*)
		 (min,max,name) :: extractMinMax tl;;

let rec checkMinMax_rec l =
	match l with
	| [] -> (print_string ("Check B2: Complete\n");[])
	| hd::tl ->
		let (min, max, name) = hd in
		if min < 1 then (print_string ("Check B2: Failed - (Min < 1)... " 			^name^" min = "^(string_of_int min)^"\n"); checkMinMax_rec tl)
	else if max < min then (print_string ("Check B2: Failed - (Max < Min)... " 
		^name^" ("^(string_of_int min) ^ "," ^ (string_of_int max) ^ ")\n"); 
		checkMinMax_rec tl)
	else (checkMinMax_rec tl);;
	
let checkMinMax l = 
	let minmaxlist = extractMinMax l in
	checkMinMax_rec minmaxlist;;


(* Check B3
 * Verify names match what is in usables name list
 *	B3a - check usables exist in connectable
 *	B3b - check connectable exist in usables
 *	B3c - check connectable_inputs exist in usables
 *	B3d - check connection_outputs exist in usables
 * Failures covered: 
 *	Name in connectable, _inputs, or _output list, not in usables.
 *	Name in usables, not in connectable. 
 *	Note: _inputs and _outputs could exist in usables only *)

let rec extractConn lc =
	match lc with
	| [] -> []
	| hd::tl ->
		hd.connectable :: extractConn tl;;

(* B3a *)
let rec usable_in_conn u c =
	match u with
	| [] -> Ok ""
	| hd::tl ->
		if (List.exists c ~f:(fun x-> x=hd)) then
		(* Found *)
		usable_in_conn tl c
		else
		(* Not Found *)
		Error ("Check B3a: Failed - Usable " ^ hd ^ 
			" Not Found in Connectable") ;;

(* B3b *)
let rec conn_in_usable u lc = 
	match lc with
	| [] -> Ok ""
	| hd::tl -> 
		if (List.exists u ~f:(fun x-> x=hd.connectable)) then
		(* Found *)
		conn_in_usable u tl
		else
		(* Not Found *)
		Error ("Check B3b: Failed - Conn " ^ hd.connectable ^ 
			" Not Found in Usables") ;;

(* B3c *)
let rec input_extract lc =
	match lc with
	| [] -> []
	| hd::tl ->
		hd.connectable_inputs :: input_extract tl;;

let rec input_in_usable u lc =
	match lc with
	| [] -> Ok ""
	| hd::tl ->
		if (List.exists u ~f:(fun x-> x=hd)) then
		(* Found *)
		input_in_usable u tl
		else
		(* Not Found *)
		Error ("Check B3c: Failed - InputConn " ^ hd ^ 
			"Not Found in Usables") ;;

(* B3d *)
let rec output_extract lc =
	match lc with
	| [] -> []
	| hd::tl ->
		hd.connectable_outputs :: output_extract tl;;

let rec output_in_usable u lc =
	match lc with
	| [] -> Ok ""
	| hd::tl ->
		if (List.exists u ~f:(fun x-> x=hd)) then
		(* Found *)
		output_in_usable u tl
		else
		(* Not Found *)
		Error ("Check B3d: Failed - OutputConn " ^ hd ^ 
			" Not Found in Usables");;

let checkConnectNames lu lc = (*lu=usables, lc=connectables*)
  let printErrorMsg res =
    match res with
    | Ok _ -> print_string "checkConnectNames: pass"
    | Error e -> print_string e 
  in
	let u = extractNames lu 
	and c = extractConn lc 
	and user_input_flat = List.concat (input_extract lc)
	and user_output_flat = List.concat (output_extract lc) in
	
	let resA = usable_in_conn u c  (* B3a *)
	and resB = conn_in_usable u lc  (* B3b *) 
	and resC = input_in_usable u user_input_flat  (* B3c *)
	and resD = output_in_usable u user_output_flat  (* B3d *)
  in
    if resA <> Ok "" then printErrorMsg resA 
    else if resB <> Ok "" then printErrorMsg resB 
    else if resC <> Ok "" then printErrorMsg resC 
    else if resD <> Ok "" then printErrorMsg resD 
    else printErrorMsg resD ;;