(* 

Copyright © 2019 United States Government as represented by the Administrator of the 
National Aeronautics and Space Administration.  All Rights Reserved.

Author: Kit Siu
Date: 2018-01-22

*)

#use "topfind" ;;
#thread ;;
#require "core_extended" ;;
#directory "../_build";;
#directory "../_build/safety-analysis/tool";;
#directory "../_build/architecture-synthesis/tool";;

#load "faultTree.cmo" ;;
#load "qualitative.cmo" ;;
#load "quantitative.cmo" ;;
#load "modeling.cmo" ;;
#load "faultTreeSynthesis.cmo" ;;
#load "visualization.cmo" ;; 
#load "visualization_res.cmo" ;;
#load "syn_algo.cmo" ;;
#load "syn_val.cmo" ;;
#load "syn_out.cmo" ;;
#load "soteria.cmo" ;;

open Core;;
open FaultTree ;;
open Qualitative ;;
open Quantitative ;;
open Modeling ;;
open FaultTreeSynthesis ;;
open Visualization ;;
open Visualization_res ;;
open Syn_algo ;;
open Syn_val ;;
open Syn_out ;;
open Soteria ;;

#print_depth 1000 ;;
#print_length 10000 ;;

