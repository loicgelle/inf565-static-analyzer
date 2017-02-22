(* This file implements variable static analysis *)

(* WARNING: We ASSUME that the program consists of one function ONLY *)

open Simple_java_syntax
open Abstract_domain

(* Print debug information *)
let debug = true

(* Instantiate abstract domain *)
module Domain = AbstractDomain(Domains.IntervalsType)
open Domain

let rec analyze_command gamma cmd = Domain.print_debug debug gamma cmd; match fst cmd with
| Sc_assign(v, expr) ->
  let new_st = Domain.compute_expr_state gamma expr in
  Domain.set_var_state gamma v.s_var_uniqueId new_st
| Sc_assert _ -> gamma
| Sc_while(expr, blk) ->
  let commands_fun g = List.fold_left analyze_command g blk in
  Domain.compute_loop_final_state commands_fun gamma expr
| Sc_if(expr, blk1, blk2) ->
  begin
    match (Domain.check_condition gamma expr) with
    | Verified -> List.fold_left analyze_command gamma blk1
    | Not_verified -> List.fold_left analyze_command gamma blk2
    | Unknown ->
      let state_t = List.fold_left analyze_command gamma blk1 in
      let state_f = List.fold_left analyze_command gamma blk2 in
      Domain.merge_states state_t state_f
  end
| Sc_proc_call _ -> gamma

let analyze_proc gamma pr =
  List.fold_left analyze_command gamma pr.s_proc_body

let analyze_var_decl gamma vd =
  let var = fst vd in
  match snd vd with
  | None -> Domain.set_var_state gamma var.s_var_uniqueId Domain.val_undetermined
  | Some e -> Domain.set_var_state gamma var.s_var_uniqueId (Domain.compute_expr_state gamma e)

let analyze_decl gamma = function
| Sd_var vd ->
  analyze_var_decl gamma vd
| Sd_function p ->
  analyze_proc gamma p

let analyze_class cl =
  let gamma = Domain.return_empty_state () in
  let final_gamma = List.fold_left analyze_decl gamma cl.s_class_body in
  Domain.print_final_debug debug final_gamma

let constant_analysis prg =
  (* I use List.iter although I expect programs with one class only... *)
  List.iter analyze_class prg
