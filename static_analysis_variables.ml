(* This file implements variable static analysis *)
(* i.e. for the moment constant analysis *)

(* WARNING: We ASSUME that the program consists of one function ONLY *)

open Simple_java_syntax
open Simple_java_display
open Simple_java_display

type var_state =
| Determined of s_constant
| Undetermined
module VarState = Map.Make(struct type t = int let compare = compare end)

let str_of_state = function
| Undetermined -> "undetermined"
| Determined(Sc_bool b) -> string_of_bool b
| Determined(Sc_int i) -> Int64.to_string i

let print_state gamma =
  let aux a b =
    print_endline ((string_of_int a) ^ " -> " ^ (str_of_state b)) in
  VarState.iter aux gamma

let print_debug gamma cmd =
  print_endline "BEFORE CMD...";
  print_command ">>" cmd;
  print_endline "...STATE IS:";
  print_state gamma;
  print_endline "----------"

let get_var_state gamma id =
  try
    VarState.find id gamma
  with
  | Not_found -> Undetermined

let set_var_state gamma id st =
  VarState.add id st gamma

let merge_states g1 g2 =
  let merge_func key a b = match a, b with
  | Some v1, Some v2 ->
    begin
      match v1, v2 with
      | Determined (Sc_int i1), Determined (Sc_int i2) when i1 = i2 ->
        Some (Determined (Sc_int i1))
      | Determined (Sc_bool b1), Determined (Sc_bool b2) when b1 = b2 ->
        Some (Determined (Sc_bool b1))
      | _, _ -> Some Undetermined
    end
  | Some _, None -> None
  | None, Some _ -> None in
  VarState.merge merge_func g1 g2

let rec compute_expr_state gamma expr = match fst expr with
| Se_const c -> Determined c
| Se_random(_, _) -> Undetermined
| Se_var v -> get_var_state gamma v.s_var_uniqueId
| Se_unary(Su_neg, e) ->
  begin
    match (compute_expr_state gamma e) with
    | Determined(Sc_bool b) -> Determined(Sc_bool (not b))
    | _ -> Undetermined
  end
| Se_binary(Sb_or, e1, e2) ->
  begin
    match (compute_expr_state gamma e1, compute_expr_state gamma e2) with
    | Determined(Sc_bool b1), Determined(Sc_bool b2) -> Determined(Sc_bool (b1 || b2))
    | _, _ -> Undetermined
  end
| Se_binary(op, e1, e2) ->
  begin
    match (compute_expr_state gamma e1, compute_expr_state gamma e2) with
    | Determined(Sc_int i1), Determined(Sc_int i2) ->
      (match op with
      | Sb_add -> Determined(Sc_int (Int64.add i1 i2))
      | Sb_sub -> Determined(Sc_int (Int64.sub i1 i2))
      | Sb_mul -> Determined(Sc_int (Int64.mul i1 i2))
      | Sb_div -> Determined(Sc_int (Int64.div i1 i2))
      | Sb_lt -> (if Int64.compare i1 i2 < 0 then Determined(Sc_bool true) else Determined(Sc_bool false))
      | _ -> Undetermined)
    | _, _ -> Undetermined
  end

let rec analyze_command gamma cmd = print_debug gamma cmd; match fst cmd with
| Sc_assign(v, expr) ->
  let new_st = compute_expr_state gamma expr in
  set_var_state gamma v.s_var_uniqueId new_st
| Sc_assert _ -> gamma
| Sc_while(expr, blk) ->
  begin
    match (compute_expr_state gamma expr) with
    | Determined(Sc_bool true) ->
      analyze_command (List.fold_left analyze_command gamma blk) cmd
    | _ -> gamma
  end
| Sc_if(expr, blk1, blk2) ->
  begin
    match (compute_expr_state gamma expr) with
    | Determined(Sc_bool true) -> List.fold_left analyze_command gamma blk1
    | Determined(Sc_bool false) -> List.fold_left analyze_command gamma blk2
    | Undetermined ->
      let state_t = List.fold_left analyze_command gamma blk1 in
      let state_f = List.fold_left analyze_command gamma blk2 in
      merge_states state_t state_f
  end
| Sc_proc_call _ -> gamma

let analyze_proc gamma pr =
  List.fold_left analyze_command gamma pr.s_proc_body

let analyze_var_decl gamma vd =
  let var = fst vd in
  match snd vd with
  | None -> set_var_state gamma var.s_var_uniqueId Undetermined
  | Some e -> set_var_state gamma var.s_var_uniqueId (compute_expr_state gamma e)

let analyze_decl gamma = function
| Sd_var vd ->
  analyze_var_decl gamma vd
| Sd_function p ->
  analyze_proc gamma p

let analyze_class cl =
  let gamma = VarState.empty in
  let final_gamma = List.fold_left analyze_decl gamma cl.s_class_body in
  print_endline "FINAL STATE IS:"; print_state final_gamma

let constant_analysis prg =
  (* I use List.iter although I expect programs with one class only... *)
  List.iter analyze_class prg
