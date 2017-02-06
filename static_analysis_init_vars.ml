(* This file implements some simple static analysis *)
open Simple_java_syntax
open Localizing

exception Not_init_var_error of extent

(* Part 2: detects use of uninitialized variables *)
(* ASSUMING that the program consists of one function ONLY *)
type var_state =
  | Initialized
  | Maybe_not_initialized
type analysis2_env = (int, var_state) Hashtbl.t

let rec list_inter_aux acc lst1 = function
| [] -> acc
| h::q when List.mem h lst1 -> list_inter_aux (h::acc) lst1 q
| h::q -> list_inter_aux acc lst1 q

let list_inter lst1 lst2 =
  list_inter_aux [] lst1 lst2

let is_initialized gamma id =
  try
    match Hashtbl.find gamma id with
    | Initialized -> true
    | Maybe_not_initialized -> false
  with
  | Not_found -> false

let set_not_initialized gamma id =
  try
    match Hashtbl.find gamma id with
    | Initialized ->
      Hashtbl.replace gamma id Maybe_not_initialized
    | Maybe_not_initialized -> ()
  with
  | Not_found ->
    Hashtbl.add gamma id Maybe_not_initialized

let set_initialized gamma id =
  try
    match Hashtbl.find gamma id with
    | Initialized -> ()
    | Maybe_not_initialized ->
      Hashtbl.replace gamma id Initialized
  with
  | Not_found ->
    Hashtbl.add gamma id Initialized

let check_critical_var loc gamma id =
  if not (is_initialized gamma id) then
    raise(Not_init_var_error loc)

let rec list_used_vars = function
| Se_const _ -> []
| Se_random(_, _) -> []
| Se_var v -> [v.s_var_uniqueId]
| Se_unary(_, e) -> list_used_vars (fst e)
| Se_binary(_, e1, e2) ->
  (list_used_vars (fst e1))@(list_used_vars (fst e2))

let check_expr gamma e =
  let loc = snd e in
  let l = list_used_vars (fst e) in
  List.iter (check_critical_var loc gamma) l

let check_var_decl gamma vd =
  let var = fst vd in
  match snd vd with
  | None -> set_not_initialized gamma var.s_var_uniqueId
  | Some e ->
    (check_expr gamma e; set_initialized gamma var.s_var_uniqueId)

let rec list_assigned_in_block_aux acc = function
| [] -> acc
| (Sc_assign(v, _), _)::q ->
  list_assigned_in_block_aux (v.s_var_uniqueId::acc) q
| (Sc_if(_, blk1, blk2), _)::q ->
  let l = list_assigned_in_if blk1 blk2 in
  list_assigned_in_block_aux (l@acc) q
| (_, _)::q ->
  list_assigned_in_block_aux acc q
and list_assigned_in_block cmd =
  list_assigned_in_block_aux [] cmd
and list_assigned_in_if blk1 blk2 =
  let lst1 = list_assigned_in_block blk1 in
  let lst2 = list_assigned_in_block blk2 in
  list_inter lst1 lst2

let rec check_command gamma b_do_not_init cmd = match fst cmd with
| Sc_assign(v, expr) ->
  begin
    check_expr gamma expr;
    if not b_do_not_init then
      set_initialized gamma v.s_var_uniqueId
  end
| Sc_assert expr -> check_expr gamma expr
| Sc_while(expr, blk) ->
  begin
    check_expr gamma expr;
    List.iter (check_command gamma true) blk
  end
| Sc_if(expr, blk1, blk2) ->
  begin
    check_expr gamma expr;
    List.iter (check_command gamma true) blk1;
    List.iter (check_command gamma true) blk2;
    let common_vars = list_assigned_in_if blk1 blk2 in
    List.iter (set_initialized gamma) common_vars
  end
| Sc_proc_call _ -> () (* not supported by check *)

let check_proc gamma pr =
  List.iter (check_command gamma false) pr.s_proc_body

let check_decl gamma = function
| Sd_var vd ->
  check_var_decl gamma vd
| Sd_function p ->
  check_proc gamma p

let check_class cl =
  let gamma = Hashtbl.create 10 in
  List.iter (check_decl gamma) cl.s_class_body

let check_non_initialized_vars prg =
    List.iter check_class prg
