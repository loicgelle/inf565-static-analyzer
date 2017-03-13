(* This file implements some simple static analysis *)
open Simple_java_syntax
open Localizing

exception Typing_error of extent

(* Part 3: detects typing errors *)
let check_types_consistency loc t1 t2 = match t1, t2 with
| St_int, St_int -> ()
| St_bool, St_bool -> ()
| St_void, St_void -> ()
| _, _ -> raise(Typing_error(loc))

let rec get_expr_type expr =
  let loc = snd expr in
  match fst expr with
  | Se_const(Sc_int _) -> St_int
  | Se_const(Sc_bool _) -> St_bool
  | Se_random(_, _) -> St_int
  | Se_var v -> v.s_var_type
  | Se_unary(Su_neg, e) ->
    begin
      check_types_consistency loc St_bool (get_expr_type e);
      St_bool
    end
  | Se_binary(op, e1, e2) ->
    (match op with
    | Sb_or ->
      begin
        check_types_consistency loc St_bool (get_expr_type e1);
        check_types_consistency loc St_bool (get_expr_type e2);
        St_bool
      end
    | Sb_lt | Sb_eq ->
      begin
        check_types_consistency loc St_int (get_expr_type e1);
        check_types_consistency loc St_int (get_expr_type e2);
        St_bool
      end
    | _ ->
      begin
        check_types_consistency loc St_int (get_expr_type e1);
        check_types_consistency loc St_int (get_expr_type e2);
        St_int
      end)

let check_var_decl vd =
  let var = fst vd in
  match snd vd with
  | None -> ()
  | Some e ->
    check_types_consistency (snd e) (var.s_var_type) (get_expr_type e)

let rec check_command cmd =
  let loc = snd cmd in
  match fst cmd with
  | Sc_assign(v, expr) ->
    check_types_consistency loc (v.s_var_type) (get_expr_type expr)
  | Sc_assert expr ->
    check_types_consistency loc St_bool (get_expr_type expr)
  | Sc_while(expr, _) ->
    check_types_consistency loc St_bool (get_expr_type expr)
  | Sc_if(expr, _, _) ->
    check_types_consistency loc St_bool (get_expr_type expr)
  | Sc_proc_call _ -> ()

let check_proc pr =
  List.iter check_command pr.s_proc_body

let check_decl = function
| Sd_var vd ->
  check_var_decl vd
| Sd_function p ->
  check_proc p

let check_class cl =
  List.iter check_decl cl.s_class_body

let check_typing prg =
  List.iter check_class prg
