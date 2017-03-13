(* This file allows interpreting programs in Simple Java syntax *)

open Localizing
open Simple_java_syntax

exception Interp_error of (string * (extent option))

let error s e = raise (Interp_error(s, e))

type var_value =
| Vint of int64
| Vbool of bool
| Vvoid
| Vnull (* not initialized *)

(* A var environment associates a varname with its value *)
type class_var_env = (string, var_value) Hashtbl.t
(* A proc environment associates a proc name with its instructions *)
type class_proc_env = (string, s_block) Hashtbl.t

(* A class environment stores its variable env and its proc env *)
type class_inner_env = { procs: class_proc_env; vars: class_var_env}
type class_env = (string, class_inner_env) Hashtbl.t

(* An environment associates a classname with its inner environment *)
type env = (string, class_env) Hashtbl.t

let print_var_value v_name v_val =
  let begstr = "   " ^ v_name ^ " = " in
  let valstr = match v_val with
  | Vint i -> Int64.to_string i
  | Vbool b -> string_of_bool b
  | Vvoid -> "void"
  | Vnull -> "null" in
  print_endline (begstr ^ valstr)

let print_class_env cl_name cl_inn_env =
  print_endline ("In class " ^ cl_name ^ ":");
  Hashtbl.iter print_var_value cl_inn_env.vars

let print_env gamma =
  Hashtbl.iter print_class_env gamma

let rec interp_expr v_gamma expr =
  let loc = Some(snd expr) in
  match (fst expr) with
  | Se_const(Sc_int i) -> Vint i
  | Se_const(Sc_bool b) -> Vbool b
  | Se_random(i, j) ->
    let v = Int64.add i (Random.int64 (Int64.sub j i)) in
    Vint v
  | Se_var v ->
    (try
      Hashtbl.find v_gamma v.s_var_name
    with
    | Not_found -> error ("Variable " ^ v.s_var_name ^ " not found") loc)
  | Se_unary(Su_neg, e) ->
    (match interp_expr v_gamma e with
    | Vbool b -> Vbool (not b)
    | _ -> error ("Cannot apply negation to non boolean value") loc)
  | Se_binary(Sb_or, e1, e2) ->
    (match interp_expr v_gamma e1, interp_expr v_gamma e2 with
    | Vbool b1, Vbool b2 -> Vbool (b1 || b2)
    | _ -> error ("Cannot apply OR to non boolean values") loc)
  | Se_binary(op, e1, e2) ->
    (match interp_expr v_gamma e1, interp_expr v_gamma e2 with
    | Vint i1, Vint i2 ->
      (match op with
      | Sb_add -> Vint (Int64.add i1 i2)
      | Sb_sub -> Vint (Int64.sub i1 i2)
      | Sb_mul -> Vint (Int64.mul i1 i2)
      | Sb_div -> Vint (Int64.div i1 i2)
      | Sb_lt -> (if Int64.compare i1 i2 < 0 then Vbool true else Vbool false)
      | _ -> error "case treated" loc)
    | _ -> error ("Cannot apply operator to non integer values") loc)

let rec interp_cmd cl_name gamma cmd =
  let loc = Some(snd cmd) in
  match fst cmd with
  | Sc_assign(v, expr) ->
    let var_env = (Hashtbl.find gamma cl_name).vars in
    let expr_val = interp_expr var_env expr in
    Hashtbl.replace var_env v.s_var_name expr_val
  | Sc_if(expr, b1, b2) ->
    (let var_env = (Hashtbl.find gamma cl_name).vars in
    let expr_val = interp_expr var_env expr in
    match expr_val with
    | Vbool true -> interp_block cl_name gamma b1
    | Vbool false -> interp_block cl_name gamma b2
    | _ -> error "IF statement needs a boolean value to test" loc)
  | Sc_while(expr, b) ->
    interp_cmd_while cl_name gamma expr b loc
  | Sc_proc_call pr_call ->
    (let proc_class = pr_call.s_proc_call_class in
    let proc_name = pr_call.s_proc_call_name in
    try
      let proc_class_env = Hashtbl.find gamma proc_class in
      let proc_block = Hashtbl.find proc_class_env.procs proc_name in
      interp_block proc_class gamma proc_block
    with
    | Not_found -> error ("Cannot execute proc " ^ proc_name
      ^ " in class " ^ proc_class ^ ": not found in environment") loc)
  | Sc_assert expr ->
    (let var_env = (Hashtbl.find gamma cl_name).vars in
    let expr_val = interp_expr var_env expr in
    match expr_val with
    | Vbool true -> ()
    | Vbool false -> error "assertion false" loc
    | _ -> error "ASSERT statement needs a boolean value to test" loc)
and interp_block cl_name gamma blk =
  List.iter (interp_cmd cl_name gamma) blk
and interp_cmd_while cl_name gamma testexpr b loc =
  let var_env = (Hashtbl.find gamma cl_name).vars in
  let expr_val = interp_expr var_env testexpr in
  match expr_val with
  | Vbool true ->
    interp_block cl_name gamma b;
    interp_cmd_while cl_name gamma testexpr b loc
  | Vbool false -> ()
  | _ -> error "WHILE statement needs a boolean value to test" loc

let init_decl_proc_env p_gamma sp =
  Hashtbl.add p_gamma sp.s_proc_name sp.s_proc_body

let init_decl_var_env v_gamma var expr =
  let v = match expr with
  | Some e -> interp_expr v_gamma e
  | None -> Vnull in
  match (var.s_var_type, v) with
  | _, Vnull
  | St_int, Vint _
  | St_bool, Vbool _
  | St_void, Vvoid ->
    Hashtbl.add v_gamma var.s_var_name v
  | _, _ -> error "incompatible types in declaration" (Some(var.s_var_extent))

let init_decl_env p_gamma v_gamma = function
| Sd_var (v, e) -> init_decl_var_env v_gamma v e
| Sd_function sp -> init_decl_proc_env p_gamma sp

let init_class_env gamma s_cl =
  let class_var_gamma = Hashtbl.create 10 in
  let class_proc_gamma = Hashtbl.create 10 in
  List.iter (init_decl_env class_proc_gamma class_var_gamma) s_cl.s_class_body;
  Hashtbl.add gamma s_cl.s_class_name
    {procs = class_proc_gamma; vars = class_var_gamma}

let init_env s_prg =
  Random.self_init ();
  let gamma = Hashtbl.create 10 in
  List.iter (init_class_env gamma) s_prg; gamma

(* Interpret given proc in given class *)
let interp_prg prg cl_name proc_name =
  let gamma = init_env prg in
  try
    begin
      let cl_env = Hashtbl.find gamma cl_name in
      let fun_blk = Hashtbl.find cl_env.procs proc_name in
      interp_block cl_name gamma fun_blk;
      print_endline "--- FINAL ENV ---";
      print_env gamma
    end
  with
  | Not_found -> error ("Cannot execute proc " ^ proc_name
    ^ " in class " ^ cl_name ^ ": not found in environment") None
