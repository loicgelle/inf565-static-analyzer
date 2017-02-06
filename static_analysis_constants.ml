(* This file implements some simple static analysis *)
open Simple_java_syntax
open Simple_java_display

(* Part 4: Constant analysis *)
(* ASSUMING that the program consists of one function ONLY *)
type var_state =
  | Determined of s_constant
  | Undetermined
type analysis4_env = (int, var_state) Hashtbl.t

let str_of_state = function
| Undetermined -> "undetermined"
| Determined(Sc_bool b) -> string_of_bool b
| Determined(Sc_int i) -> Int64.to_string i

let print_env gamma =
  let aux a b =
    print_endline ((string_of_int a) ^ " -> " ^ (str_of_state b)) in
  Hashtbl.iter aux gamma

let set_var_state gamma id st =
  try
    Hashtbl.find gamma id;
    Hashtbl.replace gamma id st
  with
  | Not_found ->
    Hashtbl.add gamma id st

let get_var_state gamma id =
  try
    Hashtbl.find gamma id
  with
  | Not_found ->
    set_var_state gamma id Undetermined;
    Undetermined

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

let analyze_var_decl gamma vd =
  (*print_endline "------";
  print_endline "State before var decl...";
  print_var_decl "" vd;
  print_endline "...:";
  print_env gamma;*)
  let var = fst vd in
  let new_st = match snd vd with
  | None -> Undetermined
  | Some e -> (compute_expr_state gamma e) in
  set_var_state gamma var.s_var_uniqueId new_st
  (*print_endline "AFTER:";
  print_env gamma;
  print_endline "------"*)

let rec analyze_command gamma cmd =
  (*print_endline "------";
  print_endline "State before command...";
  print_command "" cmd;
  print_endline "...:";
  print_env gamma;*)
  match fst cmd with
  | Sc_assign(v, expr) ->
    let new_st = compute_expr_state gamma expr in
    set_var_state gamma v.s_var_uniqueId new_st
  | Sc_assert _ -> ()
  | Sc_while(expr, blk) ->
    begin
      match (compute_expr_state gamma expr) with
      | Determined(Sc_bool true) ->
        (List.iter (analyze_command gamma) blk;
        analyze_command gamma cmd)
      | _ -> ()
    end
  | Sc_if(expr, blk1, blk2) ->
    begin
      match (compute_expr_state gamma expr) with
      | Determined(Sc_bool true) -> List.iter (analyze_command gamma) blk1
      | Determined(Sc_bool false) -> List.iter (analyze_command gamma) blk2
      | _ -> ()
    end
  | Sc_proc_call _ -> () (* not supported by check *)
  (*print_endline "AFTER:";
  print_env gamma;
  print_endline "------"*)

let analyze_proc gamma pr =
  List.iter (analyze_command gamma) pr.s_proc_body

let analyze_decl gamma = function
| Sd_var vd ->
  analyze_var_decl gamma vd
| Sd_function p ->
  analyze_proc gamma p

let analyze_class cl =
  let gamma = Hashtbl.create 10 in
  List.iter (analyze_decl gamma) cl.s_class_body

let constant_analysis prg =
  List.iter analyze_class prg
