(* This file allows printing programs from Simple Java syntax *)
(* Printing functions contain a left shift for pretty printing *)

open Simple_java_syntax

(* Tabulation between different levels *)
let shiftStr = "  "

let str_of_type = function
| St_int -> "int"
| St_bool -> "bool"
| St_void -> "void"

let print_var shift var =
  let var_type = str_of_type var.s_var_type in
  let var_uniqueId = string_of_int var.s_var_uniqueId in
  print_endline (shift ^ " "
    ^ "name: " ^ var.s_var_name ^ " | "
    ^ "type: " ^ var_type ^ " | "
    ^ "uniqueId: " ^ var_uniqueId)

let str_of_unary = function
| Su_neg -> "NOT"

let str_of_binary = function
| Sb_add -> "+"
| Sb_sub -> "-"
| Sb_mul -> "*"
| Sb_div -> "/"
| Sb_or -> "OR"
| Sb_eq -> "=="
| Sb_lt -> "<"

let rec str_of_expr_e expr = match fst expr with
| Se_const(Sc_int i) -> Int64.to_string i
| Se_const(Sc_bool b) -> string_of_bool b
| Se_random(i,j) -> "random(" ^ (Int64.to_string i) ^ ", " ^ (Int64.to_string j) ^ ")"
| Se_var v -> v.s_var_name
| Se_unary(op, e) -> (str_of_unary op) ^ " (" ^ (str_of_expr_e e) ^ ")"
| Se_binary(op, e1, e2) -> "(" ^ (str_of_expr_e e1) ^ " " ^ (str_of_binary op) ^ " " ^ (str_of_expr_e e2) ^ ")"

let print_expr_e shift expr =
  print_endline (shift ^ (str_of_expr_e expr))

let rec print_command shift comm = match fst comm with
| Sc_assign(v, e) ->
  print_endline (shift ^ (v.s_var_name) ^ " <- " ^ (str_of_expr_e e))
| Sc_if(e, b1, b2) ->
  (print_endline (shift ^ "if (" ^ (str_of_expr_e e) ^ ") then:");
  print_block (shift ^ shiftStr) b1;
  print_endline(shift ^ "else:");
  print_block (shift ^ shiftStr) b2)
| Sc_while(e, b) ->
  (print_endline (shift ^ "while (" ^ (str_of_expr_e e) ^ ") do:");
  print_block (shift ^ shiftStr) b)
| Sc_proc_call(s) ->
  print_endline (shift ^ "CALL " ^ s.s_proc_call_class ^ "." ^ s.s_proc_call_name);
| Sc_assert e ->
  print_endline (shift ^ "ASSERT " ^ (str_of_expr_e e));
and print_block shift block =
  List.iter (print_command (shift ^ shiftStr)) block

let print_proc shift proc =
  begin
    print_endline (shift ^ " Proc name: " ^ proc.s_proc_name);
    print_endline (shift ^ " Proc body:");
    print_block (shift ^ shiftStr) proc.s_proc_body
  end

let print_var_decl shift vd = match vd with
| v, init ->
  begin
    print_endline (shift ^ " " ^ "Variable:");
    print_var (shift ^ shiftStr) v;
    match init with
    | Some e ->
      begin
        print_endline (shift ^ " " ^ "Initializer:");
        print_expr_e (shift ^ shiftStr) e
      end
    | None -> ()
  end

let print_declaration shift decl =
  match decl with
  | Sd_var vd -> (
    print_endline (shift ^ " " ^ "Declaration of variable:");
    print_var_decl (shift ^ shiftStr) vd)
  | Sd_function fd -> (
    print_endline (shift ^ " " ^ "Declaration of function:");
    print_proc (shift ^ shiftStr) fd)

let print_class cl =
  begin
    print_endline ("Class name: " ^ cl.s_class_name);
    print_endline "Class body:";
    List.iter (fun decl -> print_declaration shiftStr decl) cl.s_class_body;
    print_endline "------------"
  end

let print_program p =
  List.iter (fun cl -> print_class cl) p
