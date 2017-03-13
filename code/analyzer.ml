open Simple_java_syntax
open Simple_java_display
open Simple_java_interpreter
open Static_analysis_init_vars
open Static_analysis_typing
open Static_analysis_variables
open Localizing

let usage = "usage: ./analyzer [options] file.java"
let interpret_class = ref ""

let spec =
  [
    "--interpret", Arg.Set_string interpret_class, "  interpret function main of given class";
  ]

let main () =
  (* Parsing arguments *)
  let f_name = ref "" in
  Arg.parse spec (fun s -> f_name := s) "Mini-Java analyzer";
  (* Parsing of the source file *)
  let simple_java_prog =
    if String.compare !f_name "" = 0 then (Arg.usage spec usage; exit 1);
    Localizing.current_file_name := !f_name;
    let f_desc = open_in !f_name in
    let lexbuf = Lexing.from_channel f_desc in
    let java_prog =
      try Java_parser.program Java_lexer.token lexbuf
      with
      | e ->
          Printf.printf "Exception during parsing: %s\n"
	    (Printexc.to_string e);
          failwith "Stopped" in
    let prg = Simple_java_translate.tr_java_prog java_prog in
    try
      if not (String.compare !interpret_class "" = 0) then Simple_java_interpreter.interp_prg prg !interpret_class
      else begin
        (* Performs variable initialization static analysis *)
        Static_analysis_init_vars.check_non_initialized_vars prg;
        (* Performs typing static analysis *)
        Static_analysis_typing.check_typing prg;
        (* Performs variable static analysis and program simplification *)
        (* The domain used - constants, intervals, congruences, ... - has
         * to be precised in the module Static_analysis_variables *)
        (* Simplifications performed:
         * - dead code elimination
         * - simplification of expressions when possible *)
        let simpl_prg = Static_analysis_variables.variables_analysis prg in
        (* Prints simplified program *)
        Simple_java_display.print_program simpl_prg
      end
    with
    | Interp_error(s, loc_opt) ->
      (match loc_opt with
      | None -> (print_endline "Interpretation error!"; print_endline s)
      | Some loc -> print_endline (extent_to_string loc);
      print_endline "Interpretation error!"; print_endline s)
    | Not_init_var_error(loc) -> (print_endline (extent_to_string loc); print_endline "Error: use of not initialized variable")
    | Typing_error(loc) -> (print_endline (extent_to_string loc); print_endline "Typing error") in
  Printf.printf "finished...\n"

let _ = main ()
