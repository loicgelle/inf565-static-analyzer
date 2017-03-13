open Simple_java_syntax
open Simple_java_display
open Simple_java_interpreter
open Static_analysis_init_vars
open Static_analysis_typing
open Static_analysis_variables
open Localizing

let main () =
  (* Parsing arguments *)
  let f_name = ref "" in
  Arg.parse [ ] (fun s -> f_name := s) "Mini-Java analyzer";
  (* Parsing of the source file *)
  let simple_java_prog =
    if String.compare !f_name "" = 0 then failwith "no program file given";
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
      (*Simple_java_display.print_program prg;*)
      Static_analysis_init_vars.check_non_initialized_vars prg;
      Static_analysis_typing.check_typing prg;
      let simpl_prg = Static_analysis_variables.variables_analysis prg in
      Simple_java_display.print_program simpl_prg
    with
    | Interp_error(s, loc_opt) ->
      (match loc_opt with
      | None -> ()
      | Some loc -> print_endline (extent_to_string loc);
      print_endline "Interpretation error!"; print_endline s)
    | Not_init_var_error(loc) -> (print_endline (extent_to_string loc); print_endline "Error: use of not initialized variable")
    | Typing_error(loc) -> (print_endline (extent_to_string loc); print_endline "Typing error") in
  Printf.printf "finished...\n"

let _ = main ()
