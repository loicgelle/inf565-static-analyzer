open Simple_java_syntax
open Simple_java_display
open Simple_java_interpreter
open Static_analysis_init_vars
open Static_analysis_typing
open Static_analysis_variables
open Localizing

let usage = "usage: ./analyzer [options] file.java"
let interpret_class = ref ""
let domain_id = ref 0
let debug = ref false

let spec =
  [
    "--interpret", Arg.Set_string interpret_class, "  interpret function main of given class";
    "--domain", Arg.Set_int domain_id, "  specify abstract domain (0: constants; 1: intervals; 2: congruences; 3: congruences & intervals)";
    "--debug", Arg.Set debug, "  display useful information for debugging"
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
        (* Simplifications performed:
         * - dead code elimination
         * - simplification of expressions when possible *)
        let simpl_prg = match !domain_id with
        | 0 ->
          let module StaticAnalysisModule = StaticAnalysis(Domain_constants.ConstantsType) in
          StaticAnalysisModule.variables_analysis !debug prg
        | 1 ->
          let module StaticAnalysisModule = StaticAnalysis(Domain_intervals.IntervalsType) in
          StaticAnalysisModule.variables_analysis !debug prg
        | 2 ->
          let module StaticAnalysisModule = StaticAnalysis(Domain_congruences.CongruencesType) in
          StaticAnalysisModule.variables_analysis !debug prg
        | _ ->
          let module StaticAnalysisModule = StaticAnalysis(Domain_congruences_and_intervals.CongruencesAndIntervalsType) in
          StaticAnalysisModule.variables_analysis !debug prg in
        (* Prints simplified print_program*)
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
