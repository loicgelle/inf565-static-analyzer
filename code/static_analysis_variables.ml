(* This file implements variable static analysis *)
(* WARNING: We ASSUME that the program consists of one function ONLY *)

open Simple_java_syntax
open Abstract_domain

module StaticAnalysis (Dom : Domains.DomainType) = struct
  (* Possible simplifications in the AST *)
  type simplification =
  | Remove_Instr
  | Remove_True_Branch
  | Remove_False_Branch
  | Remove_While
  | Replace_Expr of s_expr_e

  (* Hashtable to store simplifications *)
  let simpl_tbl = Hashtbl.create 100

  module Domain = AbstractDomain(Dom)
  open Domain

  (* Associates a unique id to an expression for further simplification *)
  let simpl_id e = Localizing.extent_unique_id e

  (* Removes None values from a list *)
  let map_ignore_none mapfun lst =
    let rec aux acc = function
    | [] -> acc
    | h::t ->
      (match mapfun h with
        | None -> aux acc t
        | Some new_h -> aux ((List.rev new_h)@acc) t)
    in List.rev (aux [] lst)

  (* AST structure handlers and simplifiers *)
  (* -------------------------------------- *)
  let rec analyze_command b_debug gamma cmd = Domain.print_debug b_debug gamma cmd;
    let loc_id = simpl_id (snd cmd) in match fst cmd with
    | Sc_assign(v, expr) ->
      begin
        let new_st = Domain.compute_expr_state gamma expr in
        let old_st = Domain.get_var_state (v.s_var_type = St_bool) gamma v.s_var_uniqueId in
        let new_gamma = Domain.set_var_state gamma v.s_var_uniqueId new_st in
        if Domain.is_unchanged new_st old_st then
          Hashtbl.add simpl_tbl loc_id Remove_Instr
        else
          Hashtbl.add simpl_tbl loc_id (Replace_Expr(Domain.simplify_expr gamma expr));
        new_gamma
      end
    | Sc_assert _ -> gamma
    | Sc_while(expr, blk) ->
      begin
        let commands_fun g = List.fold_left (analyze_command b_debug) g blk in
        let new_gamma = Domain.compute_loop_final_state commands_fun gamma expr in
        (match Domain.check_condition gamma expr with
        | Not_verified ->
          Hashtbl.add simpl_tbl loc_id Remove_While
        | _ -> ());
        new_gamma
      end
    | Sc_if(expr, blk1, blk2) ->
      begin
        match (Domain.check_condition gamma expr) with
        | Verified ->
          (Hashtbl.add simpl_tbl loc_id Remove_False_Branch;
          List.fold_left (analyze_command b_debug) gamma blk1)
        | Not_verified ->
          (Hashtbl.add simpl_tbl loc_id Remove_True_Branch;
          List.fold_left (analyze_command b_debug) gamma blk2)
        | Unknown ->
          let (init_g_t, init_g_f) = Domain.build_cond_states gamma expr in
          let state_t = List.fold_left (analyze_command b_debug) init_g_t blk1 in
          let state_f = List.fold_left (analyze_command b_debug) init_g_f blk2 in
          (Hashtbl.add simpl_tbl loc_id (Replace_Expr(Domain.simplify_expr gamma expr));
          Domain.merge_states_if state_t state_f)
      end
    | Sc_proc_call _ -> gamma

  let rec simplify_command cmd =
    let loc = snd cmd in
    let loc_id = simpl_id loc in
    match fst cmd with
    | Sc_assign(v, _) ->
      begin
        try
          match Hashtbl.find simpl_tbl loc_id with
          | Remove_Instr -> None
          | Replace_Expr new_expr ->
            Some([Sc_assign(v, new_expr), loc])
          | _ -> Some([cmd])
        with
        | Not_found -> Some([cmd])
      end
    | Sc_while(_) ->
      begin
        try
          match Hashtbl.find simpl_tbl loc_id with
          | Remove_While -> None
          | _ -> Some([cmd])
        with
        | Not_found -> Some([cmd])
      end
    | Sc_if(_, blk1, blk2) ->
      begin
        try
          match Hashtbl.find simpl_tbl loc_id with
          | Remove_True_Branch -> Some(map_ignore_none simplify_command blk2)
          | Remove_False_Branch -> Some(map_ignore_none simplify_command blk1)
          | Replace_Expr new_expr ->
            let new_blk1 = map_ignore_none simplify_command blk1 in
            let new_blk2 = map_ignore_none simplify_command blk2 in
            Some([Sc_if(new_expr, new_blk1, new_blk2), loc])
          | _ -> Some([cmd])
        with
        | Not_found -> Some([cmd])
      end
    | _ -> Some([cmd])

  let analyze_proc b_debug gamma pr =
    List.fold_left (analyze_command b_debug) gamma pr.s_proc_body

  let simplify_proc pr =
    { s_proc_name = pr.s_proc_name;
      s_proc_body = map_ignore_none simplify_command pr.s_proc_body }

  let analyze_var_decl gamma vd =
    let var = fst vd in
    let undet_st = (if var.s_var_type = St_bool then val_undetermined_bool else val_undetermined_int) in
    match snd vd with
    | None -> Domain.set_var_state gamma var.s_var_uniqueId undet_st
    | Some e -> Domain.set_var_state gamma var.s_var_uniqueId (Domain.compute_expr_state gamma e)

  let analyze_decl b_debug gamma = function
  | Sd_var vd ->
    analyze_var_decl gamma vd
  | Sd_function p ->
    analyze_proc b_debug gamma p

  let simplify_decl = function
  | Sd_var vd ->
    Sd_var vd
  | Sd_function p ->
    Sd_function(simplify_proc p)

  let analyze_class b_debug cl =
    let gamma = Domain.return_empty_state () in
    let final_gamma = List.fold_left (analyze_decl b_debug) gamma cl.s_class_body in
    Domain.print_final_state final_gamma

  let simplify_class cl =
    { s_class_name = cl.s_class_name;
      s_class_body = List.map simplify_decl cl.s_class_body }

  (* Analysis entry point *)
  let variables_analysis b_debug prg =
    (* I use List.iter although I expect programs with one class only... *)
    List.iter (analyze_class b_debug) prg;
    List.map simplify_class prg
end
