open Simple_java_syntax
open Simple_java_display

module AbstractDomain (DT : Domains.DomainType) = struct
  type var_state = DT.info_type
  open DT

  type condition_status =
  | Verified
  | Not_verified
  | Unknown

  module VarState = Map.Make(struct type t = int let compare = compare end)

  let max_loop_rounds = 10

  let return_empty_state () = VarState.empty
  let val_undetermined = DT.val_undetermined

  let print_state gamma =
    let aux a b =
      print_endline ((string_of_int a) ^ " -> " ^ (DT.str_of_state b)) in
    VarState.iter aux gamma

  let print_debug debug gamma cmd = if not debug then () else
    (print_endline "BEFORE CMD...";
    print_command ">>" cmd;
    print_endline "...STATE IS:";
    print_state gamma;
    print_endline "----------")

  let print_final_debug debug gamma = if not debug then () else
    (print_endline "FINAL STATE IS:";
    print_state gamma;
    print_endline "----------")

  let get_var_state gamma id =
    try
      VarState.find id gamma
    with
    | Not_found -> val_undetermined

  let set_var_state gamma id st =
    VarState.add id st gamma

  let merge_states g1 g2 =
    let merge_func key a b = match a, b with
    | Some v1, Some v2 -> Some (DT.merge_infos v1 v2)
    | Some _, None -> None
    | None, Some _ -> None
    | None, None -> None in
    VarState.merge merge_func g1 g2

  let rec compute_expr_state gamma expr = match fst expr with
  | Se_const c -> DT.const_to_info c
  | Se_random(i1, i2) -> DT.random_to_info i1 i2
  | Se_var v -> get_var_state gamma v.s_var_uniqueId
  | Se_unary(Su_neg, e) -> DT.unop_to_info (compute_expr_state gamma e)
  | Se_binary(op, e1, e2) ->
    let info1 = compute_expr_state gamma e1 in
    let info2 = compute_expr_state gamma e2 in
    DT.binop_to_info op info1 info2

  let rec simplify_expr gamma expr =
    let loc = snd expr in
    match fst expr with
    | Se_var v ->
      begin
        let st = get_var_state gamma v.s_var_uniqueId in
        try
          let c = DT.info_to_expr st in
          (c, (snd expr))
        with
        | Cannot_simplify -> expr
      end
    | Se_unary(Su_neg, e) ->
      begin
        let simpl_e = simplify_expr gamma e in
        match fst simpl_e with
        | Se_const(Sc_bool b) -> (Se_const(Sc_bool (not b)), loc)
        | _ -> (Se_unary(Su_neg, simpl_e), loc)
      end
    | Se_binary(op, e1, e2) ->
      begin
        let simpl_e1 = simplify_expr gamma e1 in
        let simpl_e2 = simplify_expr gamma e2 in
        match op with
        | Sb_or ->
          begin
            match fst simpl_e1, fst simpl_e2 with
            | Se_const(Sc_bool b1), Se_const(Sc_bool b2) ->
              (Se_const(Sc_bool (b1 || b2)), loc)
            | _, _ -> (Se_binary(op, simpl_e1, simpl_e2), loc)
          end
        | Sb_lt ->
          begin
            match fst simpl_e1, fst simpl_e2 with
            | Se_const(Sc_int i1), Se_const(Sc_int i2) when Int64.compare i1 i2 < 0 ->
              (Se_const(Sc_bool true), loc)
            | Se_const(Sc_int _), Se_const(Sc_int _) ->
              (Se_const(Sc_bool false), loc)
            | _, _ -> (Se_binary(op, simpl_e1, simpl_e2), loc)
          end
        | _ ->
          begin
            match fst simpl_e1, fst simpl_e2 with
            | Se_const(Sc_int i1), Se_const(Sc_int i2) ->
              begin
                let res = match op with
                | Sb_add -> Int64.add i1 i2
                | Sb_mul -> Int64.mul i1 i2
                | Sb_div -> Int64.div i1 i2
                | Sb_sub -> Int64.sub i1 i2 in
                (Se_const(Sc_int res), loc)
              end
            | _, _ -> (Se_binary(op, simpl_e1, simpl_e2), loc)
          end
      end
    | _ -> expr

  let check_condition gamma expr =
    let st = compute_expr_state gamma expr in
    if DT.is_true st then Verified
    else if DT.is_false st then Not_verified
    else Unknown

  let compute_changed_vars g1 g2 =
    let merge_func key a b = match a, b with
    | Some v1, Some v2 when v1 = v2 -> None
    | None, None -> None
    | _, _ -> Some(true) in
    let g = VarState.merge merge_func g1 g2 in
    let lst = VarState.bindings g in
    List.map (fun (a, b) -> a) lst

  let set_changed_vars_to_unknown lst gamma =
    let change_fun g i =
      VarState.add i val_undetermined g in
    List.fold_left change_fun gamma lst

  let is_unchanged = DT.is_unchanged

  let compute_loop_final_state cmd_fun init_gamma expr =
    let rec aux cnt changed_vars last_gamma gamma =
      begin
        if cnt = max_loop_rounds then
          let new_gamma = set_changed_vars_to_unknown changed_vars gamma in
          merge_states new_gamma (cmd_fun new_gamma)
        else
          match (check_condition init_gamma expr) with
          | Not_verified -> gamma
          | Verified when cnt <= 1 ->
            aux (cnt + 1) changed_vars
              gamma (cmd_fun gamma)
          | Verified ->
            aux (cnt + 1) ((compute_changed_vars last_gamma gamma)@changed_vars)
              gamma (cmd_fun gamma)
          | Unknown ->
            let new_gamma1 = cmd_fun gamma in
            let new_changed_vars = ((compute_changed_vars new_gamma1 gamma)@changed_vars) in
            let new_gamma2 = set_changed_vars_to_unknown new_changed_vars new_gamma1 in
            merge_states gamma (cmd_fun new_gamma2)
      end in
    aux 0 [] init_gamma init_gamma



end
