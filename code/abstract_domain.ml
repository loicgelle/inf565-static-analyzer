open Simple_java_syntax
open Simple_java_display

module AbstractDomain (Dom : Domains.DomainType) = struct
  module DT = Domain_wrapper_boolean.DomainWithBoolean(Dom)
  type var_state = DT.info_type
  open DT

  type condition_status =
  | Verified
  | Not_verified
  | Unknown

  module VarState = Map.Make(struct type t = int let compare = compare end)

  let max_loop_rounds = 10

  let return_empty_state () = VarState.empty
  let val_undetermined_bool = DT.val_undetermined_bool
  let val_undetermined_int = DT.val_undetermined_int

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

  let print_final_state gamma =
    (print_endline "FINAL STATE IS:";
    print_state gamma;
    print_endline "----------")

  let get_var_state is_boolean gamma id =
    try
      VarState.find id gamma
    with
    | Not_found when is_boolean -> DT.val_undetermined_bool
    | Not_found -> DT.val_undetermined_int

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
  | Se_var v -> get_var_state (v.s_var_type = St_bool) gamma v.s_var_uniqueId
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
        let st = get_var_state (v.s_var_type = St_bool) gamma v.s_var_uniqueId in
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
        | Sb_eq ->
          begin
            match fst simpl_e1, fst simpl_e2 with
            | Se_const(Sc_int i1), Se_const(Sc_int i2) when Int64.compare i1 i2 = 0 ->
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
                | Sb_sub -> Int64.sub i1 i2
                | _ -> failwith "typing should be correct at that point" in
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
      let oldst = VarState.find i g in
      let newst = DT.get_undetermined_st oldst in
      VarState.add i newst g in
    List.fold_left change_fun gamma lst

  let is_unchanged = DT.is_unchanged

  let states_union gammalst =
    let merge_func key a b = match a, b with
    | Some v1, Some v2 -> Some(v2::v1)
    | Some _, None -> None
    | None, Some _ -> None
    | None, None -> None in
    let empty_gamma = VarState.map (fun _ -> []) (List.hd gammalst) in
    let gamma_lst_states = List.fold_left (VarState.merge merge_func) empty_gamma gammalst in
    VarState.map (DT.extend_info true) gamma_lst_states

  let merge_states_if g1 g2 =
    let merge_func key a b = match a, b with
    | Some v1, Some v2 -> Some(v2::v1)
    | Some _, None -> None
    | None, Some _ -> None
    | None, None -> None in
    let empty_gamma = VarState.map (fun _ -> []) g1 in
    let gamma_lst_states = List.fold_left (VarState.merge merge_func) empty_gamma [g1; g2] in
    VarState.map (DT.extend_info false) gamma_lst_states

  let rec build_cond_states gamma expr = match fst expr with
  | Se_binary(Sb_eq, e1, e2) ->
    begin
      let (g1_t, g1_f) = match fst e1 with
      | Se_var v ->
        let curr_st = get_var_state (v.s_var_type = St_bool) gamma v.s_var_uniqueId in
        let new_st_t = DT.reduce_states_eq curr_st (compute_expr_state gamma e2) in
        let new_st_f = DT.reduce_states_neq curr_st (compute_expr_state gamma e2) in
        (set_var_state gamma v.s_var_uniqueId new_st_t, set_var_state gamma v.s_var_uniqueId new_st_f)
      | _ -> (gamma, gamma) in
      match fst e2 with
      | Se_var v ->
        let curr_st = get_var_state (v.s_var_type = St_bool) gamma v.s_var_uniqueId in
        let new_st_t = DT.reduce_states_eq curr_st (compute_expr_state gamma e1) in
        let new_st_f = DT.reduce_states_neq curr_st (compute_expr_state gamma e1) in
        (set_var_state g1_t v.s_var_uniqueId new_st_t, set_var_state g1_f v.s_var_uniqueId new_st_f)
      | _ -> (g1_t, g1_f)
    end
  | Se_binary(Sb_lt, e1, e2) ->
    begin
      let (g1_t, g1_f) = match fst e1 with
      | Se_var v ->
        let curr_st = get_var_state (v.s_var_type = St_bool) gamma v.s_var_uniqueId in
        let new_st_t = DT.reduce_states_lt curr_st (compute_expr_state gamma e2) in
        let new_st_f = DT.reduce_states_gte curr_st (compute_expr_state gamma e2) in
        (set_var_state gamma v.s_var_uniqueId new_st_t, set_var_state gamma v.s_var_uniqueId new_st_f)
      | _ -> (gamma, gamma) in
      match fst e2 with
      | Se_var v ->
        let curr_st = get_var_state (v.s_var_type = St_bool) gamma v.s_var_uniqueId in
        let new_st_t = DT.reduce_states_gte curr_st (compute_expr_state gamma e1) in
        let new_st_f = DT.reduce_states_lt curr_st (compute_expr_state gamma e1) in
        (set_var_state g1_t v.s_var_uniqueId new_st_t, set_var_state g1_f v.s_var_uniqueId new_st_f)
      | _ -> (g1_t, g1_f)
    end
  | _ -> gamma, gamma


  let compute_loop_final_state cmd_fun init_gamma expr =
    let rec aux cnt saved_gammas gamma =
      begin
        if cnt = max_loop_rounds then
          let new_gamma = states_union ((cmd_fun gamma)::saved_gammas) in
          merge_states new_gamma init_gamma
        else
          match (check_condition gamma expr) with
          | Not_verified -> gamma
          | Verified ->
            let new_st = cmd_fun gamma in
            aux (cnt + 1) (new_st::saved_gammas) new_st
          | Unknown ->
            let new_gamma1 = cmd_fun gamma in
            let new_gamma2 = states_union (new_gamma1::saved_gammas) in
            merge_states init_gamma (cmd_fun new_gamma2)
      end in
    aux 0 [] init_gamma

end
