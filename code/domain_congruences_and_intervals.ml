open Simple_java_syntax

module CongruencesAndIntervalsType : Domains.DomainType = struct
  exception Cannot_simplify_in_domain

  module C = Domain_congruences.CongruencesType
  module I = Domain_intervals.IntervalsType

  type info_type = C.info_type * I.info_type
  type spec_type =
  | RangeLength of int64
  | NoSpec

  let val_undetermined = (C.val_undetermined, I.val_undetermined)

  let str_of_state = function
  | ic, ii ->
    "(" ^ (C.str_of_state ic) ^ ", " ^ (I.str_of_state ii) ^ ")"

  let const_to_info i = (C.const_to_info i, I.const_to_info i)

  let merge_infos v1 v2 =
    (C.merge_infos (fst v1) (fst v2), I.merge_infos (snd v1) (snd v2))

  let random_to_info i1 i2 = (C.random_to_info i1 i2, I.random_to_info i1 i2)

  let is_lt info1 info2 =
    try
      I.is_lt (snd info1) (snd info2)
    with
    | I.Cannot_simplify_in_domain -> raise Cannot_simplify_in_domain

  let is_eq _ info1 info2 =
    try
      I.is_eq I.NoSpec (snd info1) (snd info2)
    with
    | I.Cannot_simplify_in_domain ->
      begin
        try
          let info = C.RangeLength(I.count_possible (snd info2)) in
          C.is_eq info (fst info1) (fst info2)
        with
        | C.Cannot_simplify_in_domain
        | I.Cannot_simplify_in_domain ->
          begin
            try
              let info = C.RangeLength(I.count_possible (snd info1)) in
              C.is_eq info (fst info2) (fst info1)
            with
            | C.Cannot_simplify_in_domain
            | I.Cannot_simplify_in_domain ->
              try
                C.is_eq C.NoSpec (fst info1) (fst info2)
              with
              | C.Cannot_simplify_in_domain -> raise Cannot_simplify_in_domain
          end
      end

  let binop_add_to_info info1 info2 =
    (C.binop_add_to_info (fst info1) (fst info2), I.binop_add_to_info (snd info1) (snd info2))

  let binop_sub_to_info info1 info2 =
    (C.binop_sub_to_info (fst info1) (fst info2), I.binop_sub_to_info (snd info1) (snd info2))

  let binop_mul_to_info info1 info2 =
    (C.binop_mul_to_info (fst info1) (fst info2), I.binop_mul_to_info (snd info1) (snd info2))

  let binop_div_to_info info1 info2 =
    (C.binop_div_to_info (fst info1) (fst info2), I.binop_div_to_info (snd info1) (snd info2))

  let is_unchanged info1 info2 =
    (C.is_unchanged (fst info1) (fst info2)) && (I.is_unchanged (snd info1) (snd info2))

  let info_to_expr info =
    try
      match C.info_to_expr (fst info), I.info_to_expr (snd info) with
      | Se_const(Sc_int ic), Se_const(Sc_int ii) when Int64.equal ic ii ->
        Se_const(Sc_int ic)
      | _, _ -> raise Cannot_simplify_in_domain
    with
    | C.Cannot_simplify_in_domain
    | I.Cannot_simplify_in_domain -> raise Cannot_simplify_in_domain

  let extend_info b_extend lst =
    let lstc = List.map (fun a -> fst a) lst in
    let lsti = List.map (fun a -> snd a) lst in
    (C.extend_info b_extend lstc, I.extend_info b_extend lsti)

  let reduce_states_eq info1 info2 =
    (C.reduce_states_eq (fst info1) (fst info2), I.reduce_states_eq (snd info1) (snd info2))

  let reduce_states_neq info1 info2 =
    (C.reduce_states_neq (fst info1) (fst info2), I.reduce_states_neq (snd info1) (snd info2))

  let reduce_states_lt info1 info2 =
    (C.reduce_states_lt (fst info1) (fst info2), I.reduce_states_lt (snd info1) (snd info2))

  let reduce_states_gte info1 info2 =
    (C.reduce_states_gte (fst info1) (fst info2), I.reduce_states_gte (snd info1) (snd info2))

  let count_possible _ =
    raise Cannot_simplify_in_domain

end
