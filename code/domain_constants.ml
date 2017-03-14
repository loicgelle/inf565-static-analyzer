open Simple_java_syntax

(* See interface in domains.ml for comments and information *)
module ConstantsType : Domains.DomainType = struct
  exception Cannot_simplify_in_domain

  type info_type =
  | Determined of int64
  | Undetermined
  type spec_type =
  | RangeLength of int64
  | NoSpec

  let val_undetermined = Undetermined

  let str_of_state = function
  | Undetermined -> "undetermined"
  | Determined(i) -> Int64.to_string i

  let merge_infos v1 v2 = match v1, v2 with
    | Determined i1, Determined i2 when i1 = i2 ->
      Determined i1
    | _, _ -> Undetermined

  let const_to_info c = Determined c

  let random_to_info _ _ = Undetermined

  let is_lt info1 info2 = match info1, info2 with
  | Determined i1, Determined i2 ->
    Int64.compare i1 i2 < 0
  | _, _ -> raise Cannot_simplify_in_domain

  let is_eq _ info1 info2 = match info1, info2 with
  | Determined i1, Determined i2 ->
    Int64.compare i1 i2 = 0
  | _, _ -> raise Cannot_simplify_in_domain

  let binop_add_to_info info1 info2 = match info1, info2 with
  | Determined i1, Determined i2 -> Determined(Int64.add i1 i2)
  | _, _ -> raise Cannot_simplify_in_domain

  let binop_sub_to_info info1 info2 = match info1, info2 with
  | Determined i1, Determined i2 -> Determined(Int64.sub i1 i2)
  | _, _ -> raise Cannot_simplify_in_domain

  let binop_mul_to_info info1 info2 = match info1, info2 with
  | Determined i1, Determined i2 -> Determined(Int64.mul i1 i2)
  | _, _ -> raise Cannot_simplify_in_domain

  let binop_div_to_info info1 info2 = match info1, info2 with
  | Determined i1, Determined i2 -> Determined(Int64.add i1 i2)
  | _, _ -> raise Cannot_simplify_in_domain

  let is_unchanged info1 info2 = match info1, info2 with
  | Determined i1, Determined i2 when Int64.equal i1 i2 -> true
  | _, _ -> false

  let info_to_expr = function
  | Undetermined -> raise Cannot_simplify_in_domain
  | Determined i -> Se_const(Sc_int i)

  let extend_info _ lst =
    let rec extend_info_aux info1 info2 =
      if is_unchanged info1 info2 then info1
      else val_undetermined in
    match lst with
    | [] -> val_undetermined
    | h::t -> List.fold_left extend_info_aux h t

  let reduce_states_gte info1 _ = info1
  let reduce_states_lt info1 _ = info1
  let reduce_states_neq info1 info2 = info1

  let reduce_states_eq info1 info2 = match info1, info2 with
  | _, Determined(_) -> info2
  | _, _ -> info1

  let count_possible = function
  | Determined(_) -> Int64.one
  | _ -> raise Cannot_simplify_in_domain

end
