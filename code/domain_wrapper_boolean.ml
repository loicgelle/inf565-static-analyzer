open Simple_java_syntax
open Simple_java_display

(* See similar interface in domains.ml for comments and information *)
module DomainWithBoolean(Dom: Domains.DomainType) = struct
  open Dom
  exception Cannot_simplify

  type bool_info =
  | Determined of bool
  | Undetermined
  type info_type =
  | Boolean of bool_info
  | Integer of Dom.info_type

  let val_undetermined_bool = Boolean(Undetermined)
  let val_undetermined_int = Integer(Dom.val_undetermined)

  let str_of_state = function
  | Boolean(Undetermined) -> "bool: undetermined"
  | Boolean(Determined(b)) -> "bool: " ^ (string_of_bool b)
  | Integer(info) -> "int: " ^ (Dom.str_of_state info)

  let merge_infos_bool i1 i2 = match i1, i2 with
  | Determined b1, Determined b2 when b1 = b2 -> Determined b1
  | _, _ -> Undetermined

  let merge_infos i1 i2 = match i1, i2 with
  | Boolean(b_info1), Boolean(b_info2) ->
    Boolean(merge_infos_bool b_info1 b_info2)
  | Integer(i_info1), Integer(i_info2) ->
    Integer(Dom.merge_infos i_info1 i_info2)
  | _, _ -> failwith "typing should be correct at that point"

  let const_to_info = function
  | Sc_bool b -> Boolean(Determined b)
  | Sc_int i -> Integer(Dom.const_to_info i)

  let random_to_info i1 i2 = Integer(Dom.random_to_info i1 i2)

  let unop_to_info = function
  | Boolean(Determined(b)) -> Boolean(Determined(not b))
  | _ -> Boolean(Undetermined)

  let is_true = function
  | Boolean(Determined(b)) -> b
  | _ -> false

  let is_false = function
  | Boolean(Determined(b)) -> not b
  | _ -> false

  let is_unchanged_bool b_info1 b_info2 = match b_info1, b_info2 with
  | Determined b1, Determined b2 when b1 = b2 -> true
  | _, _ -> false

  let is_unchanged info1 info2 = match info1, info2 with
  | Boolean(b_info1), Boolean(b_info2) -> is_unchanged_bool b_info1 b_info2
  | Integer(i_info1), Integer(i_info2) -> Dom.is_unchanged i_info1 i_info2
  | _, _ -> failwith "typing should be correct at that point"

  let info_to_expr = function
  | Boolean(Determined b) -> Se_const(Sc_bool b)
  | Integer(i_info) ->
    begin
      try
        Dom.info_to_expr(i_info)
      with
      | Cannot_simplify_in_domain -> raise Cannot_simplify
    end
  | _ -> raise Cannot_simplify

  let binop_to_info op info1 info2 = match op with
  | Sb_or ->
    begin
      match info1, info2 with
      | Boolean(Determined b1), Boolean(Determined b2) -> Boolean(Determined (b1 || b2))
      | _, _ -> Boolean(Undetermined)
    end
  | Sb_lt ->
    begin
      try
        match info1, info2 with
        | Integer(i_info1), Integer(i_info2) ->
          let b = Dom.is_lt i_info1 i_info2 in
          Boolean(Determined(b))
        | _, _ -> Boolean(Undetermined)
      with
      | Cannot_simplify_in_domain -> Boolean(Undetermined)
    end
    | Sb_eq ->
    begin
      try
        match info1, info2 with
        | Integer(i_info1), Integer(i_info2) ->
          let b = Dom.is_eq NoSpec i_info1 i_info2 in
          Boolean(Determined(b))
        | _, _ -> Boolean(Undetermined)
      with
      | Cannot_simplify_in_domain -> Boolean(Undetermined)
    end
  | _ ->
    begin
      match info1, info2 with
      | Integer(i_info1), Integer(i_info2) ->
        begin
          try
            match op with
            | Sb_add -> Integer(binop_add_to_info i_info1 i_info2)
            | Sb_sub -> Integer(binop_sub_to_info i_info1 i_info2)
            | Sb_mul -> Integer(binop_mul_to_info i_info1 i_info2)
            | Sb_div -> Integer(binop_div_to_info i_info1 i_info2)
            | _ -> Integer(Dom.val_undetermined)
          with
          | Cannot_simplify_in_domain -> Integer(Dom.val_undetermined)
        end
      | _, _ -> Integer(Dom.val_undetermined) (* should not happen since typing has been checked *)
    end

  let get_undetermined_st = function
  | Boolean(_) -> Boolean(Undetermined)
  | Integer(_) -> Integer(Dom.val_undetermined)

  let strip_bool_tag = function
  | Boolean(i) -> i
  | _ -> failwith "not a boolean info"

  let strip_int_tag = function
  | Integer(i) -> i
  | _ -> failwith "not an integer info"

  let extend_boolean_info =
    let rec aux last_info = function
    | [] -> Determined(last_info)
    | (Determined(h))::t when h = last_info -> aux last_info t
    | _ -> Undetermined in
    function
    | [] -> failwith "cannot extend without information"
    | (Determined(b))::t -> aux b t
    | _ -> Undetermined

  let extend_info b_extend info_lst = match info_lst with
  | [] -> failwith "cannot extend without information"
  | Boolean(_)::_ -> Boolean(extend_boolean_info (List.map strip_bool_tag info_lst))
  | Integer(_)::_ -> Integer(Dom.extend_info b_extend (List.map strip_int_tag info_lst))

  let reduce_states_eq info1 info2 = match info1, info2 with
  | Boolean(ib1), Boolean(ib2) -> Boolean(ib2)
  | Integer(ii1), Integer(ii2) -> Integer(Dom.reduce_states_eq ii1 ii2)
  | _, _ -> failwith "types should be correct at that point"

  let reduce_states_neq info1 info2 = match info1, info2 with
  | Boolean(ib1), Boolean(_) when is_true info2 -> Boolean(Determined(false))
  | Boolean(ib1), Boolean(_) when is_false info2 -> Boolean(Determined(true))
  | Boolean(_), Boolean(_) -> Boolean(Undetermined)
  | Integer(ii1), Integer(ii2) -> Integer(Dom.reduce_states_neq ii1 ii2)
  | _, _ -> failwith "types should be correct at that point"

  let reduce_states_lt info1 info2 = match info1, info2 with
  | Integer(ii1), Integer(ii2) -> Integer(Dom.reduce_states_lt ii1 ii2)
  | _, _ -> failwith "types should be correct at that point"

  let reduce_states_gte info1 info2 = match info1, info2 with
  | Integer(ii1), Integer(ii2) -> Integer(Dom.reduce_states_gte ii1 ii2)
  | _, _ -> failwith "types should be correct at that point"

end
