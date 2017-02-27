open Simple_java_syntax

module ConstantsType : Domains.DomainType = struct
  exception Cannot_simplify

  type info_type =
  | Determined of s_constant
  | Undetermined

  let val_undetermined = Undetermined

  let str_of_state = function
  | Undetermined -> "undetermined"
  | Determined(Sc_bool b) -> string_of_bool b
  | Determined(Sc_int i) -> Int64.to_string i

  let merge_infos v1 v2 = match v1, v2 with
    | Determined (Sc_int i1), Determined (Sc_int i2) when i1 = i2 ->
      Determined (Sc_int i1)
    | Determined (Sc_bool b1), Determined (Sc_bool b2) when b1 = b2 ->
      Determined (Sc_bool b1)
    | _, _ -> Undetermined

  let const_to_info c = Determined c

  let random_to_info _ _ = Undetermined

  let unop_to_info = function
  | Determined(Sc_bool b) -> Determined(Sc_bool (not b))
  | _ -> Undetermined

  let binop_to_info op info1 info2 = match op with
  | Sb_or ->
    begin
      match info1, info2 with
      | Determined(Sc_bool b1), Determined(Sc_bool b2) -> Determined(Sc_bool (b1 || b2))
      | _, _ -> Undetermined
    end
  | _ ->
    begin
      match info1, info2 with
      | Determined(Sc_int i1), Determined(Sc_int i2) ->
        begin
          match op with
          | Sb_add -> Determined(Sc_int (Int64.add i1 i2))
          | Sb_sub -> Determined(Sc_int (Int64.sub i1 i2))
          | Sb_mul -> Determined(Sc_int (Int64.mul i1 i2))
          | Sb_div -> Determined(Sc_int (Int64.div i1 i2))
          | Sb_lt -> (if Int64.compare i1 i2 < 0 then Determined(Sc_bool true) else Determined(Sc_bool false))
          | _ -> Undetermined
        end
      | _, _ -> Undetermined (* should not happen since typing has been checked *)
    end

  let is_true = function
  | Determined(Sc_bool true) -> true
  | _ -> false

  let is_false = function
  | Determined(Sc_bool false) -> true
  | _ -> false

  let is_unchanged info1 info2 = match info1, info2 with
  | Determined(Sc_bool b1), Determined(Sc_bool b2) when b1 = b2 -> true
  | Determined(Sc_int i1), Determined(Sc_int i2) when Int64.equal i1 i2 -> true
  | _, _ -> false

  let info_to_expr = function
  | Undetermined -> raise Cannot_simplify
  | Determined(c) -> Se_const(c)

end
