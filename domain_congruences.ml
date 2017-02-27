open Simple_java_syntax

module CongruencesType : Domains.DomainType = struct
  exception Cannot_simplify
  type int_info = {a: int64; b: int64}
  type info_type =
  | Boolean of bool
  | Integer of int64 * int64
    (* (a, b) means that n = a [b] *)
  | Undetermined

  let val_undetermined = Undetermined

  let str_of_state = function
  | Boolean b ->
    string_of_bool b
  | Integer (ia, ib) -> "= " ^ (Int64.to_string ia)
      ^ " [" ^ (Int64.to_string ib) ^ "]"
  | Undetermined -> "undetermined"

  let const_to_info = function
  | Sc_int i -> Integer(i, Int64.zero)
  | Sc_bool b -> Boolean(b)

  let rem i1 i2 =
    let r = Int64.rem i1 i2 in
    if Int64.compare r Int64.zero < 0 then Int64.add r i2 else r

  let merge_infos v1 v2 = match v1, v2 with
    | Integer (ia1, ib1), Integer (ia2, ib2)
      when Int64.equal ib2 Int64.zero && Int64.equal ib1 Int64.zero && Int64.equal ia1 ia2 ->
      Integer(ia1, Int64.zero)
    | Integer (ia1, ib1), Integer (ia2, ib2)
      when Int64.equal ia1 ia2 && Int64.equal (rem ia1 ib1) (rem ia2 ib2) ->
      Integer(rem ia1 ib1, ib1)
    | Integer (ia1, ib1), Integer (ia2, ib2)
      when Int64.equal ib2 Int64.zero && Int64.equal (rem ia1 ib1) (rem ia2 ib1) ->
      Integer(ia1, ib1)
    | Integer (ia1, ib1), Integer (ia2, ib2)
      when Int64.equal ib1 Int64.zero && Int64.equal (rem ia1 ib2) (rem ia2 ib2) ->
      Integer(ia2, ib2)
    | Boolean b1, Boolean b2 when b1 = b2 ->
      Boolean b1
    | _, _ -> Undetermined

  let random_to_info i1 i2 = Undetermined

  let unop_to_info = function
  | Boolean b -> Boolean(not b)
  | _ -> Undetermined

  let binop_to_info op info1 info2 = match op with
  | Sb_or ->
    begin
      match info1, info2 with
      | Boolean b1, Boolean b2 -> Boolean (b1 || b2)
      | _, _ -> Undetermined
    end
  | _ ->
    begin
      match info1, info2 with
      | Integer (ia1, ib1), Integer (ia2, ib2)
        when Int64.equal ib1 Int64.zero && Int64.equal ib2 Int64.zero ->
        begin
          match op with
          | Sb_add -> Integer (Int64.add ia1 ia2, Int64.zero)
          | Sb_sub -> Integer (Int64.sub ia1 ia2, Int64.zero)
          | Sb_mul -> Integer (Int64.mul ia1 ia2, Int64.zero)
          | Sb_div -> Integer (Int64.div ia1 ia2, Int64.zero)
          | Sb_lt -> Boolean(Int64.compare ia1 ia2 < 0)
          | _ -> Undetermined
        end
      | Integer (ia1, ib1), Integer (ia2, ib2)
        when Int64.equal ib2 Int64.zero ->
        begin
          match op with
          | Sb_add -> Integer (rem (Int64.add ia1 ia2) ib1, ib1)
          | Sb_sub -> Integer (rem (Int64.sub ia1 ia2) ib1, ib1)
          | Sb_mul ->
            let newq = Int64.abs (Int64.mul ib1 ia2) in
            Integer (rem (Int64.mul ia1 ia2) newq, newq)
          | Sb_div ->
            begin
              if (Int64.equal (rem ia1 ia2) Int64.zero && Int64.equal (rem ia2 ib1) Int64.zero) then
                Integer (Int64.div ia1 ia2, Int64.abs (Int64.div ib1 ia2))
              else
                Undetermined
            end
          | Sb_lt -> Undetermined
          | _ -> Undetermined
        end
      | Integer (ia2, ib2), Integer (ia1, ib1)
        when Int64.equal ib2 Int64.zero ->
        begin
          match op with
          | Sb_add -> Integer (rem (Int64.add ia1 ia2) ib1, ib1)
          | Sb_sub -> Integer (rem (Int64.sub ia1 ia2) ib1, ib1)
          | Sb_mul ->
            let newq = Int64.abs (Int64.mul ib1 ia2) in
            Integer (rem (Int64.mul ia1 ia2) newq, newq)
          | Sb_div -> Undetermined
          | Sb_lt -> Undetermined
          | _ -> Undetermined
        end
      | Integer (ia1, ib1), Integer (ia2, ib2)
        when Int64.equal ib1 ib2 ->
        begin
          match op with
          | Sb_add -> Integer (rem (Int64.add ia1 ia2) ib1, ib1)
          | Sb_sub -> Integer (rem (Int64.sub ia1 ia2) ib1, ib1)
          | Sb_mul ->
            Integer (rem (Int64.mul ia1 ia2) ib1, ib1)
          | Sb_div -> Undetermined
          | Sb_lt -> Undetermined
          | _ -> Undetermined
        end
      | _, _ -> Undetermined (* should not happen since typing has been checked *)
    end

  let is_true = function
  | Boolean(b) -> b
  | _ -> false

  let is_false = function
  | Boolean(b) -> not b
  | _ -> false

  let is_unchanged info1 info2 = match info1, info2 with
  | Boolean b1, Boolean b2 when b1 = b2 -> true
  | Integer (ia1, ib1), Integer (ia2, ib2)
    when Int64.equal ib2 Int64.zero && Int64.equal ib1 Int64.zero
    && Int64.equal ia1 ia2 -> true
  | Integer (ia1, ib1), Integer (ia2, ib2)
    when Int64.equal ib1 ib2 ->
    Int64.equal (rem ia1 ib1) (rem ia2 ib1)
  | _, _ -> false

  let info_to_expr = function
  | Boolean(b) -> Se_const(Sc_bool b)
  | Integer (ia, ib) when Int64.equal ib Int64.zero -> Se_const(Sc_int ia)
  | _ -> raise Cannot_simplify
end
