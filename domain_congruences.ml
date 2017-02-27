open Simple_java_syntax

module CongruencesType : Domains.DomainType = struct
  exception Cannot_simplify_in_domain

  type info_type = int64 * int64
    (* (a, b) means that n = a [b] *)

  let val_undetermined = Int64.zero, Int64.one

  let str_of_state = function
  | ia, ib -> "= " ^ (Int64.to_string ia)
      ^ " [" ^ (Int64.to_string ib) ^ "]"

  let const_to_info i = i, Int64.zero

  let rem i1 i2 =
    let r = Int64.rem i1 i2 in
    if Int64.compare r Int64.zero < 0 then Int64.add r i2 else r

  let merge_infos v1 v2 = match v1, v2 with
    | (ia1, ib1), (ia2, ib2)
      when Int64.equal ib2 Int64.zero && Int64.equal ib1 Int64.zero && Int64.equal ia1 ia2 ->
      ia1, Int64.zero
    | (ia1, ib1), (ia2, ib2)
      when Int64.equal ia1 ia2 && Int64.equal (rem ia1 ib1) (rem ia2 ib2) ->
      rem ia1 ib1, ib1
    | (ia1, ib1), (ia2, ib2)
      when Int64.equal ib2 Int64.zero && Int64.equal (rem ia1 ib1) (rem ia2 ib1) ->
      ia1, ib1
    | (ia1, ib1), (ia2, ib2)
      when Int64.equal ib1 Int64.zero && Int64.equal (rem ia1 ib2) (rem ia2 ib2) ->
      ia2, ib2
    | _, _ -> val_undetermined

  let random_to_info _ _ = val_undetermined

  let is_lt info1 info2 = match info1, info2 with
  | (ia1, ib1), (ia2, ib2)
    when Int64.equal ib1 Int64.zero && Int64.equal ib2 Int64.zero ->
    Int64.compare ia1 ia2 < 0
  | _, _ -> raise Cannot_simplify_in_domain

  let binop_add_to_info info1 info2 = match info1, info2 with
  | (ia1, ib1), (ia2, ib2) ->
    if Int64.equal ib1 Int64.zero && Int64.equal ib2 Int64.zero then
      Int64.add ia1 ia2, Int64.zero
    else if Int64.equal ib2 Int64.zero then
      rem (Int64.add ia1 ia2) ib1, ib1
    else if Int64.equal ib1 Int64.zero then
      rem (Int64.add ia1 ia2) ib2, ib2
    else if Int64.equal ib1 ib2 then
      rem (Int64.add ia1 ia2) ib1, ib1
    else val_undetermined

  let binop_sub_to_info info1 info2 = match info1, info2 with
  | (ia1, ib1), (ia2, ib2) -> failwith "not yet implemented"
    (* TODO *)

  let binop_mul_to_info info1 info2 = match info1, info2 with
  | (ia1, ib1), (ia2, ib2) -> failwith "not yet implemented"
    (* TODO *)

  let binop_div_to_info info1 info2 = match info1, info2 with
  | (ia1, ib1), (ia2, ib2) -> failwith "not yet implemented"
    (* TODO *)

  (*
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
    end *)

  let is_unchanged info1 info2 = match info1, info2 with
  | (ia1, ib1), (ia2, ib2)
    when Int64.equal ib2 Int64.zero && Int64.equal ib1 Int64.zero
    && Int64.equal ia1 ia2 -> true
  | (ia1, ib1), (ia2, ib2)
    when Int64.equal ib1 ib2 ->
    Int64.equal (rem ia1 ib1) (rem ia2 ib1)
  | _, _ -> false

  let info_to_expr = function
  | ia, ib when Int64.equal ib Int64.zero -> Se_const(Sc_int ia)
  | _ -> raise Cannot_simplify_in_domain
end
