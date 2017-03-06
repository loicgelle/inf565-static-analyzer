open Simple_java_syntax

module CongruencesType : Domains.DomainType = struct
  exception Cannot_simplify_in_domain

  type info_type =
  | Congruence of int64 * int64
  | Cst of int64
    (* (a, b) means that n = a [b] *)

  let val_undetermined = Congruence(Int64.zero, Int64.one)

  let str_of_state = function
  | Congruence(ia, ib) -> "= " ^ (Int64.to_string ia)
      ^ " [" ^ (Int64.to_string ib) ^ "]"
  | Cst(i) -> Int64.to_string i

  let const_to_info i = Cst(i)

  let rem i1 i2 =
    let r = Int64.rem i1 i2 in
    if Int64.compare r Int64.zero < 0 then Int64.add r i2 else r

  let merge_infos v1 v2 = match v1, v2 with
  | Cst(i1), Cst(i2) -> if Int64.equal i1 i2 then Cst(i1) else val_undetermined
  | Congruence(ia1, ib1), Congruence(ia2, ib2) ->
    if not (Int64.equal ib1 ib2) then
      val_undetermined
    else if (Int64.equal (rem ia1 ib1) (rem ia2 ib2)) then
      v2
    else
      val_undetermined
  | Congruence(ia1, ib1), Cst(i2) ->
    if Int64.equal (rem i2 ib1) (rem ia1 ib1) then Congruence(ia1, ib1)
    else val_undetermined
  | Cst(i1), Congruence(ia2, ib2) ->
    if Int64.equal (rem i1 ib2) (rem ia2 ib2) then Congruence(ia2, ib2)
    else val_undetermined

  let random_to_info _ _ = val_undetermined

  let is_lt info1 info2 = match info1, info2 with
  | Cst(i1), Cst(i2) ->
    (Int64.compare i1 i2) < 0
  | _, _ -> raise Cannot_simplify_in_domain

  let is_eq info1 info2 = match info1, info2 with
  | Cst(i1), Cst(i2) ->
    (Int64.compare i1 i2) = 0
  | _, _ -> raise Cannot_simplify_in_domain

  let binop_add_to_info info1 info2 = match info1, info2 with
  | Cst(i1), Cst(i2) ->
    Cst(Int64.add i1 i2)
  | Congruence(ia1, ib1), Congruence(ia2, ib2) ->
    if Int64.equal ib1 ib2 then
      Congruence(rem (Int64.add ia1 ia2) ib1, ib1)
    else val_undetermined
  | Congruence(ia1, ib1), Cst(i2) ->
    Congruence(rem (Int64.add ia1 i2) ib1, ib1)
  | Cst(i1), Congruence(ia2, ib2) ->
    Congruence(rem (Int64.add i1 ia2) ib2, ib2)

  let binop_sub_to_info info1 info2 = match info1, info2 with
  | Cst(i1), Cst(i2) ->
    Cst(Int64.sub i1 i2)
  | Congruence(ia1, ib1), Congruence(ia2, ib2) ->
    if Int64.equal ib1 ib2 then
      Congruence(rem (Int64.sub ia1 ia2) ib1, ib1)
    else val_undetermined
  | Congruence(ia1, ib1), Cst(i2) ->
    Congruence(rem (Int64.sub ia1 i2) ib1, ib1)
  | Cst(i1), Congruence(ia2, ib2) ->
    Congruence(rem (Int64.sub i1 ia2) ib2, ib2)

  let binop_mul_to_info info1 info2 = match info1, info2 with
  | Cst(i1), Cst(i2) ->
    Cst(Int64.mul i1 i2)
  | Congruence(ia1, ib1), Congruence(ia2, ib2) ->
    if Int64.equal ib1 ib2 then
      Congruence(rem (Int64.mul ia1 ia2) ib1, ib1)
    else val_undetermined
  | Congruence(ia1, ib1), Cst(i2) ->
    Congruence(rem (Int64.mul ia1 i2) ib1, (Int64.abs (Int64.mul i2 ib1)))
  | Cst(i1), Congruence(ia2, ib2) ->
    Congruence(rem (Int64.mul ia2 i1) ib2, (Int64.abs (Int64.mul i1 ib2)))

  let binop_div_to_info info1 info2 = match info1, info2 with
  | _, Cst(i2) when Int64.equal Int64.zero i2 ->
    val_undetermined
  | Cst(i1), Cst(i2) ->
    Cst(Int64.div i1 i2)
  | Congruence(ia1, ib1), Congruence(ia2, ib2) ->
    val_undetermined
  | Congruence(ia1, ib1), Cst(i2) ->
    if Int64.equal Int64.zero (rem ia1 i2) && Int64.equal Int64.zero (rem ib1 i2) then
      Congruence(Int64.div ia1 i2, Int64.div ib1 i2)
    else
      val_undetermined
  | Cst(i1), Congruence(ia2, ib2) ->
    val_undetermined

  let is_unchanged info1 info2 = match info1, info2 with
  | Cst(i), Cst(j) -> Int64.equal i j
  | Congruence(ia1, ib1), Congruence(ia2, ib2) ->
    if (Int64.equal ib1 ib2) then
      Int64.equal (rem ia1 ib1) (rem ia2 ib2)
    else
      false
  | _, _ -> false

  let info_to_expr = function
  | Cst(i) -> Se_const(Sc_int i)
  | _ -> raise Cannot_simplify_in_domain

  let get_exact_info lst =
    let rec aux acc lst = match acc, lst with
    | _, [] -> Some(acc)
    | Cst(i), (Cst(j))::t when Int64.equal i j ->
      aux (Cst(i)) t
    | _, _ -> None in
    match lst with
    | (Cst(i))::t -> aux (Cst(i)) t
    | _ -> None

  let merge_congruence_infos info1 info2 = match info1, info2 with
  | Cst(i1), Cst(i2) when Int64.equal i1 i2 -> Cst(i1)
  | Cst(i1), Cst(i2) ->
    let newmod = Int64.abs (Int64.sub i1 i2) in
    Congruence(rem i1 newmod, newmod)
  | Congruence(ia1, ib1), Cst(i2) ->
    if Int64.equal (rem ia1 ib1) (rem i2 ib1) then info1
    else val_undetermined
  | Cst(i1), Congruence(ia2, ib2) ->
    if Int64.equal (rem i1 ib2) (rem ia2 ib2) then info2
    else val_undetermined
  | Congruence(ia1, ib1), Congruence(ia2, ib2) ->
    if Int64.equal ib1 ib2 && Int64.equal (rem ia1 ib1) (rem ia2 ib2) then
      info1
    else
      val_undetermined

  let extend_info lst =
    match get_exact_info lst with
    | Some(i) -> i
    | None ->
      begin
        match lst with
        | [] -> val_undetermined
        | h::t ->
          List.fold_left merge_congruence_infos h t
      end

end
