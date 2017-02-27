open Simple_java_syntax

module IntervalsType : Domains.DomainType = struct
  exception Cannot_simplify_in_domain

  type int_info =
  | MInfty (* - infinity *)
  | PInfty (* + infinity *)
  | Int of int64
  type info_type = int_info * int_info

  let val_undetermined = MInfty, PInfty

  let str_of_state =
    let str_of_int_state = function
    | Int i -> Int64.to_string i
    | MInfty -> "-infty"
    | PInfty -> "+infty" in
    function
    | i_info1, i_info2 ->
      "[" ^ (str_of_int_state i_info1) ^ ", " ^ (str_of_int_state i_info2) ^ "]"

  let const_to_info i = Int(i), Int(i)

  let random_to_info i1 i2 = Int(i1), Int(i2)

  let int_infos_add b_right info1 info2 = match info1, info2 with
  | MInfty, PInfty | PInfty, MInfty -> if b_right then PInfty else MInfty
  | PInfty, _ | _, PInfty -> PInfty
  | MInfty, _ | _, MInfty -> MInfty
  | Int i1, Int i2 -> Int (Int64.add i1 i2)

  let int_infos_sub b_right info1 info2 = match info1, info2 with
  | MInfty, PInfty -> MInfty
  | PInfty, MInfty -> PInfty
  | MInfty, MInfty | PInfty, PInfty -> if b_right then PInfty else MInfty
  | PInfty, _ | _, MInfty -> PInfty
  | MInfty, _ | _, PInfty -> MInfty
  | Int i1, Int i2 -> Int (Int64.sub i1 i2)

  let int_infos_mul info1 info2 = match info1, info2 with
  | MInfty, PInfty | PInfty, MInfty -> MInfty
  | MInfty, MInfty | PInfty, PInfty -> PInfty
  | PInfty, Int i | Int i, PInfty ->
    let sign = Int64.compare i Int64.zero in
    if sign = 0 then Int Int64.zero
    else if sign > 0 then PInfty
    else MInfty
  | MInfty, Int i | Int i, MInfty ->
    let sign = Int64.compare i Int64.zero in
    if sign = 0 then Int Int64.zero
    else if sign < 0 then PInfty
    else MInfty
  | Int i1, Int i2 -> Int (Int64.mul i1 i2)

  let int_infos_div b_right info1 info2 = match info1, info2 with
  | PInfty, PInfty | MInfty, MInfty -> PInfty
  | MInfty, PInfty | PInfty, MInfty -> MInfty
  | PInfty, Int(i) ->
    let sign = Int64.compare i Int64.zero in
    if sign > 0 then PInfty else MInfty
  | MInfty, Int(i) ->
    let sign = Int64.compare i Int64.zero in
    if sign < 0 then PInfty else MInfty
  | Int(_), MInfty -> Int(Int64.zero)
  | Int(_), PInfty -> Int(Int64.zero)
  | Int(i1), Int(i2) when not b_right -> Int(Int64.div i1 i2)
  | Int(i1), Int(i2) ->
    let rem = Int64.rem i1 i2 in
    let sign = Int64.compare rem Int64.zero in
    if sign = 0 then Int(Int64.div i1 i2)
    else Int(Int64.add (Int64.div i1 i2) Int64.one)


  let int_infos_min i1 i2 = match i1, i2 with
  | MInfty, _ | _, MInfty -> MInfty
  | PInfty, i | i, PInfty -> i
  | Int(i1), Int(i2) ->
    let sign = Int64.compare i1 i2 in
    if sign > 0 then Int i2 else Int i1

  let int_infos_max i1 i2 = match i1, i2 with
  | MInfty, i | i, MInfty -> i
  | PInfty, _ | _, PInfty -> PInfty
  | Int(i1), Int(i2) ->
    let sign = Int64.compare i1 i2 in
    if sign > 0 then Int i1 else Int i2

  let int_infos_min_max lst =
    let rec aux mi ma = function
    | [] -> mi, ma
    | h::t -> aux (int_infos_min mi h) (int_infos_max ma h) t
    in aux PInfty MInfty lst

  let int_infos_min_lst lst =
    let rec aux mi = function
    | [] -> mi
    | h::t -> aux (int_infos_min mi h) t
    in aux PInfty lst

  let int_infos_max_lst lst =
    let rec aux ma = function
    | [] -> ma
    | h::t -> aux (int_infos_max ma h) t
    in aux MInfty lst

  (* Compares two values, strictly or not *)
  let int_infos_lt b_strict a b = match a, b with
  | MInfty, PInfty -> true
  | MInfty, Int _ -> true
  | Int _, PInfty -> true
  | Int(ia), Int(ib) ->
    let sign = Int64.compare ia ib in
    if sign < 0 then true
    else if (sign = 0 && not b_strict) then true
    else false
  | _, _ -> false

  let int_infos_contains_zero a b =
    if int_infos_lt false a (Int(Int64.zero)) && int_infos_lt false (Int(Int64.zero)) b then true
    else false

  let is_lt info1 info2 =
    let (i11, i12) = info1 in
    let (i21, i22) = info2 in
    if int_infos_lt true i12 i21 then true
    else if int_infos_lt false i22 i11 then false
    else raise Cannot_simplify_in_domain

  let binop_add_to_info info1 info2 =
    let (i11, i12) = info1 in
    let (i21, i22) = info2 in
    int_infos_add false i11 i21, int_infos_add true i12 i22

  let binop_sub_to_info info1 info2 =
    let (i11, i12) = info1 in
    let (i21, i22) = info2 in
    int_infos_sub false i11 i21, int_infos_sub true i12 i22

  let binop_mul_to_info info1 info2 =
    let (i11, i12) = info1 in
    let (i21, i22) = info2 in
    let lst =
      [int_infos_mul i11 i21;
      int_infos_mul i12 i21;
      int_infos_mul i11 i22;
      int_infos_mul i12 i22] in
    let (mi, ma) = int_infos_min_max lst in
    mi, ma

  let binop_div_to_info info1 info2 =
    let (i11, i12) = info1 in
    let (i21, i22) = info2 in
    if int_infos_contains_zero i21 i22 then MInfty, PInfty
    else
      let lst1 =
        [int_infos_div false i11 i22;
        int_infos_div false i12 i22;
        int_infos_div false i11 i21;
        int_infos_div false i12 i21] in
      let lst2 =
        [int_infos_div true i11 i22;
        int_infos_div true i12 i22;
        int_infos_div true i11 i21;
        int_infos_div true i12 i21] in
      let mi = int_infos_min_lst lst1 in
      let ma = int_infos_max_lst lst2 in
      mi, ma

  let interval_union i11 i12 i21 i22 =
    int_infos_min i11 i21, int_infos_max i12 i22

  let merge_infos v1 v2 = match v1, v2 with
    | (i11, i12), (i21, i22) -> interval_union i11 i12 i21 i22

  let is_unchanged info1 info2 = match info1, info2 with
  | (int11, int12), (int21, int22) ->
    begin
      match int11, int12, int21, int22 with
      | Int i11, Int i12, Int i21, Int i22
        when Int64.equal i11 i12 && Int64.equal i21 i12 && Int64.equal i21 i22 -> true
      | _, _, _, _ -> false
    end

  let info_to_expr = function
  | int1, int2 ->
    begin
      match int1, int2 with
      | Int i1, Int i2 when Int64.equal i1 i2 -> Se_const(Sc_int i1)
      | _, _ -> raise Cannot_simplify_in_domain
    end

end
