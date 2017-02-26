open Simple_java_syntax

module type DomainType = sig
  type info_type

  exception Cannot_simplify

  val val_undetermined: info_type
  val merge_infos: info_type -> info_type -> info_type
  val const_to_info: s_constant -> info_type
  val random_to_info: int64 -> int64 -> info_type
  val unop_to_info: info_type -> info_type
  val binop_to_info: s_binary_op -> info_type -> info_type -> info_type
  val is_true: info_type -> bool
  val is_false: info_type -> bool
  val str_of_state: info_type -> string
  val is_unchanged: info_type -> info_type -> bool
  val info_to_expr: info_type -> s_expr
end

module ConstantsType : DomainType = struct
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

module IntervalsType : DomainType = struct
  exception Cannot_simplify
  type int_info =
  | MInfty (* - infinity *)
  | PInfty (* + infinity *)
  | Int of int64
  type info_type =
  | Boolean of bool
  | Interval of int_info * int_info
  | Undetermined

  let val_undetermined = Undetermined

  let str_of_state =
    let str_of_int_state = function
    | Int i -> Int64.to_string i
    | MInfty -> "-infty"
    | PInfty -> "+infty" in
    function
    | Boolean b ->
      string_of_bool b
    | Interval(i_info1, i_info2) ->
      "[" ^ (str_of_int_state i_info1) ^ ", " ^ (str_of_int_state i_info2) ^ "]"
    | Undetermined -> "undetermined"

  let const_to_info = function
  | Sc_int i -> Interval(Int(i), Int(i))
  | Sc_bool true -> Boolean(true)
  | Sc_bool false -> Boolean(false)

  let random_to_info i1 i2 = Interval(Int(i1), Int(i2))

  let unop_to_info = function
  | Boolean b -> Boolean(not b)
  | _ -> Undetermined

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

  let ints_from_info = function
  | Interval(i1, i2) -> i1, i2
  | Undetermined -> MInfty, PInfty
  | _ -> failwith "argument should be integer information"

  let int_infos_contains_zero a b =
    if int_infos_lt false a (Int(Int64.zero)) && int_infos_lt false (Int(Int64.zero)) b then true
    else false

  let binop_to_info op info1 info2 = match op with
  | Sb_or ->
    begin
      match info1, info2 with
      | Boolean b1, Boolean b2 -> Boolean (b1 || b2)
      | _, _ -> Undetermined
    end
  | _ ->
    begin
      let (i11, i12) = ints_from_info info1 in
      let (i21, i22) = ints_from_info info2 in
      match op with
      | Sb_add -> Interval(int_infos_add false i11 i21, int_infos_add true i12 i22)
      | Sb_sub -> Interval(int_infos_sub false i11 i21, int_infos_sub true i12 i22)
      | Sb_mul ->
        let lst =
          [int_infos_mul i11 i21;
          int_infos_mul i12 i21;
          int_infos_mul i11 i22;
          int_infos_mul i12 i22] in
        let (mi, ma) = int_infos_min_max lst in
        Interval(mi, ma)
      | Sb_div ->
        if int_infos_contains_zero i21 i22 then Interval(MInfty, PInfty)
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
          Interval(mi, ma)
      | Sb_lt ->
        if int_infos_lt true i12 i21 then Boolean(true)
        else if int_infos_lt false i22 i11 then Boolean(false)
        else Undetermined
      | _ -> Interval(MInfty, PInfty)
    end

  let interval_union i11 i12 i21 i22 =
    Interval(int_infos_min i11 i21, int_infos_max i12 i22)

  let merge_infos v1 v2 = match v1, v2 with
    | Interval (i11, i12), Interval (i21, i22) -> interval_union i11 i12 i21 i22
    | Boolean b1, Boolean b2 when b1 = b2 -> Boolean b1
    | _, _ -> Undetermined

  let is_true = function
  | Boolean(true) -> true
  | _ -> false

  let is_false = function
  | Boolean(false) -> true
  | _ -> false

  let is_unchanged info1 info2 = match info1, info2 with
  | Boolean b1, Boolean b2 when b1 = b2 -> true
  | Interval(int11, int12), Interval(int21, int22) ->
    begin
      match int11, int12, int21, int22 with
      | Int i11, Int i12, Int i21, Int i22
        when Int64.equal i11 i12 && Int64.equal i21 i12 && Int64.equal i21 i22 -> true
      | _, _, _, _ -> false
    end
  | _, _ -> false

  let info_to_expr = function
  | Boolean b -> Se_const(Sc_bool b)
  | Interval(int1, int2) ->
    begin
      match int1, int2 with
      | Int i1, Int i2 when Int64.equal i1 i2 -> Se_const(Sc_int i1)
      | _, _ -> raise Cannot_simplify
    end
  | _ -> raise Cannot_simplify

end
