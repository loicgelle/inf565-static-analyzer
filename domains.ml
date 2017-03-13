open Simple_java_syntax

module type DomainType = sig
  type info_type
  type spec_type =
  | RangeLength of int64
  | NoSpec

  exception Cannot_simplify_in_domain

  val val_undetermined: info_type
  val merge_infos: info_type -> info_type -> info_type
  val const_to_info: int64 -> info_type
  val random_to_info: int64 -> int64 -> info_type
  val str_of_state: info_type -> string
  val is_unchanged: info_type -> info_type -> bool
  val info_to_expr: info_type -> s_expr
  val is_lt: info_type -> info_type -> bool
  val is_eq: spec_type -> info_type -> info_type -> bool
  val binop_add_to_info: info_type -> info_type -> info_type
  val binop_sub_to_info: info_type -> info_type -> info_type
  val binop_mul_to_info: info_type -> info_type -> info_type
  val binop_div_to_info: info_type -> info_type -> info_type
  val extend_info: bool -> info_type list -> info_type
  val reduce_states_eq: info_type -> info_type -> info_type
  val reduce_states_neq: info_type -> info_type -> info_type
  val reduce_states_lt: info_type -> info_type -> info_type
  val reduce_states_gte: info_type -> info_type -> info_type
  val count_possible: info_type -> int64
end
