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
