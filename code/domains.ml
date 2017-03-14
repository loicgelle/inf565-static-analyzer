open Simple_java_syntax

module type DomainType = sig
  (* Abstract domain information type *)
  type info_type

  (* Optional specification to refine analysis *)
  (* Useful for coupled intervals / congruences analysis *)
  type spec_type =
  | RangeLength of int64
  | NoSpec

  (* Exception raised when answer is
   * - not certain (e.g. in case condition cannot be evaluated)
   * - not simplifiable
   * Locally caught in caller functions *)
  exception Cannot_simplify_in_domain

  val val_undetermined: info_type

  (* Deduce unique information from two informations *)
  val merge_infos: info_type -> info_type -> info_type

  (* Compute information associated to constants and random expressions *)
  val const_to_info: int64 -> info_type
  val random_to_info: int64 -> int64 -> info_type

  (* Converts state into debug information string *)
  val str_of_state: info_type -> string

  (* Determines if given informations are equivalent *)
  val is_unchanged: info_type -> info_type -> bool

  (* Turns information (e.g. constant) into expression when possible *)
  val info_to_expr: info_type -> s_expr

  (* Determines if the first information is less than the second one *)
  val is_lt: info_type -> info_type -> bool

  (* Determines if the first information is equal to the second one
   * Can make use of extra specification to refine analysis *)
  val is_eq: spec_type -> info_type -> info_type -> bool

  (* Deduce information after binary operations *)
  val binop_add_to_info: info_type -> info_type -> info_type
  val binop_sub_to_info: info_type -> info_type -> info_type
  val binop_mul_to_info: info_type -> info_type -> info_type
  val binop_div_to_info: info_type -> info_type -> info_type

  (* Returns correct overapproximation based on a list of states *)
  val extend_info: bool -> info_type list -> info_type

  (* Condition first information to the fact that it equals the second *)
  val reduce_states_eq: info_type -> info_type -> info_type

  (* Condition first information to the fact that it differs from the second *)
  val reduce_states_neq: info_type -> info_type -> info_type

  (* Condition first information to the fact that it is less than the second *)
  val reduce_states_lt: info_type -> info_type -> info_type

  (* Condition first information to the fact that it is greater than or equal to the second *)
  val reduce_states_gte: info_type -> info_type -> info_type

  (* Count possible constants that verify the information
   * raises Cannot_simplify_in_domain when infinite *)
  val count_possible: info_type -> int64
end
