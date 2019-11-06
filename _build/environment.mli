(** [Environment_sig] is a module meant to define environment of the calculator.
    This module contains the array of local memory bindings which are the result of
    the calculator parsing statement(s) *)

module type Environment_sig = sig

  (** type t is the type of the memory_bindings. *)
  type t

  (** [empty] is the empty memory_bindings. *)
  val empty : t

  (** [is_empty mem_binds] is true if memory_bindings is empty
      otherwise is false. *)
  val is_empty: t -> bool

  (**[memory_bindings] is an association list that maps variable names
      to values. It is used to store local variables. *)
  val memory_bindings : t

  (** [contains k t] is true if k is in t. Otherwise is false. *)
  val contains : string -> t -> bool

  (** [add_binding s t] adds a new binding s with value t to memory_bindings.
      Returns the new list. *)
  val add_binding : string -> float -> t -> t

  (** [remove_binding s] removes the binding in memory_bindings that
      corresponds with key. returns new_list*)
  val remove_binding : string -> t -> t

  (** [get_val key] returns the value that is bound in memory_bindings
      that corresponds to key. *)
  val get_val : string -> t -> float

  (** [member key] is true if key corresponds to a key in memory_bindings
      otherwise is false. *)
  val member : string -> t -> bool

  (** [format_bindings b] returns a string with the bindings in b formatted
       to be printed to the user
  val format_bindings : t -> string *)
end
module Environment : Environment_sig
