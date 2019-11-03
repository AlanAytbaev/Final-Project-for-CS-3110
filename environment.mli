(** [Environment_sig] is a module meant to define environment of the calculator.
    This module contains the array of local memory bindings which are the result of
    the calculator parsing statement(s) *)

module type Environment_sig = sig

  (** type v is the type of the value stored in memory. *)
  type v

  (** type k is the type of the key in the memory_bindings. *)
  type k

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
  val contains : k -> t -> bool

  (** [add_binding s t] adds a new binding s with value t to memory_bindings.
      Returns the new list. *)
  val add_binding : k -> v -> t -> t

  (** [remove_binding s] removes the binding in memory_bindings that
      corresponds with key. returns new_list*)
  val remove_binding : k -> t -> t

  (** [get_val key] returns the value that is bound in memory_bindings
      that corresponds to key. *)
  val get_val : k -> t -> v

  (** [member key] is true if key corresponds to a key in memory_bindings
      otherwise is false. *)
  val member : k -> t -> bool

end
module Environment : Environment_sig