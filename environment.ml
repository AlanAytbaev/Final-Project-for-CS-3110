** [Environment] is a module meant to define environment of the calculator.
 This module contains the array of local memory bindings which are the result of
 the calculator parsing statement(s)*)
module Environment : Environment_sig = struct

        (** type v is the type of the value stored in memory. *)
        type v = string

        (** type k is the type of the key in the memory_bindings. *)
        type k = float

        (** type t is the type of the memory_bindings. *)
        type t = (k * v) list

        (** [empty] is the empty memory_bindings. *)
        val empty = []

        (** [is_empty mem_binds] is true if memory_bindings is empty
            otherwise is false. *)
        let is_empty (mb : t) =
                match mb with
                | empty -> true
                | _ -> false

        (**[memory_bindings] is an association list that maps variable names
            to values. It is used to store local variables. *)
        val memory_bindings = empty

        (** [contains k t] is true if k is in t. Otherwise is false. *)
        val contains (key : k) (mb : t) =
                List.mem_assoc k mb

        (** [add_binding s t] adds a new binding s with value t to memory_bindings.
            Returns the value t. *)
        let add_binding (key : k) (val : v) (mb : t) =
                match (List.mem_assoc k mb) with
                | false -> (k, v) :: mb
                | true -> let nlist = (List.remove_assoc k mb) in
                  (k, v) :: nlist

        (** [remove_binding s] removes the binding in memory_bindings that
            corresponds with key. Is true if key was in memory_bindings,
            otherwise is false. *)
        val remove_binding (key : k) (mb : t) =
                List.remove_assoc k mb

        (** [get_val key] returns the value that is bound in memory_bindings
            that corresponds to key. *)
        val get_val (key : k) (mb : t) =
                List.assoc_opt k mb

        (** [member key] is true if key corresponds to a key in memory_bindings
            otherwise is false. *)
        val member (key : k) (mb : t) =
                (List.mem_assoc k d)
end
