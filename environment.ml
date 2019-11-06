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

  (** [to_list t] is the string list representation of the association 
       list [t] *)
  val to_list : t -> string 

  (** [choose t] is the value of the first binding of the association list*)
  val choose : t -> string
end


(** [Environment] is a module meant to define environment of the calculator.
    This module contains the array of local memory bindings which are the result of
    the calculator parsing statement(s)*)
module Environment : Environment_sig = struct

  (** type t is the type of the memory_bindings. *)

  type t = (string * float) list


  let empty = []


  let is_empty mb = mb = empty


  let memory_bindings = empty


  let contains key mb =
    List.mem_assoc key mb

  (** [add_binding s t] adds a new binding s with value t to memory_bindings.
      Returns the value t. *)
  let add_binding k v mb =
    if (List.mem_assoc k mb)
    then begin let new_list = (List.remove_assoc k mb) in (k,v) :: new_list end
    else (k,v) :: mb

  let remove_binding key mb =
    List.remove_assoc key mb

  let get_val key mb =
    match (List.assoc_opt key mb) with
    |Some v -> v
    |None -> raise Not_found

  let member key mb =
    (List.mem_assoc key mb)


  (** [format_bindings b] returns a string with the bindings in b formatted
      to be printed to the user
  let format_bindings (b : t) =
    match b with
    | [] -> ""
    | hd::tl -> let (key, val) = hd in
      String.concat "" [key;(string_of_float val);] *)

  let rec to_list t =
    match t with 
    |[] -> "empty"
    |(k,v)::tail -> ("("^k^" = "^Float.to_string(v)^")/n" )^to_list tail

  let choose (t:(string * float) list) = 
    match t with 
    |[] -> raise Not_found
    |(k,v)::tail -> k

end

