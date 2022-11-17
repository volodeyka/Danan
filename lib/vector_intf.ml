module type VECTOR = sig

  type letter

  type t
  type word     = letter list
  type alphabet = (int, letter) Hashtbl.t
  val compare        : t -> t -> int
  val equal          : t -> t -> bool
  val increment      : letter -> t -> t
  val to_string      : t -> string
  val empty          : t
  val to_letter      : alphabet -> t -> letter
  val of_word        : word -> t
  val delete         : letter -> t -> t
  val union          : t list -> t

end
