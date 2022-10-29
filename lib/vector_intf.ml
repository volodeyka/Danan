module type VECTOR = sig

  type letter

  type t
  type word     = letter list
  type alphabet = (int, letter) Hashtbl.t

  val to_letter      : alphabet -> t -> letter
  val of_word        : word -> t
  val delete         : letter -> t -> t

end
