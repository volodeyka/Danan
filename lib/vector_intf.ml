module type VECTOR = sig

  type letter

  module AMap  : (Map.S with type key = letter)

  type t        = int    AMap.t
  type word     = letter list
  type alphabet = (int, letter) Hashtbl.t

  val to_letter      : alphabet -> t -> letter
  val of_word        : word -> t

end
