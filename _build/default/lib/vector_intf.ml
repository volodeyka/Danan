module type VECTOR = sig

  type letter
  module AMap  : (Map.S with type key = letter)

  type t = int AMap.t
  type alphabet
  type word = letter list
  val to_letter : alphabet -> t -> letter
  val repr : word -> t
end