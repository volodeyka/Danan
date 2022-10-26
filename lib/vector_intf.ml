module type VECTOR = sig

  type letter
  type alphabet

  module AMap  : (Map.S with type key = letter)

  type t    = int    AMap.t
  type word = letter list
  type pids = t      list

  val to_letter      : alphabet -> t -> letter
  val repr           : word -> t
  val construct_pids : word -> pids
  
end
