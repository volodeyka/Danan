open Vector_intf

module type SPEC = sig
  type letter 
  val conc_rel : letter -> letter -> bool
  val alpha : int

end

module type PID = sig

  module Spec : SPEC
  module Vec  : (VECTOR with type letter := Spec.letter)

  type pids = Vec.t list

  val of_word : Vec.word -> pids

end