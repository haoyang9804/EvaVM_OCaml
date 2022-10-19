(*
    We need an data structure to abstract values in both
    Instruction stack and value stack
*)

type evaValue = Number of float

(** Constructor for NUMBER *)
let number_ value = Number value
