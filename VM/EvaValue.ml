(*
    We need an data structure to abstract values in both
    Instruction stack and value stack
*)

type evaValue = NULL | Number of float

(** Constructor for NUMBER *)
let number_ value = Number value

let _number = function
    | Number value -> value
    | _ -> failwith "not a number"
