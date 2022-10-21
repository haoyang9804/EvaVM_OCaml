open VM.EvaVM
open VM.EvaValue
open Bytecode.OpCode

let () = 
instructions := OP_CONST :: Int 0 :: OP_HALT :: !instructions;
!constantPool.(0) <- (number_ 42.);
let value = eval !instructions in print_float (_number value)
