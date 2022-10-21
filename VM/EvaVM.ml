open EvaValue
open Bytecode.OpCode

let stack_limit = 1024
let constPool_limit = 1024
let _dynamic_stack_limit = ref 64
let _dynamic_constPool_limit = ref 64
let constantPool = ref (Array.make !_dynamic_constPool_limit NULL)
let instructions : op list ref = ref []
let stack = ref (Array.make !_dynamic_stack_limit NULL)

(** stack pointer sp*)
let sp = ref 0

exception Lessthan0
let popStack () = 
  if !sp < 0 then raise Lessthan0
  else sp := !sp - 1; !stack.(!sp)

exception STACK_OVERFLOW
let pushStack value =
  if !sp > stack_limit then raise STACK_OVERFLOW
  else
    if !sp > !_dynamic_stack_limit then
      let extraStack = Array.make !_dynamic_stack_limit NULL in
      stack := Array.append !stack extraStack;
      _dynamic_stack_limit := 2 * !_dynamic_stack_limit;
      !stack.(!sp) <- value;
      sp := !sp + 1
    else
      !stack.(!sp) <- value;
      sp := !sp + 1

exception UnexpectedOP
exception Error_OP_CONST
exception Empty_Instruction

let rec eval _instructions =
  match List.hd _instructions with
  | OP_HALT ->
    popStack () 
  | OP_CONST -> 
    (match _instructions with 
      | OP_CONST :: (Int constIndex :: tail) -> 
        pushStack !constantPool.(constIndex);
        eval tail
      | _ -> failwith "Incorrect subsequent op after OP_CONST")
  | _ -> raise UnexpectedOP
    
    
     
