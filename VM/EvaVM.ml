open EvaValue

module type Stack = sig
  (** Module type for Value Stack and Instruction Stack *)
  type t 
  type tlist = t list
  val empty : tlist
  val push : t -> tlist -> tlist
  val pop : tlist -> tlist
  val 
end

(** ValueStack is a stack for values that are directly passed or indirectly calculated*)
module ValueStack = struct
  type 'a t = 'a list

  let empty = []
  let push ele stack = ele :: stack

  exception EmptyStack

  let pop = function _ :: t -> t | _ -> raise EmptyStack
  let peek = function h :: _ -> h | _ -> raise EmptyStack
  let peek_pop stack = (peek stack, pop stack)
end

(** InstructionStack is a stack of low-level instructions*)
module InstructionStack = struct
  type 'a t = 'a list
end
