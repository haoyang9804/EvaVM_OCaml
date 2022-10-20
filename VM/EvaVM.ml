open EvaValue
open Bytecode.OpCode

module type Stack = sig
  (** Module type for Value Stack and Instruction Stack *)
  type t 
  type tlist = t list
  val empty : tlist
  val push : t -> tlist -> tlist
  val pop : tlist -> tlist
  val peek : tlist -> t
  val peek_pop : tlist -> t * tlist
end

module PlainStack = struct
  let empty = []
  let push ele stack = ele :: stack
  exception EmptyStack

  let pop = function _ :: t -> t | _ -> raise EmptyStack
  let peek = function h :: _ -> h | _ -> raise EmptyStack
  let peek_pop stack = (peek stack, pop stack) 
end

(** ValueStack is a stack for values that are directly passed or indirectly calculated*)
module ValueStack : Stack = struct

  type t = evaValue
  type tlist = evaValue list
  (* let empty = [] *)
include PlainStack
end

(** InstructionStack is a stack of low-level instructions*)
module InstructionStack : Stack  = struct
  type t = op
  type tlist = op list
include PlainStack
end

