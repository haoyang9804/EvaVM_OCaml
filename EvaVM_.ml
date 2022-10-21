open EvaValue
open Bytecode.OpCode

(** Mutable stack*)
module type MStack = sig
  type 'a node
  type 'a t
  val name : string ref
  (** string here is the name of this stack, this is used for bug trace*)
  val empty : string -> 'a t
  (** push a value onto the stack, enlarge the stack*)
  val push : 'a -> 'a t -> unit
  (** return the topest value on the stack, not change the stack*)
  val peek : 'a t -> 'a
  (** pop the topest value on the stack, shrink the stack*)
  val pop : 'a t-> unit
end

module MStack : MStack = struct
  type 'a node = {value : 'a; mutable next : 'a node option}
  type 'a t = {mutable top : 'a node option}
  let name = ref "stack"
  let empty n= name := n; {top = None}
  let push x s =
    s.top <- Some {value = x; next = s.top}
  exception EmptyStack of string
  let peek s = 
    match s.top with
    | None -> raise (EmptyStack !name)
    | Some {value} -> value
  let pop s =
    match s.top with
    | None -> raise (EmptyStack !name)
    | Some {value; next} -> s.top <- next
end

(* module MValueStack (M : MStack) = struct
  type node = {value : evaValue; mutable next : evaValue node option}
  type t = {mutable top : node option}

end *)

let _stack_limit = 512
let valueStack : evaValue MStack.t = MStack.empty "valueStack"
(* let valueStack : evaValue array = Array.create _stack_limit NULL *)

(** Stack pointer for valueStack*)
let sp = ref 0

let instructionStack : op MStack.t = MStack.empty "instructionStack"

exception NotInt of op
exception UnsupportedOP of string

(** Find the value in valueStack by index*)

let eval () =
  match MStack.peek instructionStack with
  | OP_HALT -> 
      sp := !sp - 1;
      let value = MStack.peek valueStack in
        MStack.pop valueStack; value
  | OP_CONST -> 
      MStack.pop instructionStack;
      let constant = MStack.peek instructionStack in
        match constant with
        | Int index ->
          MStack.pop instructionStack;
          MStack.push (number_ $ findValueById index)
        | _ -> raise (NotInt constant)
      ;
      eval ()
  | Int value -> raise (UnsupportedOP "Int")

(* module type Stack = sig
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
  let eval (iStack : tlist)=
  match iStack with
  | h :: t ->
    match h with
    | OP_HALT -> ()
    | OP_CONST -> 
  
end

(** A value stack*)
let valueStack = ref ValueStack.empty

(** An instruction stack*)
let instructionStack = ref InstructionStack.empty *)

