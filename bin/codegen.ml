open Llvm_analysis
open Containers

module Codegen = struct
type name = Name of string | Not_Name of int
type 'a named = Bind of string * 'a | Do of 'a
type state = { next : int }
type symbol_table = string * Llvm.llvalue
type 'a instruction = 'a named list
type 'a terminator = 'a named
type 'a block_state
  = {
    idx   : int                             (* Block index *)
  ; stack : 'a instruction             (* Stack of instructions *)
  ; term  : 'a terminator option        (* Block terminator *)
  }
    [@@deriving show]
module  BlockState = struct
    type t = name

    let compare v v1 =          (*  TODO *)
      match v , v1 with
      | Name vv,Name vv1 ->
      String.compare vv vv1
      | Not_Name vv,Not_Name vv1 ->
      Int.compare vv vv1
      | _ ,_ -> -1
end

module BlockStateMap = CCMap.Make(BlockState)

module  Names = struct
    type t = string
    let compare v v1 =          (*  TODO *)
      String.compare v v1
end

module NamesMap = CCMap.Make(Names)

type 'a codegen=
    {
    currentBlock : name                      (* Name of the active block to append to *)
  ; blocks       : 'a block_state BlockStateMap.t   (* Blocks for function *)
  ; symtab       : symbol_table               (* Function scope symbol table *)
  ; blockcount   : int                       (* Count of basic blocks *)
  ; count        : int64 (* Count of unnamed instructions *)
  ; names        : int NamesMap.t (* Name Supply *)
}
    [@@deriving show]
end
