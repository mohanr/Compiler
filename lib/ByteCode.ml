open Scanner__Lang1.Language

type int_binary_op =
  |Add|Sub|Mul|Div|Less|Greater|Equal
[@@deriving show]

type int_unary_op = Neg
[@@deriving show]

type instr = Halt | IntConst of int | IntBinaryOp of int_binary_op
           | IntUnaryOp of int_unary_op

[@@deriving show]

(* TODO Unused *)
module type IntBinaryOp = sig
  type t
  val fromArithmeticFn  : Scanner__Lang1.arithmetic_fn -> t
  val fromComparisonFn  : Scanner__Lang1.comparison_fn -> t
end

module type ByteCodeGen =
sig
  type bytecode
  val generate : expr -> unit

  val emit_instr: instr -> unit
end



module Op  = struct

let fromArithmeticFn (fn : Scanner__Lang1.arithmetic_fn) =
  match fn with
  | Add -> IntBinaryOp Add
  | Sub -> IntBinaryOp Sub
  | Div -> IntBinaryOp Div
  | Mul -> IntBinaryOp Mul

let fromComparisonFn (fn : Scanner__Lang1.comparison_fn) =
  match fn with
  | Less -> IntBinaryOp Less
  | Greater -> IntBinaryOp Greater
  | Equal -> IntBinaryOp Equal
end

module type Operation =
sig
  type value
  val execute : unit
  val current_value : value option
end

module VM   = struct
  let bc_stack  = Stack.create()

  module VMOperation
                     ( Byte : ByteCodeGen )= struct
  type value = VInt of int
  end


module VMOp =
VMOperation(struct
  type bytecode = instr array
  let buf = Buffer.create 16

  let emit_instr instr =
      Buffer.add_string buf (Format.asprintf "%a" pp_instr (instr))

  let rec generate expr =
    match expr with
       | Lit (BInt i) -> emit_instr ( IntConst i )
       | Builtin ( Arithmetic( fn, opA, opB ) ) ->
           ignore (generate opA);
           ignore (generate opB);
           let op_type = Op.fromArithmeticFn fn in
           emit_instr  op_type
       | Builtin ( Comparison( fn, lhs, rhs) ) ->
           ignore (generate lhs);
           ignore (generate rhs);
           let op_type = Op.fromComparisonFn fn in
           emit_instr op_type
       | Builtin ( UnaryArithmetic( fn, expr) ) ->
           ignore (generate expr);
           match fn with
           | Neg -> emit_instr (IntUnaryOp Neg)
       |  _ -> failwith "TO Investigate"


     end)

  let execute : unit = ()
  let current_value =
    match (Stack.is_empty bc_stack) with
    |  false ->  Some (VInt (Stack.top bc_stack));
    |  true -> None

end
