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
  val generate : expr -> instr list -> instr list

  val emit_instr: instr -> instr list -> instr list
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


module Value = struct



  type value =
    | VInt of int
    | BlackHole

[@@deriving show]
 type eval_error = WrongType of value *  string
 exception EvalException of eval_error
 let asInt = function
        | VInt i->  i
        | other -> raise( EvalException(WrongType( other, "int" )) )
end

module type Operation =
sig
  val execute : instr list -> unit
  val current_value : Value.value option
end
module VM   = struct
  let bc_stack : Value.value Stack.t = Stack.create()
  let instructions = []

  module VMOperation
                     ( Byte : ByteCodeGen )
                     ( Oper : Operation)= struct

    let eval expr =
      let instr_list = Byte.generate expr instructions in
      (* List.iter( fun instr -> Printf.printf "%s\n"  (Format.asprintf "%a" pp_instr instr )) instr_list; *)

      Oper.execute instr_list;
      Printf.printf "%s" (Format.asprintf "%a\n" Value.pp_value
                            (Stack.top bc_stack));

  end


module VMOp =
VMOperation(struct

  let  emit_instr instr instructions : instr list=
     (* instructions @ [(Format.asprintf "%a" pp_instr (instr))] *)

     instructions @ [instr]


  let rec generate expr instructions =
    (*Unsure how the missing cases are handled*)
    match expr with
       | Lit (BInt i) -> emit_instr ( IntConst i ) instructions
       | Builtin ( Arithmetic( fn, opA, opB ) ) ->
           let instructions = (generate opA instructions) in
           let instructions = (generate opB instructions) in
           let op_type = Op.fromArithmeticFn fn in
           emit_instr  op_type instructions
       | Builtin ( Comparison( fn, lhs, rhs) ) ->
           let instructions = (generate lhs instructions) in
           let instructions =(generate rhs instructions) in
           let op_type = Op.fromComparisonFn fn in
           emit_instr op_type instructions
       | Builtin ( UnaryArithmetic( fn, expr) ) ->
           let instructions = (generate expr instructions) in
           match fn with
           | Neg -> emit_instr (IntUnaryOp Neg) instructions
       |  _ -> failwith "TO Investigate"


     end)
  (struct
  let execute instructions : unit=
    let halt = ref false in
    let rec loop cp =
      if (cp < (List.length instructions) && (!halt <> true)) then
        let instr = List.nth instructions cp in
        (* Printf.printf "Pointer is %d Length of instructions %d \n" cp (List.length instructions); *)
        (* List.iter( fun instr -> Printf.printf "Instruction %s\n"  (Format.asprintf "%a" pp_instr instr )) instructions; *)

        match instr with
        | Halt -> halt := true
        | IntConst i -> Stack.push (Value.VInt i) bc_stack;
                        loop (cp + 1);
        | IntBinaryOp op ->
          let arg2  = Stack.pop bc_stack |> Value.asInt in
          let arg1 = Stack.pop bc_stack |> Value.asInt in
          let result =
          (match op with
          | Add -> arg1 + arg2
          | Sub -> arg1 - arg2
          | Div -> arg1 / arg2
          | Mul -> arg1 * arg2
          | Less -> if arg1 < arg2 then 1 else 0
          | Greater -> if arg1 > arg2 then 1 else 0
          | Equal -> if arg1 == arg2 then 1 else 0)
          in
          Stack.push  (Value.VInt result) bc_stack;
          loop (cp + 1);
        | IntUnaryOp op ->
          let arg = Stack.pop bc_stack |> Value.asInt in
          Printf.printf "Popping %d\n" arg;
          let result =
          (match op with
            | Neg -> -arg ) in
          Stack.push  (Value.VInt result) bc_stack;
          loop (cp + 1);

    in
    loop 0

  let current_value =
    match (Stack.is_empty bc_stack) with
    |  false ->  Some (Stack.top bc_stack);
    |  true -> None
   end)

end
