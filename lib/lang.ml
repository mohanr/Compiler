type arithmetic_fn = | Add | Sub | Mul | Div
[@@deriving show]

module type ARITHMETIC_FN=
sig
  type t = arithmetic_fn
  [@@deriving show]
  type a = int
  type b = int
  val apply : t -> a -> b -> a
end

type unary_fn = Neg
  [@@deriving show]

type comparison_fn =
    | Less
    | Equal
    | Greater
[@@deriving show]

module type COMPARISON_FN=
sig
  type t = comparison_fn
  [@@deriving show]
  type a = int
  type b = int
  val apply : t -> a -> b -> a
end


module  Lang(ArithmeticType : ARITHMETIC_FN)
            (ComparisonType : COMPARISON_FN) = struct

  module ArithmeticType = ArithmeticType
  module ComparisonType = ComparisonType
  type var_name = string
  [@@deriving show]
  type btype =  int
  [@@deriving show]


exception Type_error


type builtin_fn =
    |Arithmetic of  ArithmeticType.t *  expr *  expr
    |Comparison of  ComparisonType.t *  expr *  expr
    |UnaryArithmetic of unary_fn * expr
[@@deriving show]
and
expr =
    | Var of var_name
    | Abs of  var_name *  expr
    | App of expr *  expr
    | Lit of btype
    | Builtin of builtin_fn
    | Cond of expr *  expr *  expr
[@@deriving show]

type eval_error = WrongType of expr *  string

exception EvalException of eval_error

end

module Language =
  Lang(struct
    type t = arithmetic_fn
    [@@deriving show]
    type a = int
    type b = int
    let apply fn a b : int =
       match fn with
         | Add -> a + b
         | Sub -> a - b
         | Mul -> a * b
         | Div -> a / b
     end)
  (struct
    type t = comparison_fn
    [@@deriving show]
    type a = int
    type b = int
    let apply fn a b : int =
       match fn with
         | Less -> if a < b then 1 else 0
         | Greater ->  if a > b then 1 else 0
         | Equal ->  if a = b then 1 else 0
     end)


module Expr = struct
include Language

    let asInt = function
        | Lit btype  -> btype
        | other -> raise( EvalException(WrongType( other, "int" )) )
    let asAbs = function
        | Abs (var,  body ) -> var, body
        | other -> raise( EvalException(WrongType( other, "lambda" )) )


    let rec subst (replaceable  : var_name ) (replaceWith : expr ) (expr : expr) =
       let substFn = subst replaceable replaceWith in
       match expr with
        | Builtin( UnaryArithmetic (_, expr ))-> expr (* This isn't used TODO *)
        | Lit _ -> expr
        | Builtin ( Arithmetic( fn, opA, opB ) ) ->
             Builtin ( Arithmetic( fn, substFn opA, substFn opB ) )
        | Builtin ( Comparison( fn, opA, opB ) ) ->
             Builtin ( Comparison( fn,  substFn opA,  substFn opB ) )
        | Cond (pred, trueBranch, falseBranch) ->
             Cond ( substFn pred, substFn trueBranch, substFn falseBranch)
        | App ( expr, arg ) ->  App( substFn expr , substFn arg )
        | Var boundName -> if boundName = replaceable then replaceWith else expr
        | Abs (boundName, body ) -> if boundName = replaceable then expr else
                                     Abs( boundName, substFn body )


    and  eval( expr : expr ) : expr =
     match expr with
        | Builtin( UnaryArithmetic (_, expr ))-> expr (* This isn't used TODO *)
        | Lit _ -> expr
        | Builtin ( Arithmetic( fn, opA, opB ) ) ->
            let valA = eval opA  |> asInt in
            let valB = eval opB  |> asInt in
            Lit  (  ArithmeticType.apply fn valA valB)
        | Builtin ( Comparison( fn, opA, opB ) ) ->
            let lhs = eval opA  |> asInt in
            let rhs = eval opB  |> asInt in
            Lit (ComparisonType.apply fn lhs rhs  )
        | Cond (pred, trueBranch, falseBranch) ->
            let valPred = eval pred |> asInt in
            if valPred <> 0 then eval trueBranch else eval falseBranch
        | Abs _ -> expr

        | App( expr, arg ) ->
            let lambdaVar, lambdaBody = eval expr |> asAbs in
            subst lambdaVar arg lambdaBody |> eval

        | Var _ -> failwith  "Wrong evaluation "

end
