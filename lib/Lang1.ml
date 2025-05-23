open Containers
open Stdlib
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
  type btype = BInt of int
  [@@deriving show]

  module EnvKey = struct
    type t = var_name
    let compare s s1 = if s < s1 then -1 else if s > s1 then 1 else 0
    (* String.compare *)
  end
exception Type_error

type recursive = bool
[@@deriving show]

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
    | Bind of recursive * var_name * expr * expr
[@@deriving show]

module PPMap =CCMap.Make(EnvKey)


  type value =
    | VInt of int
    | Closure of closure
    | BlackHole
  and closure = {mutable env : env ; var : var_name ; body : expr}
  and
  env =
  | EnvMap of value PPMap.t
      [@printer
        fun fmt map -> fprintf fmt "%a" (PPMap.pp CCString.pp pp_value) map]
[@@deriving show] (* only one call to `deriving show` is enough *)

type eval_error = WrongType of value *  string

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


module Value = struct
include Language

    let asInt = function
        | VInt i->  i
        | other -> raise( EvalException(WrongType( other, "int" )) )

    let asClosure = function
        | Closure c->  c
        | other -> raise( EvalException(WrongType( other, "closure" )) )


    let rec eval env  ( expr : expr ) : value =
     match expr with
        | Builtin( UnaryArithmetic (_, _))-> VInt 0 (* This isn't used TODO *)
        | Lit (BInt i) -> VInt i
        | Builtin ( Arithmetic( fn, opA, opB ) ) ->
            let valA = eval env opA  |> asInt in
            let valB = eval env  opB  |> asInt in
            VInt (ArithmeticType.apply fn valA valB )
        | Builtin ( Comparison( fn, opA, opB ) ) ->
            let lhs = eval env  opA  in
            (* Printf.printf "%s" (Format.asprintf "%a" pp_value lhs); *)

            let lh = lhs |> asInt in
            let rhs = eval env  opB  |> asInt in
            VInt( ComparisonType.apply fn lh rhs )
        | Cond (pred, trueBranch, falseBranch) ->
            let valPred = eval env  pred |> asInt in
            if valPred <> 0 then eval env  trueBranch else eval env  falseBranch
        | Abs ( var, body ) -> Closure { env = env; var = var; body = body }

        | App( expr, arg ) ->
            let { env = closureEnv ; var = closureVar; body = closureBody} = eval env  expr |> asClosure in
            let argValue = eval env arg in
            let EnvMap env_map = closureEnv in
            let map_env = EnvMap (PPMap.add closureVar argValue env_map) in
            (* Printf.printf "%s" (Format.asprintf "%a" pp_value argValue); *)

            eval  map_env closureBody
        | Bind( recursive,  var, body , expr) ->
          let bodyEnv =
            if not recursive then
              env
            else
              let EnvMap env_map = env in
              EnvMap( PPMap.add var BlackHole env_map ) in
          let bodyVal =
            (match eval bodyEnv body with
            |Closure closure as v ->
              let EnvMap env_map = env in
              let closureEnv = EnvMap (PPMap.add var v env_map) in
              closure.env <- closureEnv;
              v
            |other -> other) in
              let EnvMap env_map = env in
              let exprEnv = EnvMap( PPMap.add var bodyVal env_map) in
              eval exprEnv expr

        | Var name ->
           let EnvMap map = env in
           Printf.printf "%s" name;
           PPMap.find name map

end
