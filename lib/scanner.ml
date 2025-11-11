open Eio.Std
open Angstrom

module type PARSER = sig
  type consume
  val parse_string : string ->  (int, string) result
end

module LibAngstrom = struct

  let is_whitespace = function
  | '\x20' | '\x0a' | '\x0d' | '\x09' -> true
  | _ -> false
  let ws = take_while is_whitespace
  let reservedops = ["+","*","-",";"]
  let reservednames = ["def","extern"]
  let comma = char ',' <* ws
  let colon = char ':' <* ws

  let is_comment = function
    | '#'  -> false
    | x -> not @@ is_whitespace x

let is_integer = function '0'..'9' -> true | _ -> false
let is_double= function | '0'..'9' | '.' -> true | _ -> false


let integer =
    take_while1 is_integer>>| int_of_string

let double=
    take_while1 is_double>>| float_of_string

let parens p =
  char '(' >>= fun _ ->
  p >>= fun x ->
  char ')' >>= fun _ -> return x


let reservedops = function
    |ops  -> ()

let reservednames = function
    |names -> ()

let lexer =
    fix(fun lexer ->
      ws
      *> choice
      [
        parens integer ;
      ])
end

let main text =
 Angstrom.parse_string ~consume:All LibAngstrom.lexer text
module Syntax = struct

type name = string

type 'a expr =
    Const of float
  | BinOp of 'a op * 'a expr * 'a expr
  | Var of string
  | Call of name * 'a expr list
  | Function of name * 'a expr list  * 'a expr
  | Extern of name * 'a expr list
and 'a op
  = Plus
  | Minus
  | Times
  | Divide

end

type parsed_text = (int, string ) result
[@@deriving show]

type _  parsing_fn =
 Create_angstrom_parser : Consume.t  -> Consume.t parsing_fn

let create_parser  : type a. a parsing_fn ->  (module PARSER  with type consume = a )  =
  fun a  ->
  match a with
  | Create_angstrom_parser ty  ->
  let module Parser = struct
    type consume = a

    let parse_string text =
    Angstrom.parse_string ~consume:ty LibAngstrom.lexer text
  end in
  (module Parser : PARSER with type consume = a )


(* let create_parser  (type t) consume  (parsing_fn : consume :t -> 'a -> string -> (int, string) result ) lexer text = *)
(*   let module Parser = struct *)
(*     type consume = t *)

(*     let parse_string  = *)
(*       parsing_fn ~consume:consume lexer text *)
(*   end in *)
(*   (module Parser : PARSER with type consume = t ) *)

let parser_setup  (* text *) =
   (* (create_parser  Consume.All Angstrom.parse_string LibAngstrom.lexer text *)
   (*  : (module PARSER with type consume = Consume.t )) *)
   (create_parser (Create_angstrom_parser Consume.All)
    : (module PARSER with type consume = Consume.t ))

let cli ~stdin ~stdout =
  let buf = Eio.Buf_read.of_flow stdin ~initial_size:100 ~max_size:1_000_000 in
  (* while true do *)

    let line = Eio.Buf_read.line buf in
    traceln "> %s" line;
    match line with
     |line -> let (module P : PARSER with type consume = Consume.t  ) = parser_setup (* line *) in
              (* let parsed_text = main line in *)
              let parsed_text = P.parse_string in
               Eio.Flow.copy_string (Fmt.str " %S\n" (show_parsed_text (parsed_text line))) stdout
   ;
  (* done *)
