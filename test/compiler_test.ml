open Scanner
open Scanner__.Lang1
open Scanner__.Lang1.Value
open Scanner__.ByteCode.VM.VMOp

open Stdlib
type parsed_text = (int, string ) result
[@@deriving show]

(* Eager *)
(* let lazyFixpoint = *)
(*   let innerAbs = *)
(*     Abs( *)
(*       "x", *)
(*       App( Var "f",  App( Var "x", Var "x" ) )) in *)
(*   Abs(  "f",  App ( innerAbs,  innerAbs )) *)
(* Lazy *)
let lazyFixpoint =
  let innerAbs =
    Abs ("x",
      App (Var "f", Abs ("z", App (App (Var "x", Var "x"), Var "z")))
    )
  in
  Abs ("f", App (innerAbs, innerAbs))

let fibDirect n =
  let xMinus n =  Builtin (Arithmetic( Sub, Var "x", Lit n  )) in
  let fb = Builtin( Arithmetic( Add, App( Var "fib", xMinus (BInt 1)) , App( Var "fib", xMinus (BInt 2))  ) ) in
  Bind(
    true, "fib",
        Abs(
          "x",
          Cond(
            Builtin( Comparison( Less, Var "x", Lit (BInt 2))  ),
            Lit (BInt 1),
            fb
          )

  ), App( Var "fib", Lit n)
  )

let fibStep =
  let xMinus n =  Builtin (Arithmetic( Sub, Var "x", Lit n  )) in
  let fb = Builtin( Arithmetic( Add, App( Var "f", xMinus (BInt 1)) , App( Var "f", xMinus (BInt 2))  ) ) in
  Abs(  "f",
        Abs(
          "x",
          Cond(
            Builtin( Comparison( Less, Var "x", Lit (BInt 2))  ),
            Lit (BInt 1),
            fb
          )
        )
     )

let fib( n : btype ) =
  let fn = App( lazyFixpoint, fibStep ) in
  let  empty_map : env = EnvMap( PPMap.empty ) in
  Scanner__.Lang1.Value.eval empty_map (App( fn, Lit n))


let fibD( n : btype ) =
  let  empty_map : env = EnvMap( PPMap.empty ) in
  Scanner__.Lang1.Value.eval empty_map (fibDirect n)


(* https://blag.cedeela.fr/format-all-the-data-structures/ *)
(* https://stackoverflow.com/questions/59740132/pretty-print-a-hashtbl-in-ocaml-to-work-with-ppx-deriving *)

let%expect_test _=

  let parsed_text = main "(1)" in
  Printf.printf "%s" (show_parsed_text parsed_text);
  [%expect {| (Ok 1) |}]

let%expect_test _=

  Printf.printf "%s" (Format.asprintf "%a" pp_value (fib (BInt 5)));
  [%expect {| fxfxxxfzxfxxxfzxfxxxfzxfxxxfzxfxxxfzxfxxxfzxfxxxfzxfxxxfzxfxxxfzxfxxxfzxfxxxfzxfxxxfzxfxxxfzxfxxxfzx(Lang1.Lang.VInt 8) |}]


let%expect_test _=

  Printf.printf "%s" (Format.asprintf "%a" pp_value (fibD (BInt 5)));
  [%expect {| fibxfibxxfibxxfibxxfibxxfibxxfibxxfibxxfibxxfibxxfibxxfibxxfibxxfibxxfibxx(Lang1.Lang.VInt 8) |}]

let%expect_test _=

  eval (Lit (BInt 5));
  [%expect {| (ByteCode.Value.VInt 5) |}]

let%expect_test _=

  eval (Builtin (Arithmetic (Add, (Lit( BInt( 1 ))),  Lit( BInt 55))));
  (* eval (Builtin (UnaryArithmetic (Neg, Lit( BInt 55)))); *)

  [%expect {| (ByteCode.Value.VInt 56) |}]

let () =
 Eio_main.run @@ fun env ->
  cli
    ~stdin:(Eio.Flow.string_source "(1)")
    ~stdout:(Eio.Stdenv.stdout env)
