(* File main.ml   © P. Cousot 2021 *)

open AbstractSyntax;;
open Printer;;

let lexbuf = Lexing.from_channel stdin in
  try
    let p' = built_abstract_syntax (Parser.prog Lexer.token lexbuf) and margin = 0 in 
                print_labelled_node p' margin
  with
  | Lexer.Error msg ->
      Printf.fprintf stderr "%s%!" msg
  | Parser.Error ->
      Printf.fprintf stderr "At offset %d: syntax error.\n%!" (Lexing.lexeme_start lexbuf)