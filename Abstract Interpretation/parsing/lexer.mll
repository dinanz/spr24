(* File lexer.mll   © P. Cousot 2021 *)
{
open Parser
exception Error of string
}
rule token = parse
    [' ' '\t' '\n'] { token lexbuf } (* skip blanks, tabs and newlines *)
  | "nand"     { NAND }
  | "if"       { IF }
  | "else"     { ELSE }
  | "while"    { WHILE }
  | "break"    { BREAK }
  | (['a'-'z'] | ['A'-'Z'])(['a'-'z'] | ['A'-'Z'] | ['0'-'9'])* as idt
               { IDENT idt }
  | ['0'-'9']+ as num
               { NUM (int_of_string num) }
  | '-'        { MINUS }
  | '<'        { LT }
  | '('        { LPAREN }
  | ')'        { RPAREN }
  | '='        { ASSIGN }
  | ';'        { SEMICOLON }
  | '{'        { LBRACKET }
  | '}'        { RBRACKET }
  | eof        { END }
  | _
    { raise (Error (Printf.sprintf "At offset %d: unexpected character.\n" 
                                          (Lexing.lexeme_start lexbuf))) }
