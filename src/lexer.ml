open Parser
open Sedlexing.Utf8

(* TODO: more information *)
exception Lexer_unknown_token

let whitespace = [%sedlex.regexp? Plus (' ' | '\t' | '\n')]
let alphabet = [%sedlex.regexp? 'a' .. 'z' | 'A' .. 'Z']
let digit = [%sedlex.regexp? '0' .. '9']
let variable = [%sedlex.regexp? (alphabet | '_'), Star (alphabet | digit | '_')]

let rec tokenizer buf =
  match%sedlex buf with
  | whitespace -> tokenizer buf
  | "fun" -> FUN
  | ":" -> COLON
  | "." -> DOT
  | "->" -> ARROW
  | "===" -> ALIAS
  | "let" -> LET
  | "type" -> TYPE
  | "kind" -> KIND
  | "forall" -> FORALL
  | "in" -> IN
  | "*" -> STAR
  | "(" -> LEFT_PARENS
  | ")" -> RIGHT_PARENS
  | variable -> VAR (lexeme buf)
  | eof -> EOF
  | _ -> raise Lexer_unknown_token

let provider buf () =
  let token = tokenizer buf in
  let start, stop = Sedlexing.lexing_positions buf in
  (token, start, stop)

(* TODO: expose better APIs *)
let from_string parser string =
  let buf = from_string string in
  let provider = provider buf in
  MenhirLib.Convert.Simplified.traditional2revised parser provider
