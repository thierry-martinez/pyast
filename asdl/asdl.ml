include Ast

module Err = Err

let of_lexbuf lexbuf =
  try
    Parser.module_ Lexer.lexer lexbuf
  with Parser.Error ->
    Err.syntax (Loc.of_lexbuf lexbuf)

let of_channel ?filename channel =
  let lexbuf = Lexing.from_channel channel in
  Option.iter (Lexing.set_filename lexbuf) filename;
  of_lexbuf lexbuf

let of_file filename =
  let channel = open_in filename in
  Redirect.read_and_close channel (fun () ->
    of_channel ~filename channel)
