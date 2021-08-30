let identifier = ['A' - 'Z' 'a' - 'z' '0' - '9' '_']+

rule lexer = parse
| identifier as s {
  match s with
  | "module" -> Parser.Module
  | "attributes" -> Parser.Attributes
  | "version" -> Parser.Version
  | _ -> Parser.Identifier s
}
| ',' { Parser.Comma }
| '*' { Parser.Star }
| '?' { Parser.QuestionMark }
| '=' { Parser.Equal }
| '|' { Parser.Pipe }
| '(' { Parser.ParenOpen }
| ')' { Parser.ParenClose }
| '{' { Parser.CurlyBracketOpen }
| '}' { Parser.CurlyBracketClose }
| ' ' | '\t' | "--" [^ '\n']* { lexer lexbuf }
| '\n' {
  Lexing.new_line lexbuf;
  lexer lexbuf
}
| '"' ([^ '"']* as s) '"' { Parser.String s }
| eof { Parser.EOF }
| _ as c {
  Err.unexpected_character (Loc.of_lexbuf lexbuf) c
}
