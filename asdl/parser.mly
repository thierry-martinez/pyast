%token <string> Identifier String
%token Module Attributes Version Comma Star Equal Pipe QuestionMark ParenOpen
%token ParenClose CurlyBracketOpen CurlyBracketClose EOF
%type <Ast.module_> module_
%start module_
%%

let module_ :=
  Module; id = identifier; version = version?; CurlyBracketOpen;
    definitions = definition*;
  CurlyBracketClose; EOF; {
    { Ast.id; version; definitions }
  }

let version :=
  Version; ~ = String; <>

let definition :=
  type_id = identifier; Equal; ~ = desc; attributes = attributes?; {
    { type_id; desc; attributes = Option.value ~default:[] attributes }
  }

let desc := product | sum

let product := ~ = fields; <Ast.Product>

let sum := ~ = separated_nonempty_list(Pipe, constructor); <Ast.Sum>

let fields :=
  ParenOpen; ~ = separated_nonempty_list(Comma, field); ParenClose; <>

let field :=
  type_id = identifier; modifier = modifier?; id = identifier; {
    { Ast.type_id; modifier; id }
  }

let constructor :=
  constructor_id = identifier; fields = fields?; {
    { Ast.constructor_id; fields = Option.value ~default:[] fields }
  }

let attributes :=
  Attributes; ~ = fields; <>

let modifier :=
| QuestionMark; { Ast.Question }
| Star; { Ast.Star }

let identifier :=
| Identifier
| Module; { "module" }
