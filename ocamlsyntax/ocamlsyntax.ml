let is_keyword id =
  match id with
  | "and"
  | "as"
  | "assert"
  | "begin"
  | "class"
  | "constraint"
  | "do"
  | "done"
  | "downto"
  | "else"
  | "end"
  | "exception"
  | "external"
  | "false"
  | "for"
  | "fun"
  | "function"
  | "functor"
  | "if"
  | "in"
  | "include"
  | "inherit"
  | "initializer"
  | "lazy"
  | "let"
  | "match"
  | "method"
  | "module"
  | "mutable"
  | "new"
  | "nonrec"
  | "object"
  | "of"
  | "open"
  | "or"
  | "private"
  | "rec"
  | "sig"
  | "struct"
  | "then"
  | "to"
  | "true"
  | "try"
  | "type"
  | "val"
  | "virtual"
  | "when"
  | "while"
  | "with"
  | "lor"
  | "lxor"
  | "mod"
  | "land"
  | "lsl"
  | "lsr"
  | "asr" -> true
  | _ -> false

let make_valid_type_name type_name =
  let type_name = String.lowercase_ascii type_name in
  if is_keyword type_name then
    type_name ^ "_"
  else
    type_name

let make_valid_label = make_valid_type_name

let make_valid_constructor_name name =
  String.capitalize_ascii name

let make_valid_identifier = make_valid_type_name

let concat a b =
  Printf.sprintf "%s_%s" a b

let with_target target f =
  match target with
  | "-" -> f Format.std_formatter
  | _ ->
      let channel = open_out target in
      Redirect.write_and_close channel (fun () ->
        let fmt = Format.formatter_of_out_channel channel in
        f fmt;
        Format.pp_print_flush fmt ())
