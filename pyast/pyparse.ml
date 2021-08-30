module Version = Pyast_utils.Version

type compile_mode = Exec | Eval | Single

module type S = sig
  include Union.S

  val version : Version.t

  module Parse (Builder : Union.S) : sig
    val mod_ : Py.Object.t -> Builder.mod_
  end

  module To (Builder : Union.S) : sig
    val mod_ : mod_ -> Builder.mod_
  end

  val parse_ast : Py.Object.t -> mod_

  val parse : ?filename:string -> ?mode:compile_mode -> string -> mod_

  val parse_file : ?mode:compile_mode -> string -> mod_
end

let string_of_compile_mode = function
  | Exec -> "exec"
  | Eval -> "eval"
  | Single -> "single"

let python_parse ?(filename = "<unknown>") ?(mode = Exec) source =
  let parse =
    Py.Module.get_function_with_keywords (Py.Import.import_module "ast")
      "parse" in
  let source = Py.String.of_string source in
  let filename = Py.String.of_string filename in
  let mode = Py.String.of_string (string_of_compile_mode mode) in
  parse [| source |] ["filename", filename; "mode", mode]

let python_parse_file ?mode filename =
  python_parse ~filename ?mode (Redirect.string_of_file filename)

exception UnsupportedVersion of Version.t

let get_version_ref = ref (fun _ -> failwith "get_version not yet available")

let get_version (v : Version.t) : (module S) =
  !get_version_ref v

let get_current_version () =
  get_version (Version.parse (Py.version ()))
