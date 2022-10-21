open Ppxlib

let type_info (field : Asdl.field) : Union_asdl.type_info =
  let type_key =
    match field.modifier with
    | None -> field.type_id
    | Some Question -> field.type_id ^ "_opt"
    | Some Star -> field.type_id ^ "_list" in
  let constructor_id = Ocamlsyntax.make_valid_constructor_name type_key in
  let core_type = Union_asdl.core_type_of_field field in
  { constructor_id; core_type }

let of_field (field : Asdl.field) : Union_asdl.field =
  Singleton (
    { type_id = field.type_id; modifier = field.modifier }, type_info field)

let add_field map (field : Asdl.field) =
  Union_asdl.String_map.add field.id (of_field field) map

let of_fields fields =
  List.fold_left add_field Union_asdl.String_map.empty fields

let add_constructor map ({ constructor_id; fields } : Asdl.constructor) =
  Union_asdl.String_map.add constructor_id (of_fields fields) map

let of_constructors constructors =
  List.fold_left add_constructor Union_asdl.String_map.empty constructors

let of_desc (desc : Asdl.desc) : Union_asdl.desc =
  match desc with
  | Product product -> { product = Some (of_fields product); sum = None; }
  | Sum sum -> { product = None; sum = Some (of_constructors sum); }

let add_definition map ({ type_id; desc; attributes } : Asdl.definition) =
  let definition : Union_asdl.definition =
    { desc = of_desc desc ; attributes = of_fields attributes } in
  Union_asdl.String_map.add type_id definition map

let of_definitions (definitions : Asdl.definition list) =
  List.fold_left add_definition Union_asdl.String_map.empty definitions

let insert_type type_key make_type_info
    (types : Union_asdl.field) : Union_asdl.field =
  match types with
  | Singleton (type_key', type_info') ->
      if type_key = type_key' then
        types
      else
        Multiple (Union_asdl.Type_map.add type_key (make_type_info ())
          (Union_asdl.Type_map.singleton type_key' type_info'))
  | Multiple map ->
      Multiple (map |> Union_asdl.Type_map.update type_key (function
        | None -> Some (make_type_info ())
        | (Some _) as item -> item))

let union_field map (field : Asdl.field) =
  map |> Union_asdl.String_map.update field.id (function
    | None -> Some (of_field field)
    | Some field' ->
        let types =
          insert_type { type_id = field.type_id; modifier = field.modifier }
            (fun () -> type_info field) field' in
        Some types)

let union_fields map fields =
  List.fold_left union_field map fields

let union_constructor map ({ constructor_id; fields } : Asdl.constructor) =
  map |> Union_asdl.String_map.update constructor_id (function
    | None -> Some (of_fields fields)
    | Some constructor' ->
        Some (union_fields constructor' fields))

let union_constructors map constructors =
  List.fold_left union_constructor map constructors

let union_option desc' desc of_desc union_desc =
  match desc' with
  | None -> Some (of_desc desc)
  | Some desc' -> Some (union_desc desc' desc)

let union_desc (desc' : Union_asdl.desc) (desc : Asdl.desc) =
  match desc with
  | Product product ->
      { desc' with
        product = union_option desc'.product product of_fields union_fields }
  | Sum sum ->
      { desc' with
        sum = union_option desc'.sum sum of_constructors union_constructors }

let union_definition map ({ type_id; desc; attributes } : Asdl.definition) =
  map |> Union_asdl.String_map.update type_id (function
    | None ->
        Some {
          Union_asdl.desc = of_desc desc;
          attributes = of_fields attributes }
    | Some definition' ->
        let desc = union_desc definition'.desc desc in
        let attributes = union_fields definition'.attributes attributes in
        Some { desc; attributes })

let union_definitions map (definitions : Asdl.definition list) =
  List.fold_left union_definition map definitions

let add_field prefix id (field : Union_asdl.field) (ty, s) =
  let core_type, item =
    Union_asdl.core_type_of_union_field prefix id field in
  let s =
    match item with
    | None -> s
    | Some item ->
        Ast_helper.Sig.type_ Recursive [item] :: s in
  let id = Ocamlsyntax.make_valid_label id in
  Ast_helper.Typ.arrow (Optional id) core_type ty, s

let add_fields prefix fields (ty, s) =
  Union_asdl.String_map.fold (add_field prefix) fields (ty, s)

let add_signature_of_fields id type_id fields attributes s =
  let name = Ocamlsyntax.make_valid_identifier id in
  let result_type = Ocamlsyntax.make_valid_type_name type_id in
  let ty, s =
    ([%type: unit -> [%t Ast_helper.Typ.constr (Metapp.mklid result_type) []]],
      s) |>
    add_fields id fields |>
    add_fields id attributes in
  Ast_helper.Sig.value (Ast_helper.Val.mk (Metapp.mkloc name) ty) :: s

let add_signature_of_constructor type_id attributes constructor_id fields
    s =
  add_signature_of_fields (Ocamlsyntax.concat type_id constructor_id) type_id
    fields attributes s

let add_signature_of_sum type_id sum attributes s =
  Union_asdl.String_map.fold (add_signature_of_constructor type_id attributes)
    sum s

let add_signature_of_definition type_id
    ({ desc; attributes } : Union_asdl.definition) s =
  let s =
    match desc.product with
    | None -> s
    | Some product ->
        add_signature_of_fields type_id type_id product attributes s in
  let s =
    match desc.sum with
    | None -> s
    | Some sum ->
        add_signature_of_sum type_id sum attributes s in
  s

let add_signature_of_definition_decl type_id _ s =
  Ast_helper.Sig.type_ Recursive [Ast_helper.Type.mk
    (Metapp.mkloc (Ocamlsyntax.make_valid_type_name type_id))] :: s

let signature_of_union union =
  Union_asdl.String_map.fold add_signature_of_definition union
    (Union_asdl.String_map.fold add_signature_of_definition_decl union [])

let main target_signature target_union source_files =
  try
    let union =
      match source_files with
      | [] -> assert false
      | first :: source_files ->
          List.fold_left (fun union source_file ->
            union_definitions union (Asdl.of_file source_file).definitions)
            (of_definitions (Asdl.of_file first).definitions) source_files in
    let signature = List.rev (signature_of_union union) in
    Ocamlsyntax.with_target target_signature (fun formatter ->
      Pprintast.structure formatter [%str
        open Common
        module type S = [%m Ast_helper.Mty.signature signature ]
      ]);
    let channel = open_out target_union in
    Redirect.write_and_close channel (fun () ->
      Marshal.to_channel channel union []);
  with Asdl.Err.E e ->
    Format.eprintf "%a@." Asdl.Err.format e

let target_signature =
  let doc = "Target signature" in
  Cmdliner.Arg.(
    required & pos 0 (some string) None & info [] ~docv:"SIG" ~doc)

let target_union =
  let doc = "Target union" in
  Cmdliner.Arg.(
    required & pos 1 (some string) None & info [] ~docv:"UNION" ~doc)

let source_files =
  let doc = "Source files" in
  Cmdliner.Arg.(
    value & pos_right 1 non_dir_file [] & info [] ~docv:"FILE" ~doc)

let options = Cmdliner.Term.(
  const main $ target_signature $ target_union $ source_files)

let info =
  let doc = "Create common signature from ASDL" in
  let man = [
      `S Cmdliner.Manpage.s_bugs;
      `P "Email bug reports to <thierry.martinez@inria.fr>.";
    ] in
  Cmdliner.Cmd.info "union_asdl" ~doc ~man

let () =
  exit (Cmdliner.Cmd.eval (Cmdliner.Cmd.v info options))
