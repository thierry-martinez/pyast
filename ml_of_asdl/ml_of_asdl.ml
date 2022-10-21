open Ppxlib

let label_declaration_of_field (field : Asdl.field) :
    Parsetree.label_declaration =
  let name = Metapp.mkloc (Ocamlsyntax.make_valid_label field.id) in
  Ast_helper.Type.field name (Union_asdl.core_type_of_field field)

let constructor_declaration_of_constructor (constructor : Asdl.constructor) :
    Parsetree.constructor_declaration =
  let name =
    Metapp.mkloc
      (Ocamlsyntax.make_valid_constructor_name constructor.constructor_id) in
  let args : Parsetree.constructor_arguments =
    match constructor.fields with
    | [] -> Pcstr_tuple []
    | [field] ->
        let ty = Union_asdl.core_type_of_field field in
        Parsetree.Pcstr_tuple [ty]
    | fields ->
        let types = List.map label_declaration_of_field fields in
        Parsetree.Pcstr_record types in
  Ast_helper.Type.constructor name ~args

let type_declaration_of_definition
    (definitions : Asdl.definition Union_asdl.String_map.t) type_id
    (union_definition : Union_asdl.definition) s :
    Parsetree.type_declaration list =
  let type_name = Ocamlsyntax.make_valid_type_name type_id in
  match Union_asdl.String_map.find_opt type_id definitions with
  | None ->
      let manifest =
        match type_id with
        | "arg" -> [%type: string]
        | "slice" -> [%type: expr]
        | _ -> [%type: unit] in
      Ast_helper.Type.mk ~manifest (Metapp.mkloc type_name) :: s
  | Some definition ->
      let attributes =
        List.map label_declaration_of_field definition.attributes in
      let (kind : Parsetree.type_kind), auxiliary =
        match definition.desc with
        | Product product ->
            let labels =
              List.map label_declaration_of_field product @
              attributes in
            Ptype_record labels, []
        | Sum sum ->
            let constructors =
              List.map constructor_declaration_of_constructor sum in
            let kind : Parsetree.type_kind =
              Ptype_variant constructors in
            match attributes with
            | [] -> kind, []
            | _ :: _ ->
                let desc_type_name = type_name ^ "_desc" in
                let auxiliary =
                  [Ast_helper.Type.mk ~kind (Metapp.mkloc desc_type_name)] in
                let desc =
                  Ast_helper.Type.field (Metapp.mkloc "desc")
                    (Ast_helper.Typ.constr (Metapp.mklid desc_type_name) []) in
                Ptype_record (desc :: attributes), auxiliary in
      Ast_helper.Type.mk ~kind (Metapp.mkloc type_name) :: auxiliary @ s

let add_field prefix id (field : Union_asdl.field) (expr, s) =
  let core_type, item =
    Union_asdl.core_type_of_union_field prefix id field in
  let s =
    match item with
    | None -> s
    | Some item ->
        Ast_helper.Str.type_ Recursive [item] :: s in
  let id = Ocamlsyntax.make_valid_label id in
  Ast_helper.Exp.fun_ (Optional id) None
    (Ast_helper.Pat.var (Metapp.mkloc id)) expr,
  s

let add_fields prefix fields (ty, s) =
  Union_asdl.String_map.fold (add_field prefix) fields (ty, s)

let make_builder type_id full_name fields attributes body s =
  let type_name = Ocamlsyntax.make_valid_type_name type_id in
  let name = Ocamlsyntax.make_valid_identifier full_name in
  let expr, s = ([%expr fun () -> ([%e body] :
    [%t Ast_helper.Typ.constr (Metapp.mklid type_name) []])], s) |>
    add_fields full_name fields |>
    add_fields full_name attributes in
  let bindings =
    Ast_helper.Vb.mk (Ast_helper.Pat.var (Metapp.mkloc name)) expr in
  bindings, s

let make_field (version : Pyast_utils.Version.t)
      (union_fields : Union_asdl.product) prefix (field : Asdl.field) :
  Ast_helper.lid * Parsetree.expression =
  let union_field = Union_asdl.String_map.find field.id union_fields in
  let var = Metapp.Exp.var (Ocamlsyntax.make_valid_identifier field.id) in
  let default =
    match field.modifier with
    | Some Question -> Some [%expr None]
    | Some Star -> Some [%expr []]
    | None ->
       match Union_asdl.kind_of_field field with
       | String -> Some [%expr ""]
       | Int -> Some [%expr -1]
       | Bool -> Some [%expr false]
       | Type _ -> None in
  let expression =
    match union_field with
    | Singleton _ ->
       begin match default with
       | None -> [%expr Option.get [%e var]]
       | Some default -> [%expr Option.value ~default:[%e default] [%e var]]
       end
    | Multiple map ->
      let type_info =
        Union_asdl.Type_map.find (Union_asdl.Type_key.of_field field) map in
      let default_case =
        Ast_helper.Exp.case [%pat? _] begin match default with
          | None ->
             [%expr invalid_arg [%e Metapp.Exp.of_string
               (Format.asprintf "unavailable %s for %s in %s"
                 type_info.constructor_id field.id prefix)]]
          | Some default -> default
          end in
      let other_cases =
        match prefix, field.id with
        | "arguments", "args" when type_info.constructor_id <> "Expr_list" ->
           [Ast_helper.Exp.case [%pat? Some (Expr_list list)]
             [%expr List.map (fun (expr : expr) ->
               arg ?arg:(
                 match expr.desc with Name name -> Some name.id | _ -> None)
                 ()) list]]
        | "expr_Subscript", "slice" when type_info.constructor_id = "Expr" ->
           [Ast_helper.Exp.case [%pat? Some (Slice arg)]
             [%expr arg]]
        | "expr_Subscript", "slice" when type_info.constructor_id = "Slice" ->
           [Ast_helper.Exp.case
              [%pat? Some (Expr { desc = Tuple { elts; _ }; _})]
              [%expr ExtSlice (List.map (fun (expr : expr) : slice ->
                match expr with
                | { desc = Attribute { value =
                  { desc = Subscript { slice; _ }; _ }; attr; _ }; _ }
                  when attr = Common.encoded_slice ->
                    slice
                | _ ->
                    Index expr) elts)];
            Ast_helper.Exp.case
              [%pat? Some (Expr { desc = Attribute { value =
                  { desc = Subscript { slice; _ }; _ }; attr; _ }; _ })]
              [%expr slice];
            Ast_helper.Exp.case [%pat? Some (Expr arg)]
              [%expr Index arg]]
        | _ -> [] in
       Ast_helper.Exp.match_ [%expr ([%e var] : [%t Ast_helper.Typ.constr
         (Metapp.mklid
           (Ocamlsyntax.make_valid_type_name (Ocamlsyntax.concat prefix
             field.id)))
         []] option)]
         (Ast_helper.Exp.case [%pat? Some [%p (Metapp.Pat.construct
           (Lident type_info.constructor_id) [[%pat? arg]])]] [%expr arg] ::
          other_cases @ [default_case]) in
  Metapp.mklid (Ocamlsyntax.make_valid_label field.id), expression

let add_builder_of_product version (definition : Asdl.definition option) type_id
    (union_attributes : Union_asdl.attributes)
    (union_product : Union_asdl.product) s =
  match definition with
  | Some { desc = Product product; attributes } ->
     let fields =
       List.map (make_field version union_product type_id) product @
       List.map (make_field version union_attributes type_id) attributes in
     let expr = Ast_helper.Exp.record fields None in
     make_builder type_id type_id union_product union_attributes expr s
  | None | Some { desc = Sum _ } ->
      let expr =
        match type_id with
        | "arg" -> [%expr Option.get arg]
        | _ ->
            [%expr invalid_arg [%e Metapp.Exp.of_string
              (Format.asprintf "unavailable product in %s" type_id)]] in
      make_builder type_id type_id union_product union_attributes expr s

let add_builder_of_constructor version type_id union_attributes
    (constructors :
       (Asdl.constructor Union_asdl.String_map.t * Asdl.attributes) option)
    constructor_id union_fields (bindings, s) =
  let expr =
    match
      match constructors with
      | None -> None
      | Some (constructors, attributes) ->
          match Union_asdl.String_map.find_opt constructor_id constructors with
          | None -> None
          | Some constructor -> Some (constructor, attributes)
    with
    | None ->
       begin match type_id, constructor_id with
       | "expr", "Num" ->
          [%expr expr_constant ?lineno ?end_lineno ?end_col_offset ?col_offset
            ~value:(Some (Num (Option.get n))) ()]
       | "expr", "Str" ->
          [%expr expr_constant ?lineno ?end_lineno ?end_col_offset ?col_offset
            ~value:(Some (Str (Option.get s))) ()]
       | "expr", "NameConstant" ->
          [%expr expr_constant ?lineno ?end_lineno ?end_col_offset ?col_offset
            ~value:(Option.map (fun b -> Bool b) (Option.get value)) ()]
       | "expr", "Constant" ->
          [%expr match (Option.get value : constant) with
          | None -> expr_name ~id:"None" ~ctx:Load ()
          | Some Ellipsis -> assert false
          | Some (Bool false) -> expr_name ~id:"False" ~ctx:Load ()
          | Some (Bool true) -> expr_name ~id:"True" ~ctx:Load ()
          | Some (Num n) -> expr_num ~n ()
          | Some (Str s) -> expr_str ~s ()]
       | "expr_context", "Param" ->
          [%expr Load]
       | "expr", "Ellipsis" ->
          [%expr expr_constant ?lineno ?end_lineno ?end_col_offset ?col_offset
            ~value:(Some Ellipsis : constant) ()]
       | "slice", "Slice" ->
          [%expr expr_slice ?upper ?step ?lower ()]
       | "slice", "ExtSlice" ->
          [%expr expr_tuple ?elts:dims ~ctx:Load ()]
       | "slice", "Index" ->
          [%expr Option.get value]
       | "slice", "Ellipsis" ->
          [%expr slice_index ~value:(expr_ellipsis ()) ()]
       | "expr", "Slice" ->
          [%expr
            let attr = Common.encoded_slice in
            let ctx = expr_context_load () in
            let value = expr_tuple ~elts:[] ~ctx () in
            let slice : expr_subscript_slice =
              Slice (slice_slice ?upper ?step ?lower ()) in
            expr_attribute
              ~value:(expr_subscript ~value ~slice ~ctx () ) ~ctx ~attr ()]
       | "stmt", "Print" ->
          [%expr stmt_expr ~value:(expr_call
            ~func:(expr_name ~id:"print" ~ctx:(expr_context_load ()) ())
            ?args:values ()) ()]
       | "stmt", "Try" ->
          [%expr
            match
              Option.value ~default:[] handlers,
              Option.value ~default:[] finalbody
            with
            | [], _ :: _ ->
               stmt_tryfinally ?lineno ?end_lineno ?end_col_offset ?col_offset
                 ?finalbody ?body ()
            | _ :: _, [] ->
               stmt_tryexcept ?lineno ?end_lineno ?end_col_offset ?col_offset
                 ?orelse ?handlers ()
            | _ -> assert false]
       | "stmt", "TryExcept" ->
          [%expr stmt_try ?lineno ?end_lineno ?end_col_offset ?col_offset
              ?orelse ?handlers ?body ()]
       | "stmt", "TryFinally" ->
          [%expr stmt_try ?lineno ?end_lineno ?end_col_offset ?col_offset
              ?finalbody ?body ()]
       | "excepthandler", "ExceptHandler" ->
          [%expr excepthandler ?lineno ?end_lineno ?end_col_offset ?col_offset
            ?type_ ~name:(
              match (Option.get name : excepthandler_excepthandler_name) with
              | Expr_opt expr -> expr
              | Identifier_opt id ->
                 Option.map (fun id -> expr_name ~id ~ctx:Load ()) id) ?body ()]
       | _ -> [%expr invalid_arg [%e Metapp.Exp.of_string
          (Format.asprintf "unavailable constructor %s in %s" constructor_id
            type_id)]]
       end
    | Some (constructor, attributes) ->
        let prefix = Ocamlsyntax.concat type_id constructor_id in
        let fields =
          List.map (make_field version union_fields prefix)
            constructor.fields in
        let attributes =
          List.map (make_field version union_attributes prefix) attributes in
        let name =
          Metapp.mklid
            (Ocamlsyntax.make_valid_constructor_name constructor_id) in
        let constructor =
          match fields with
          | [] -> Ast_helper.Exp.construct name None
          | [_, field] ->
              Ast_helper.Exp.construct name (Some field)
          | fields ->
              Ast_helper.Exp.construct name
                (Some (Ast_helper.Exp.record fields None)) in
        let constructor =
          match attributes with
          | [] -> constructor
          | _ :: _ ->
              Ast_helper.Exp.record
                ((Metapp.mklid "desc", constructor) :: attributes) None in
        [%expr ([%e constructor] : [%t Ast_helper.Typ.constr
          (Metapp.mklid (Ocamlsyntax.make_valid_type_name type_id)) []])] in
  let binding, s =
    make_builder type_id (Ocamlsyntax.concat type_id constructor_id)
      union_fields union_attributes expr s in
  binding :: bindings, s

let index_list (indexer : 'a -> string) (list : 'a list) :
    'a Union_asdl.String_map.t =
  List.fold_left
    (fun accu item -> Union_asdl.String_map.add (indexer item) item accu)
    Union_asdl.String_map.empty list

let add_builder_of_sum version (definition : Asdl.definition option) type_id
    (union_attributes : Union_asdl.attributes) (sum' : Union_asdl.sum)
    (bindings, (s : Parsetree.structure)) =
  match definition with
  | None | Some { desc = Product _ } ->
     Union_asdl.String_map.fold
        (add_builder_of_constructor version type_id union_attributes None)
       sum' (bindings, s)
  | Some { desc = Sum sum; attributes } ->
     let constructors =
       index_list (fun (c : Asdl.constructor) -> c.constructor_id) sum in
     Union_asdl.String_map.fold
        (add_builder_of_constructor version type_id union_attributes
          (Some (constructors, attributes)))
        sum' (bindings, s)

let add_builder_of_definition version indexed_definitions type_id
    (definition' : Union_asdl.definition) (bindings, s) =
  let definition = Union_asdl.String_map.find_opt type_id indexed_definitions in
  let (bindings, s) =
    match definition'.desc.product with
    | None -> (bindings, s)
    | Some product ->
        let binding, s =
          add_builder_of_product version definition type_id
            definition'.attributes product s in
        binding :: bindings, s in
  let (bindings, s) =
    match definition'.desc.sum with
    | None -> (bindings, s)
    | Some product ->
        add_builder_of_sum version definition type_id definition'.attributes
          product (bindings, s) in
  (bindings, s)

let parser_of_field (union_fields : Union_asdl.field Union_asdl.String_map.t)
      (field : Asdl.field) : Asttypes.arg_label * Parsetree.expression =
  let union_field = Union_asdl.String_map.find field.id union_fields in
  let parser =
    match field.type_id with
    | "string" | "bytes" | "identifier" -> [%expr Py.String.to_string]
    | "int" ->
        begin
          match field.id with
          | "is_async" -> [%expr bool_of_int]
          | _ -> [%expr Py.Int.to_int]
        end
    | "bool" ->
        [%expr Py.Bool.to_bool]
    | _ ->
        Metapp.Exp.var (Ocamlsyntax.make_valid_identifier field.type_id) in
  let parser =
    match field.modifier with
    | None -> parser
    | Some Question -> [%expr to_option_map [%e parser]]
    | Some Star -> [%expr Py.List.to_list_map [%e parser]] in
  let parser =
    [%expr [%e parser] (Py.Object.find_attr_string obj
      [%e Metapp.Exp.of_string field.id])] in
  let expression =
    match union_field with
    | Singleton _ -> parser
    | Multiple map ->
       let type_info =
         Union_asdl.Type_map.find (Union_asdl.Type_key.of_field field) map in
       Metapp.Exp.construct (Lident type_info.constructor_id) [parser] in
  Labelled (Ocamlsyntax.make_valid_label field.id), expression

let parser_of_constructor
      (union_attributes : Union_asdl.attributes)
      (union_sum : Union_asdl.sum)
      type_id (attributes : Asdl.attributes) (constructor : Asdl.constructor) :
      Parsetree.case =
  let union_fields =
    Union_asdl.String_map.find constructor.constructor_id union_sum in
  let args =
    List.map (parser_of_field union_attributes) attributes @
    List.map (parser_of_field union_fields) constructor.fields @
    [(Asttypes.Nolabel, [%expr ()])] in
  let builder_name =
    Ocamlsyntax.make_valid_identifier
      (Ocamlsyntax.concat type_id constructor.constructor_id) in
  Ast_helper.Exp.case (Metapp.Pat.of_string constructor.constructor_id)
    (Ast_helper.Exp.apply
       (Ast_helper.Exp.ident (Metapp.mklid ~prefix:(Lident "Builder")
       builder_name)) args)

let parser_of_definition (union : Union_asdl.mod_)
      (definition : Asdl.definition) =
  let union_definition =
    Union_asdl.String_map.find definition.type_id union in
  let parser =
    match definition.desc with
    | Product product ->
        let union_fields = Option.get union_definition.desc.product in
        let builder_name =
          Ocamlsyntax.make_valid_identifier definition.type_id in
        let args =
          List.map (parser_of_field union_definition.attributes)
            definition.attributes @
          List.map (parser_of_field union_fields) product @
            [(Asttypes.Nolabel, [%expr ()])] in
        Ast_helper.Exp.apply
          (Ast_helper.Exp.ident (Metapp.mklid ~prefix:(Lident "Builder")
            builder_name)) args
    | Sum sum ->
        let constructors =
          List.map (parser_of_constructor union_definition.attributes
            (Option.get union_definition.desc.sum) definition.type_id
            definition.attributes) sum in
        let default_case =
          [Ast_helper.Exp.case [%pat? class_name] [%expr
            invalid_arg
              (Printf.sprintf "Unknown constructor: '%s'" class_name)]] in
        Ast_helper.Exp.match_ [%expr get_class_name obj]
          (constructors @ default_case) in
  Ast_helper.Vb.mk (Metapp.Pat.var
    (Ocamlsyntax.make_valid_identifier definition.type_id))
    [%expr fun obj -> [%e parser]]

let make_value id =
  Ocamlsyntax.make_valid_identifier (Ocamlsyntax.concat "value" id)

let converter_of_field (union_fields : Union_asdl.field Union_asdl.String_map.t)
      (field : Asdl.field) : Asttypes.arg_label * Parsetree.expression =
  let union_field = Union_asdl.String_map.find field.id union_fields in
  let converter =
    match field.type_id with
    | "string" | "bytes" | "identifier"
    | "int" | "bool"
    | "object" | "singleton" | "constant" -> None
    | _ ->
        Some (Metapp.Exp.var
          (Ocamlsyntax.make_valid_identifier field.type_id)) in
  let converter =
    Option.map (fun converter ->
      match field.modifier with
      | None -> converter
      | Some Question -> [%expr Option.map [%e converter]]
      | Some Star -> [%expr List.map [%e converter]]) converter in
  let identifier = Metapp.Exp.var (make_value field.id) in
  let converter =
    match converter with
    | None -> identifier
    | Some converter ->
        [%expr [%e converter] [%e identifier]] in
  let expression =
    match union_field with
    | Singleton _ -> converter
    | Multiple map ->
       let type_info =
         Union_asdl.Type_map.find (Union_asdl.Type_key.of_field field) map in
       Metapp.Exp.construct (Lident type_info.constructor_id) [converter] in
  Labelled (Ocamlsyntax.make_valid_label field.id), expression

let converter_of_constructor
      (union_attributes : Union_asdl.attributes)
      (union_sum : Union_asdl.sum)
      type_id (attributes : Asdl.attributes) (constructor : Asdl.constructor) :
      Parsetree.case =
  let union_fields =
    Union_asdl.String_map.find constructor.constructor_id union_sum in
  let args =
    List.map (converter_of_field union_attributes) attributes @
    List.map (converter_of_field union_fields) constructor.fields @
    [(Asttypes.Nolabel, [%expr ()])] in
  let builder_name =
    Ocamlsyntax.make_valid_identifier
      (Ocamlsyntax.concat type_id constructor.constructor_id) in
  let record_fields_of_fields fields =
    List.map (fun ({ id; _ } : Asdl.field) ->
      Longident.Lident (Ocamlsyntax.make_valid_label id),
      Metapp.Pat.var (make_value id)) fields in
  let pat_args =
    match constructor.fields with
    | [] -> None
    | [{ id; _ }] ->
        Some (Metapp.Pat.var (make_value id))
    | list ->
        Some (Metapp.Pat.record (record_fields_of_fields list)) in
  let pat =
    Ast_helper.Pat.construct (Metapp.mklid
      (Ocamlsyntax.make_valid_constructor_name constructor.constructor_id))
        pat_args in
  let pat =
    match attributes with
    | [] -> pat
    | _ :: _ ->
        Metapp.Pat.record ((Lident "desc", pat) ::
          record_fields_of_fields attributes) in
  Ast_helper.Exp.case pat
    (Ast_helper.Exp.apply
      (Ast_helper.Exp.ident (Metapp.mklid ~prefix:(Lident "Builder")
      builder_name)) args)

let converter_of_definition (union : Union_asdl.mod_)
      (definition : Asdl.definition) =
  let union_definition =
    Union_asdl.String_map.find definition.type_id union in
  let converter =
    match definition.desc with
    | Product product ->
        let union_fields = Option.get union_definition.desc.product in
        let builder_name =
          Ocamlsyntax.make_valid_identifier definition.type_id in
        let args =
          List.map (converter_of_field union_definition.attributes)
            definition.attributes @
          List.map (converter_of_field union_fields) product @
            [(Asttypes.Nolabel, [%expr ()])] in
        let destruct_pat =
          Metapp.Pat.record (List.map (fun (field : Asdl.field) ->
            Longident.Lident (Ocamlsyntax.make_valid_label field.id),
            Metapp.Pat.var (make_value field.id))
            (definition.attributes @ product)) in
        [%expr
           let [%p destruct_pat] = obj in
           [%e Ast_helper.Exp.apply
              (Ast_helper.Exp.ident (Metapp.mklid ~prefix:(Lident "Builder")
            builder_name)) args]]
    | Sum sum ->
        let constructors =
          List.map (converter_of_constructor union_definition.attributes
            (Option.get union_definition.desc.sum) definition.type_id
            definition.attributes) sum in
        Ast_helper.Exp.match_ [%expr obj] constructors in
  Ast_helper.Vb.mk (Metapp.Pat.var
    (Ocamlsyntax.make_valid_identifier definition.type_id))
    [%expr fun obj -> [%e converter]]

let structure_of_asdl version (asdl : Asdl.module_) (union : Union_asdl.mod_) =
  let indexed_definitions =
    index_list (fun (def : Asdl.definition) -> def.type_id) asdl.definitions in
  let type_declarations =
    Union_asdl.String_map.fold
      (type_declaration_of_definition indexed_definitions) union [] in
  let bindings, builders = Union_asdl.String_map.fold
    (add_builder_of_definition version indexed_definitions) union ([], []) in
  let parsers =
    List.map (parser_of_definition union) asdl.definitions in
  let converters =
    List.map (converter_of_definition union) asdl.definitions in
  let type_declarations =
    match type_declarations with
    | [] -> assert false
    | hd :: tl ->
        { hd with ptype_attributes = [
          Ast_helper.Attr.mk (Metapp.mkloc "deriving") (PStr [%str refl])]}
        :: tl in
  [%str
    module Ast = [%m Ast_helper.Mod.structure (
      (Ast_helper.Str.type_ Recursive type_declarations) ::
      List.rev builders @
      [Ast_helper.Str.value Recursive bindings])]

    include Ast

    module Parse (Builder : Union.S) = struct
      [%%i Ast_helper.Str.value Recursive parsers]
    end

    module To (Builder : Union.S) = struct
      [%%i Ast_helper.Str.value Recursive converters]
    end

    let parse_ast ast =
      let (module Current) = Pyparse.get_current_version () in
      let module Parser = Current.Parse (Ast) in
      Parser.mod_ ast

    let parse ?filename ?mode source =
      parse_ast (Pyparse.python_parse ?filename ?mode source)

    let parse_file ?mode filename =
      parse_ast (Pyparse.python_parse_file ?mode filename)]

let with_target target f =
  match target with
  | "-" -> f Format.std_formatter
  | _ ->
      let channel = open_out target in
      Redirect.write_and_close channel (fun () ->
        let fmt = Format.formatter_of_out_channel channel in
        f fmt;
        Format.pp_print_flush fmt ())

let main target version source_file union_file =
  try
    let asdl = Asdl.of_file source_file in
    let union =
      let channel = open_in union_file in
      Redirect.read_and_close channel (fun () ->
        (Marshal.from_channel channel : Union_asdl.mod_)) in
    let version = Pyast_utils.Version.parse version in
    let structure = [%str include Common
        let (version : Pyast_utils.Version.t) =
          [%e Refl.Lift.Exp.lift [%refl: Pyast_utils.Version.t] []
          version]
      ] @ structure_of_asdl version asdl union in
    with_target target (fun formatter ->
      Pprintast.structure formatter structure)
  with Asdl.Err.E e ->
    Format.eprintf "%a@." Asdl.Err.format e

let option_target =
  let doc = "Target file name" in
  Cmdliner.Arg.(
    value & opt string "-" & info ["o"] ~docv:"TARGET" ~doc)

let option_version =
  let doc = "Target file name" in
  Cmdliner.Arg.(
    required & opt (some string) None & info ["v"] ~docv:"VERSION" ~doc)

let source_file =
  let doc = "Source file" in
  Cmdliner.Arg.(
    required & pos 0 (some non_dir_file) None & info [] ~docv:"FILE" ~doc)

let union_file =
  let doc = "Union file" in
  Cmdliner.Arg.(
    required & pos 1 (some non_dir_file) None & info [] ~docv:"FILE" ~doc)

let options = Cmdliner.Term.(
  const main $ option_target $ option_version $ source_file $ union_file)

let info =
  let doc = "Convert ASDL into ML" in
  let man = [
      `S Cmdliner.Manpage.s_bugs;
      `P "Email bug reports to <thierry.martinez@inria.fr>.";
    ] in
  Cmdliner.Cmd.info "ml_of_asdl" ~doc ~man

let () =
  exit (Cmdliner.Cmd.eval (Cmdliner.Cmd.v info options))
