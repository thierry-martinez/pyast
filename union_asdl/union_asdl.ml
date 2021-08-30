open Ppxlib

module String_map = Map.Make (String)

type type_info = {
    constructor_id : string;
    core_type : Parsetree.core_type;
  }

module Type_key = struct
  type t = {
      type_id : string;
      modifier : Asdl.modifier option;
   }

  let of_field ({ type_id; modifier; _ } : Asdl.field) : t =
    { type_id; modifier }
end

module Type_map = Map.Make (struct
  type t = Type_key.t
  let compare = compare
end)

type field =
  | Singleton of Type_key.t * type_info
  | Multiple of type_info Type_map.t

type constructor = field String_map.t

type product = field String_map.t

type sum = constructor String_map.t

type desc = {
    product : product option;
    sum : sum option;
  }

type attributes = field String_map.t

type definition = {
    desc : desc;
    attributes : attributes;
  }

type mod_ = definition String_map.t

type kind =
  | String
  | Bool
  | Int
  | Type of string

let kind_of_field (field : Asdl.field) : kind =
  match field.type_id with
  | "string" | "bytes" | "identifier" -> String
  | "int" ->
      begin
        match field.id with
        | "is_async" -> Bool
        | _ -> Int
      end
  | "bool" -> Bool
  | _ -> Type (Ocamlsyntax.make_valid_type_name field.type_id)

let core_type_of_field (field : Asdl.field) : Parsetree.core_type =
  let ty =
    match kind_of_field field with
    | String -> [%type: string]
    | Int -> [%type: int]
    | Bool -> [%type: bool]
    | Type s -> Ast_helper.Typ.constr (Metapp.mklid s) [] in
  match field.modifier with
  | None -> ty
  | Some Question -> [%type: [%t ty] option]
  | Some Star -> [%type: [%t ty] list]

let core_type_of_union_field (prefix : string) (id : string) (field : field) :
  Parsetree.core_type * Parsetree.type_declaration option =
  match field with
  | Singleton (_, type_info) -> type_info.core_type, None
  | Multiple map ->
     let type_name =
       Ocamlsyntax.make_valid_type_name (Ocamlsyntax.concat prefix id) in
     let constructors =
       Type_map.fold (fun _key (ty : type_info) list ->
           Ast_helper.Type.constructor
             (Metapp.mkloc ty.constructor_id)
             ~args:(Pcstr_tuple [ty.core_type])
           :: list) map [] in
     let item =
       Ast_helper.Type.mk (Metapp.mkloc type_name)
         ~kind:(Ptype_variant constructors)  in
     Ast_helper.Typ.constr (Metapp.mklid type_name) [], Some item
