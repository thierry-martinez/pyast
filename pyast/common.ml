type num =
  | Int of int
  | Float of float
  | Big_int of (Py.Object.t [@opaque])
  [@@deriving refl]

type object_ = num [@@deriving refl]

let num (obj : Py.Object.t) : num =
  match Py.Type.get obj with
  | Int -> Int (Py.Int.to_int obj)
  | Long -> Int (Py.Long.to_int obj)
  | Float -> Float (Py.Float.to_float obj)
  | unexpected_type ->
      invalid_arg
        (Printf.sprintf "build_num: unexpected type '%s'"
           (Py.Type.name unexpected_type))

let object_ = num

type constant_desc =
  | Ellipsis
  | Bool of bool
  | Num of num
  | Str of string
  [@@deriving refl]

type constant = constant_desc option [@@deriving refl]

type singleton = bool option [@@deriving refl]

let to_option_map (f : Py.Object.t -> 'a) (obj : Py.Object.t) :
    'a option =
  if Py.is_none obj then
    None
  else
    Some (f obj)

let get_class_name (obj : Py.Object.t) : string =
  Py.String.to_string Pyops.(obj.@$("__class__").@$("__name__"))

let constant_desc (obj : Py.Object.t) : constant_desc =
  match Py.Type.get obj with
  | Unknown ->
     begin match get_class_name obj with
     | "ellipsis" -> Ellipsis
     | _ -> invalid_arg "constant_desc"
     end
  | Bytes | Unicode -> Str (Py.String.to_string obj)
  | Bool -> Bool (Py.Bool.to_bool obj)
  | _ -> Num (num obj)

let constant = to_option_map constant_desc

let singleton = to_option_map Py.Bool.to_bool

let option_apply (f : 'a -> 'b) (opt : 'a option) : 'b =
  f (Option.get opt)

let option_bind f x = Option.bind x f

let list_option_bind (f : 'a -> 'b list) (opt : 'a option) : 'b list =
  match opt with
  | None -> []
  | Some v -> f v

let bool_of_int (obj : Py.Object.t) : bool =
  Py.Int.to_int obj <> 0

let get_attr_string (obj : Py.Object.t) (attr : string) : Py.Object.t option =
  let value = Pywrappers.pyobject_getattrstring obj attr in
  if Py.is_null value then
    begin
      Py.Err.clear ();
      None
    end
  else
    Some value
