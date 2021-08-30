module Version = struct
  type t = {
      major : int;
      minor : int;
      subminor : int;
    } [@@deriving refl]

  let parse (s : string) : t =
    try
      Scanf.sscanf s "%d.%d.%d"
        (fun major minor subminor -> { major; minor; subminor })
    with End_of_file ->
      Scanf.sscanf s "%d.%d"
        (fun major minor -> { major; minor; subminor = 0 })

  let to_string { major; minor; subminor } : string =
    Printf.sprintf "%d.%d.%d" major minor subminor
end
