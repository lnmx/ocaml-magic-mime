
type t = Mime_type.t list

let el_attr el attr =
  let (_, attrs) = el in
  let rec walk attrs =
    match attrs with
    | [] -> None
    | ((ns, nm), value) :: rest ->
      if nm = attr then Some( value )
      else walk rest
  in
  walk attrs

let el_attr_req el attr =
  match el_attr el attr with
  | Some( value ) -> value
  | None -> assert false

let el_name el =
  let ((_, name), _) = el in
  name

let xml_repr signal =
  match signal with
  | `Data _ -> "[data]"
  | `Dtd _ -> "[dtd]"
  | `El_end -> "</>"
  | `El_start el -> Printf.sprintf "<%s>" (el_name el)

let el_skip xml =
  let rec walk xml depth =
    let signal = Xmlm.input xml in
    (* let () = Printf.printf "el_skip %d %s\n" depth (xml_repr signal) in *)
    match depth, signal with
    | (d, `Data _) | (d, `Dtd _) -> walk xml d
    | (d, `El_start _) -> walk xml (d+1)
    | (0, `El_end) -> ()
    | (d, `El_end) -> walk xml (d-1)
  in
  walk xml 0

let read_glob xml top =
  (* TODO weight case_sensitive *)
  let pattern = el_attr_req top "pattern" in
  let weight = match el_attr top "weight" with
  | None -> 50
  | Some( w ) -> int_of_string w
  in
  let case_sensitive = match el_attr top "case-sensitive" with
  | None | Some( "false" ) -> false
  | Some( "true" ) -> true
  | _ -> assert false
  in
  let glob = Mime_type.make_glob pattern weight case_sensitive in
  let () = el_skip xml in
  glob

let read_mime_type xml top =
  (* Printf.printf "read_mime_type %s\n" (xml_repr (`El_start top)); *)
  let name = el_attr_req top "type" in
  let mt = Mime_type.make name in
  let rec walk xml depth mt =
    match Xmlm.input xml with
    | `Data _ | `Dtd _ -> walk xml depth mt
    | `El_end -> if depth = 0 then mt else walk xml (depth-1) mt
    | `El_start el -> (
      match el_name el with
      | "glob" -> let gl = read_glob xml el in
        let mt = { mt with Mime_type.glob=(mt.Mime_type.glob @ [ gl ]) } in
        walk xml depth mt
      | _ -> walk xml (depth+1) mt
    )
  in
  walk xml 0 mt

let load_xml xml =
  let rec walk xml db level =
    let signal = Xmlm.input xml in
    (* Printf.printf "load_xml %s\n" (xml_repr signal); *)
    match signal with
    | `El_start tag -> ( match el_name tag with
      | "mime-type" -> let entry = read_mime_type xml tag in
        let db = db @ [ entry ] in
        walk xml db level
      | _ -> walk xml db (level+1)
      )
    | `El_end -> if level = 1 then db else walk xml db (level-1)
    | `Dtd _ | `Data _ -> walk xml db level
  in
  walk xml [] 0

let load_file path =
  let in_file = open_in path in
  let in_xml = Xmlm.make_input ~strip:true (`Channel in_file) in
  let db = load_xml in_xml in
  let () = close_in_noerr in_file in
  db

let get_mime_type db id =
  try
    let mt = List.find (fun el -> id = el.Mime_type.name) db in
    Some( mt )
  with Not_found -> None


let length db =
  List.length db

