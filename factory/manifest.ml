
module M = Set.Make(String)

type t = M.t

let load_file path =
  let rec read input manifest =
    try
      match String.trim (input_line input) with
      | "" -> read input manifest
      | mt -> read input (M.add mt manifest)
    with End_of_file -> manifest
  in
  let in_file = open_in path in
  let manifest = read in_file M.empty in
  let () = close_in_noerr in_file in
  manifest

let length m =
  M.cardinal m

let entries m =
  M.elements m

let contains m el =
  M.mem el m
