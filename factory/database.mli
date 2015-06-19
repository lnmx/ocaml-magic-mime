
(** A database of mime-type information *)
type t

(** [load_file path] loads a database from the supplied path *)
val load_file : string -> t

(** [length db] returns the number of entries in the database *)
val length : t -> int

(** [get_mime_type mime-type] returns information about the provided mime type, if available *)
val get_mime_type : t -> string -> Mime_type.t option
