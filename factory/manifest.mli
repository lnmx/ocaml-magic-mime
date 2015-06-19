
(** A set of mime types *)
type t

(** [load_file path] loads a manifest from the supplied path *)
val load_file : string -> t

(** [length manifest] returns the number of entries in the manifest *)
val length : t -> int

(** [entries manifest] returns a list of mime types in the manifest *)
val entries : t -> string list

(** [contains manifest mime-type] return true if the manifest contains the supplied mime-type *)
val contains : t -> string -> bool

