

type glob = 
{
  pattern : string;
  weight : int;
  case_sensitive : bool;
}

let make_glob pattern weight case_sensitive : glob =
  { pattern; weight; case_sensitive }

type t = 
{
  name : string;
  glob : glob list;
  alias : string list;
  sub_class_of : string list;
}

let make name : t =
  { 
      name;
      glob=[];
      alias=[];
      sub_class_of=[];
  }

