module Screen : sig

  type pos_t

  val pos : int -> int -> pos_t

  val pos_x : pos_t -> int

  val pos_y : pos_t -> int
  
  val clear : unit -> unit

  val goto : pos_t -> unit

  val writeAt : pos_t -> string -> unit

end = struct

type pos_t = {x: int; y: int}

let pos x y = {x = x; y = y}

let pos_x p = p.x

let pos_y p = p.y

let clear () = (print_string "\x1b[2J";)

let goto p = Printf.printf "\x1b[%d;%dH" p.x p.y

let writeAt p s = (goto p;
                   print_string s;)

end
