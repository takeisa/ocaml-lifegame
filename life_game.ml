(* module LifeGame : sig *)

(* end = struct *)
  
open Core.Std
open Screen

type board_t = Screen.pos_t list

let width = 20
let height = 20

let glider = let open Screen in
  [pos 4 2; pos 2 3; pos 4 3;
   pos 3 4; pos 4 4]

(* val show_board : board_t -> unit *)
let show_cells b = List.iter b ~f:(fun p -> (Screen.writeAt p "*"))

(* val is_alive : board_t -> pos_t -> bool *)
let is_alive b p = List.mem b p

(* val is_empty : board_t -> pos_t -> bool *)
let is_empty b p = not (is_alive b p)

(* neighbors : pos_t -> pos_t list *)
let neighbors p = let open Screen in
  let (x, y) = (pos_x p, pos_y p) in
  [pos (x-1) (y-1); pos x (y-1); pos (x+1) (y-1);
   pos (x-1) y; pos (x+1) y;
   pos (x-1) (y+1); pos x (y+1); pos (x+1) (y+1)]

(* wrap : pos_t -> pos_t *)
let wrap p =
  let open Screen in
  let (x, y) = (pos_x p, pos_y p) in
  let norm v s = (v - 1) mod s + 1 in
  pos (norm x width) (norm y height)

(* val live_neighbors : board_t -> pos_t -> int *)
let live_neighbors b p =
  let nbs = List.map (neighbors p) ~f:wrap in
  List.length (List.filter nbs ~f:(is_alive b))

(* survivors : board_t -> board_t *)
let survivors b =
  List.filter b
    ~f:(fun p ->
        let count = live_neighbors b p in
        count = 2 || count = 3)

(* births : board_t -> board_t *)
let births b =
  let b' = List.dedup (List.concat (List.map ~f:neighbors b)) in
  List.filter b'
    ~f:(fun p ->
        is_empty b p
          && let count = live_neighbors b p in count = 3)

(* next_gen : board_t -> board_t *)
let next_gen b = survivors b @ births b

(* life_game : board_t -> unit *)
let rec life_game b =
  (Screen.clear ();
   show_cells b;
   flush stdout;
   Unix.sleep 1;
   life_game (next_gen b);)

let () = life_game glider

(* end *)
