open Core

(* THE BOTTOM LEFT OF THE BOARD IS (0,0) *)

type gameboard = int list list

let empty =
  [
    [ 0; 0; 0; 0; 0; 0; 0 ];
    [ 0; 0; 0; 0; 0; 0; 0 ];
    [ 0; 0; 0; 0; 0; 0; 0 ];
    [ 0; 0; 0; 0; 0; 0; 0 ];
    [ 0; 0; 0; 0; 0; 0; 0 ];
    [ 0; 0; 0; 0; 0; 0; 0 ];
  ]

let curr_player = ref 1

let set_player (player : int) = curr_player := player

type history = int * int list

let game_history = ref []
(* history is :: to the front, List.rev before giving to AI *)

let is_valid_position (pos: int * int): bool =
  match pos with
  | x, y -> if x > 6 || x < 0 || y > 5 || y < 0 then false else true;;

(* Given a board and an (x,y) position, it will return the value at
   that position in the board. *)
let get_pos (pos: int * int) (board : gameboard) : int =
  if not @@ is_valid_position pos then failwith "Invalid position"
  else
    match pos with
    | x_pos, y_pos ->
    let _, laterRows = List.split_n board y_pos in
    let curr_row = List.hd_exn laterRows in
    let _, later_positions = List.split_n curr_row x_pos in
    List.hd_exn later_positions

let change_row (x_pos : int) (new_val : int) (row : int list) : int list =
  let first_positions, later_positions = List.split_n row x_pos in
  match later_positions with
  | [] -> failwith "Invalid position"
  | _ :: tl -> List.append first_positions (new_val :: tl)

let rec change_pos (curr_x : int) (curr_y : int) (pos: int * int)
    (new_val : int) (board : gameboard) : gameboard =
  if not @@ is_valid_position pos then failwith "Invalid position"
  else
    match board, pos with
    | (hd :: tl), (x_pos, y_pos) ->
        if curr_y < y_pos then
          hd :: change_pos (curr_x + 1) (curr_y + 1) (x_pos, y_pos) new_val tl
        else change_row x_pos new_val hd :: tl
    | [], (_, _) -> failwith "Invalid board"

(* Given a board and a column to place a piece, it returns None if the move is
   invalid, and otherwise returns the updated board with the lowest available position
   in the column provided filled. *)
let rec get_available_space (board : gameboard) (col : int) (curr_row : int) :
    int * int =
  if get_pos (col, curr_row) board = 0 then (col, curr_row)
  else get_available_space board col (curr_row + 1)

(* history is updated as well*)
let make_move (board : gameboard) (col : int) : gameboard option =
  if get_pos (col, 5) board <> 0 then None
  else
    let x, y = get_available_space board col 0 in
    game_history := (y, x) :: !game_history;
    Some (change_pos 0 0 (x, y) !curr_player board)

let check_win (board : gameboard) : bool * int =
  let horizontal board =
    List.fold board ~init:false ~f:(fun acc x ->
        acc
        ||
        match
          List.fold x ~init:0 ~f:(fun acc y ->
              if acc = 4 then 4 else if y = !curr_player then acc + 1 else 0)
        with
        | 4 -> true
        | _ -> false)
  in
  let vertical board =
    let c = [ 0; 1; 2; 3; 4; 5; 6 ] in
    List.fold c ~init:false ~f:(fun acc i ->
        acc
        ||
        match
          List.fold board ~init:0 ~f:(fun acc x ->
              if acc = 4 then 4
              else if List.nth_exn x i = !curr_player then acc + 1
              else 0)
        with
        | 4 -> true
        | _ -> false)
  in
  let diagonal1 board =
    List.foldi board ~init:false ~f:(fun i acc y ->
        if i < 3 then acc
        else
          acc
          || List.foldi y ~init:false ~f:(fun j acc2 x ->
                 if j > 3 then acc2
                 else
                   acc2
                   || x = !curr_player
                      && x = List.nth_exn (List.nth_exn board (i - 1)) (j + 1)
                      && x = List.nth_exn (List.nth_exn board (i - 2)) (j + 2)
                      && x = List.nth_exn (List.nth_exn board (i - 3)) (j + 3)))
  in
  let diagonal2 board =
    List.foldi board ~init:false ~f:(fun i acc y ->
        if i > 2 then acc
        else
          acc
          || List.foldi y ~init:false ~f:(fun j acc2 x ->
                 if j > 3 then acc2
                 else
                   acc2
                   || x = !curr_player
                      && x = List.nth_exn (List.nth_exn board (i + 1)) (j + 1)
                      && x = List.nth_exn (List.nth_exn board (i + 2)) (j + 2)
                      && x = List.nth_exn (List.nth_exn board (i + 3)) (j + 3)))
  in
  ( horizontal board || vertical board || diagonal1 board || diagonal2 board,
    !curr_player )

(* -1 if it's draw else 1 or 2 *)
let is_game_over (board : gameboard) (h : 'a list) : bool*int =
  match (List.length h, check_win board) with
  | 42, (false, _) -> (true, -1)
  | _, z -> z

let history_to_board (history : (int * int) list) : gameboard=
  List.foldi history ~init:empty ~f:(fun i acc (j, k) ->
      List.mapi acc ~f:(fun i1 x ->
          if i1 = j then
            List.mapi x ~f:(fun i2 y -> if i2 = k then (i % 2) + 1 else y)
          else x))
