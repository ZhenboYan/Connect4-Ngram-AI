open Core

let chunks (n : int) (l : 'a list) : 'a list list =
  if n <= 0 then invalid_arg "invalid input n"
  else
    let rec make_list (n : int) (i : int) (l : 'a list) (new_l : 'a list) :
        'a list =
      if n >= i then make_list (n - 1) i l @@ (List.nth_exn l n :: new_l)
      else new_l
    in
    List.mapi
      ~f:(fun i _ ->
        if i + n <= List.length l then make_list (i + n - 1) i l [] else [])
      l
    |> List.filter ~f:(fun x -> not @@ List.is_empty x)

let every_other_chunks (n : int) (odd_even : int) (l : 'a list) : 'a list list =
  chunks n l |> List.filteri ~f:(fun i _ -> i % 2 = odd_even)

let wining_sequence (n : int) (winner : int) (l : 'a list) : 'a list list =
  match (winner, n % 2) with
  | 1, 1 -> every_other_chunks n 0 l
  | 1, 0 -> every_other_chunks n 1 l
  | 2, 1 -> every_other_chunks n 1 l
  | 2, 0 -> every_other_chunks n 0 l
  | _ -> invalid_arg "winner has wrong value"

let split_last (l : 'a list) : 'a * 'a list =
  if List.is_empty l then invalid_arg "empty list"
  else (List.rev l |> List.hd_exn, List.rev l |> List.tl_exn |> List.rev)

module List_key (Elt : Map.Key) : Map.Key with type t = Elt.t list = struct
  type t = Elt.t list [@@deriving compare, sexp]
end

let sample (b : 'a Bag.t) : 'a option =
  let bag_to_list (b : 'a Bag.t) (l : 'a list) : 'a list =
    Bag.fold b ~init:l ~f:(fun acc a -> a :: acc)
  in
  if Bag.length b = 0 then None
  else List.nth (bag_to_list b []) (Random.int @@ Bag.length b)

module N_grams (Token : Map.Key) = struct
  module Token_list_map : Map.S with type Key.t = Token.t list =
  Map.Make (struct
    type t = Token.t list [@@deriving compare, sexp]
  end)

  type distribution = Token.t Bag.t Token_list_map.t [@@deriving sexp]

  module Token_list = List_key (Token)

  let make_kv (n : int) (player : int) (l : Token.t list) :
      (Token.t * Token.t list) list =
    wining_sequence n player l |> List.map ~f:(fun x -> split_last x)

  let make_keys (n : int) (player : int) (l : Token.t list) : Token.t list list
      =
    make_kv n player l |> List.map ~f:(fun (_, k) -> k)

  let map_add (map : Token.t Bag.t Token_list_map.t)
      (key : Token_list_map.Key.t) (data : Token.t Bag.t) :
      Token.t Bag.t Token_list_map.t =
    match Token_list_map.find map key with
    | None -> Token_list_map.add_exn map ~key ~data
    | _ -> map

  let make_bag (key : Token.t list) (value_key : ('a * Token_list.t) list)
      (bag : 'a Bag.t) : unit =
    Bag.clear bag;
    List.fold value_key
      ~f:(fun () (v, k) ->
        if Token_list.compare key k = 0 then Bag.add_unit bag v else ())
      ~init:()

  let make_distribution (key_list : Token_list_map.Key.t list)
      (value_key : (Token.t * Token_list.t) list)
      (map : Token.t Bag.t Token_list_map.t) : Token.t Bag.t Token_list_map.t =
    List.fold key_list
      ~f:(fun acc k ->
        let bag = Bag.create () in
        make_bag k value_key bag;
        map_add acc k bag)
      ~init:map

  let ngrams (n : int) (player : int) (l : Token.t list) : distribution =
    make_distribution (make_keys n player l) (make_kv n player l)
      Token_list_map.empty
end

let player1_second_move (pos : int * int) : int * int =
  match pos with
  | 0, 0 -> (1, 3)
  | 0, 1 -> (1, 1)
  | 0, 2 -> (1, 2)
  | 1, 3 -> (2, 3)
  | 0, 4 -> (1, 4)
  | 0, 5 -> (1, 5)
  | 0, 6 -> (1, 3)
  | _ -> invalid_arg "invalid input"

let player2_first_move (pos : int * int) : int * int =
  match pos with
  | 0, 0 -> (0, 3)
  | 0, 1 -> (0, 2)
  | 0, 2 -> (0, 3)
  | 0, 3 -> (1, 3)
  | 0, 4 -> (0, 3)
  | 0, 5 -> (0, 4)
  | 0, 6 -> (0, 3)
  | _ -> invalid_arg "invalid input"

let distribution_maker (p0 : int) (p1 : int) (p2 : int) (p3 : int) (p4 : int)
    (p5 : int) (p6 : int) : int =
  if p6 + p5 + p4 + p3 + p1 + p2 + p0 = 100 then
    let r = Random.int 100 + 1 in
    if r <= p6 then 6
    else if r <= p6 + p5 then 5
    else if r <= p6 + p5 + p4 then 4
    else if r <= p6 + p5 + p4 + p3 then 3
    else if r <= p6 + p5 + p4 + p3 + p2 then 2
    else if r <= p6 + p5 + p4 + p3 + p2 + p1 then 1
    else 0
  else invalid_arg "invalid distribution"

let standard_distribution _ : int = distribution_maker 2 14 20 27 20 15 2

let pair (a : 'a * 'a) (n : int) : 'a =
  match (a, n) with
  | (x, _), 1 -> x
  | (_, y), 2 -> y
  | _ -> invalid_arg "wrong n"

(* tested correct *)
let ai_is_valid_move (move : int * int) (history : (int * int) list) : bool =
  match move with
  | x, y ->
      if x < 0 || x > 5 || y < 0 || y > 6 then false
      else
        pair (List.unzip history) 1
        |> List.foldi ~init:true ~f:(fun i acc x ->
               if
                 x = pair move 1
                 && List.nth_exn (pair (List.unzip history) 2) i = pair move 2
               then acc && false
               else acc && true)

let rec get_n_items (n : int) (original : (int * int) list)
    (return_list : (int * int) list) : (int * int) list =
  match n with
  | 0 -> return_list
  | _ ->
      get_n_items (n - 1) original (List.nth_exn original (n - 1) :: return_list)

(* n here is n-1 in n_grams *)
let get_last_n_moves (n : int) (history : (int * int) list) : (int * int) list =
  if n < 2 then invalid_arg "n is designed to be greater than 2"
  else if n > 20 then invalid_arg "n is designed to be less than 20"
  else if List.length history < 3 then []
  else if List.length history < n then history
  else get_n_items n history []

let merge_distribution (new_d : ('a, 'b Bag.t, 'c) Map_intf.Map.t)
    (og_d : ('a, 'b Bag.t, 'c) Map_intf.Map.t) :
    ('a, 'b Bag.t, 'c) Map_intf.Map.t =
  Map.fold new_d ~init:og_d ~f:(fun ~key:k ~data:v acc ->
      Map.update acc k ~f:(fun data ->
          match data with
          | None -> v
          | Some x ->
              Bag.transfer ~src:v ~dst:x;
              x))

let around_last_move (history : ('a * int) list) : int =
  match List.nth_exn history (List.length history - 1) with
  | _, 6 -> distribution_maker 1 3 10 24 26 26 10
  | _, 5 -> distribution_maker 1 3 10 24 30 19 13
  | _, 4 -> distribution_maker 1 6 10 24 27 22 10
  | _, 3 -> standard_distribution 0
  | _, 2 -> distribution_maker 10 22 27 24 10 6 1
  | _, 1 -> distribution_maker 13 19 30 24 10 3 1
  | _, 0 -> distribution_maker 10 26 26 24 10 3 1
  | _, _ -> invalid_arg "invalid history"

let ai_dist_move (dist : int -> int) (history : (int * int) list) : int * int =
  if List.length history >= 42 then invalid_arg "invalid history"
  else
    let m =
      if List.length history = 0 then dist 0 else around_last_move history
    in
    let rec until_valid level move repeat =
      if level < 6 then
        if level = 4 && repeat <> 1 then until_valid 0 (dist repeat) (repeat + 1)
        else if ai_is_valid_move (level, move) history then (level, move)
        else until_valid (level + 1) move repeat
      else until_valid 0 (dist 1) repeat
    in
    until_valid 0 m 0

let desome (x : 'a option) : 'a =
  match x with Some x -> x | _ -> invalid_arg "none case"

let ai_move (history : (int * int) list) (last_n : int)
    (dist_map : ((int * int) list, (int * int) Bag.t, 'a) Map_intf.Map.t)
    (dist : int -> int) (num_repeat : int) : int * int =
  (* pre programed moves *)
  match List.length history with
  | 0 -> (0, 3)
  | 1 -> player2_first_move (List.hd_exn history)
  | 2 -> player1_second_move (List.tl_exn history |> List.hd_exn)
  | _ ->
      let rec re_move i =
        if i < num_repeat then
          match Map.find dist_map (get_last_n_moves last_n history) with
          | Some x ->
              if ai_is_valid_move (desome (sample x)) history then
                desome (sample x)
              else re_move (i + 1)
          | None -> ai_dist_move dist history
        else ai_dist_move dist history
      in
      re_move 0