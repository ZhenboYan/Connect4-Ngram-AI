open Core
open Lib
open Board

module Pos = struct
  type t = int * int [@@deriving compare, sexp]
end

module Pos_grams = N_grams (Pos)

module Dist = struct
  type t = Pos_grams.distribution [@@deriving sexp]
end

(* for sexp writing *)
module Write_Distribution = Dist

let write_sexp_to_file (filename : string)
    (distribution : Pos_grams.distribution) =
  let output_file = Out_channel.create ~append:false filename in
  Out_channel.output_string output_file
    (Write_Distribution.sexp_of_t distribution |> Sexp.to_string);
  Out_channel.close output_file

let sexp_to_map (filename : string) : Pos_grams.distribution =
  Write_Distribution.t_of_sexp (Sexp.load_sexp filename)

let switch_player (history : 'a list) =
  match List.length history % 2 with
  | 0 -> set_player 2
  | 1 -> set_player 1
  | _ -> ()

let ai_trainning_move (dist : int -> int) (history : (int * int) list) :
    int * int =
  if List.length history >= 42 then invalid_arg "invalid history"
  else
    let m = dist 0 in
    let rec until_valid level move repeat =
      if level < 6 then
        if level = 4 && repeat <> 20 then
          until_valid 0 (dist repeat) (repeat + 1)
        else if ai_is_valid_move (level, move) history then (level, move)
        else until_valid (level + 1) move repeat
      else until_valid 0 (dist 1) repeat
    in
    until_valid 0 m 0

(* random moves for trainning purposes *)
let all_0 _ : int = distribution_maker 94 1 1 1 1 1 1

let all_1 _ : int = distribution_maker 1 94 1 1 1 1 1

let all_2 _ : int = distribution_maker 1 1 94 1 1 1 1

let all_3 _ : int = distribution_maker 1 1 1 94 1 1 1

let all_4 _ : int = distribution_maker 1 1 1 1 94 1 1

let all_5 _ : int = distribution_maker 1 1 1 1 1 94 1

let all_6 _ : int = distribution_maker 1 1 1 1 1 1 94

let tail _ : int = distribution_maker 48 1 1 1 1 1 47

let left _ : int = distribution_maker 30 36 30 1 1 1 1

let right _ : int = distribution_maker 1 1 1 1 30 36 30

let ll _ : int = distribution_maker 1 30 36 30 1 1 1

let rr _ : int = distribution_maker 1 1 1 30 36 30 1

let middle _ : int = distribution_maker 1 1 30 36 30 1 1

let lll _ : int = distribution_maker 47 48 1 1 1 1 1

let rrr _ : int = distribution_maker 1 1 1 1 1 48 47

(* number of games set to play *)
let num_games = 500

(* random distribution to train AI with*)
let random_d = all_0

(* number of times sample from distribution if it's invalid move *)
let repeat = 15

(* random vs random, AI is not making moves only recording *)
let vs_random (store_file : string) (gram : int) =
  let rec play
      (new_dist :
        ( Pos_grams.Token_list_map.Key.t,
          Pos.t Bag.t,
          Pos_grams.Token_list_map.Key.comparator_witness )
        Map_intf.Map.t) (history : Pos.t list) (i : int) =
    switch_player history;
    if i < num_games then
      match is_game_over (history_to_board history) history with
      | true, -1 -> play new_dist [] i
      | true, p ->
          play
            (merge_distribution (Pos_grams.ngrams gram p history) new_dist)
            [] (i + 1)
      | false, _ ->
          play new_dist (history @ [ ai_dist_move random_d history ]) i
    else write_sexp_to_file store_file new_dist
  in
  play (sexp_to_map store_file) [] 0

(* AI vs AI, AI is making moves and recording *)
let vs_itself (store_file : string) (gram : int) =
  let rec play
      (new_dist :
        ( Pos_grams.Token_list_map.Key.t,
          Pos.t Bag.t,
          Pos_grams.Token_list_map.Key.comparator_witness )
        Map_intf.Map.t) (history : Pos.t list) (i : int) =
    switch_player history;
    if i < num_games then
      match is_game_over (history_to_board history) history with
      | true, -1 -> play new_dist [] i
      | true, p ->
          play
            (merge_distribution (Pos_grams.ngrams gram p history) new_dist)
            [] (i + 1)
      | false, _ ->
          play new_dist
            (history @ [ ai_move history gram new_dist random_d repeat ])
            i
    else write_sexp_to_file store_file new_dist
  in
  play (sexp_to_map store_file) [] 0

(* AI vs random, AI is player 1, player 2 is alwasy random with given distribution *)
let train_player1 (player1 : string) (gram : int) =
  let rec play
      (new_dist :
        ( Pos_grams.Token_list_map.Key.t,
          Pos.t Bag.t,
          Pos_grams.Token_list_map.Key.comparator_witness )
        Map_intf.Map.t) (history : Pos.t list) (i : int) =
    switch_player history;
    if i < num_games then
      match is_game_over (history_to_board history) history with
      | true, -1 -> play new_dist [] i
      | true, p ->
          play
            (merge_distribution (Pos_grams.ngrams gram p history) new_dist)
            [] (i + 1)
      | false, _ ->
          if List.length history % 2 = 0 then
            play new_dist
              (history @ [ ai_move history gram new_dist random_d repeat ])
              i
          else
            play new_dist (history @ [ ai_trainning_move random_d history ]) i
    else write_sexp_to_file player1 new_dist
  in
  play (sexp_to_map "player1.txt") [] 0

(* number of grams to train from n to 4 *)
let rec n_times (n : int) =
  train_player1 "player1.txt" n;
  if n > 3 then n_times (n - 1) else ()

let () =
  (* Sys.chdir "set to the right directory"; *)
  n_times 11
