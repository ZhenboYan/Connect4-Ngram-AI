open Core
open OUnit2
open Lib
open Board

let a_list = [ "a"; "b"; "c"; "d"; "e" ]

let test_invalid_chunks _ =
  let invalid_chunks _ = chunks 0 a_list in
  assert_raises (Invalid_argument "invalid input n") invalid_chunks;
  let invalid_chunks' _ = chunks (-1) a_list in
  assert_raises (Invalid_argument "invalid input n") invalid_chunks';
  let invalid_chunks'' _ = chunks (-2) a_list in
  assert_raises (Invalid_argument "invalid input n") invalid_chunks''

let test_split_last _ =
  assert_equal ("e", [ "a"; "b"; "c"; "d" ])
  @@ split_last [ "a"; "b"; "c"; "d"; "e" ];
  assert_equal ("d", [ "a"; "b"; "c" ]) @@ split_last [ "a"; "b"; "c"; "d" ];
  assert_equal ("c", [ "a"; "b" ]) @@ split_last [ "a"; "b"; "c" ];
  assert_equal ("b", [ "a" ]) @@ split_last [ "a"; "b" ];
  assert_equal ("a", []) @@ split_last [ "a" ]

let test_invalid_split_last _ =
  let invalid_split_last _ = split_last [] in
  assert_raises (Invalid_argument "empty list") invalid_split_last

let test_sample _ = assert_equal None @@ sample (Bag.create ())

let basic_lib_test =
  "Basic Lib Test"
  >: test_list
       [
         "Invalid Chunks" >:: test_invalid_chunks;
         "Split Last" >:: test_split_last;
         "Invalid Split Last" >:: test_invalid_split_last;
         "Sample" >:: test_sample;
       ]

module N = N_grams (Int)
module Token_list = List_key (Int)

let example_l = [ 1; 2; 3; 4; 5; 6; 7; 8; 9 ]

let bag_to_list (b : 'a Bag.t) (l : 'a list) =
  Bag.fold b ~init:l ~f:(fun acc a -> a :: acc)

let fold_sample_test n player l =
  List.fold
    (N.ngrams n player l |> Map.data)
    ~init:[]
    ~f:(fun acc a -> acc @ [ bag_to_list a [] ])

let test_ngrams _ =
  assert_equal [ [ 1; 3; 5; 7; 9 ] ] @@ fold_sample_test 1 1 example_l;
  assert_equal [ [ 2; 4; 6; 8 ] ] @@ fold_sample_test 1 2 example_l;
  assert_equal [ [ 3 ]; [ 5 ]; [ 7 ]; [ 9 ] ] @@ fold_sample_test 2 1 example_l;
  assert_equal [ [ 2 ]; [ 4 ]; [ 6 ]; [ 8 ] ] @@ fold_sample_test 2 2 example_l;
  assert_equal [ [ 3 ]; [ 5 ]; [ 7 ]; [ 9 ] ] @@ fold_sample_test 3 1 example_l;
  assert_equal [ [ 4 ]; [ 6 ]; [ 8 ] ] @@ fold_sample_test 3 2 example_l;
  assert_equal [] @@ fold_sample_test 11 1 example_l;
  assert_equal [] @@ fold_sample_test 12 2 example_l;
  assert_equal [] @@ fold_sample_test 1 1 [];
  assert_equal [] @@ fold_sample_test 1 2 [];
  assert_equal [ [ 1 ] ] @@ fold_sample_test 1 1 [ 1 ];
  assert_equal [] @@ fold_sample_test 1 2 [ 1 ]

let test_desome _ = assert_equal 1 @@ desome (Some 1)

let test_invalid_desome _ =
  let invalid_desome _ = desome None in
  assert_raises (Invalid_argument "none case") invalid_desome

module Int_Map = Map.Make (Int)
module String_list = List_key (String)
module Str_List_Map = Map.Make (String_list)

let test_wining_sequence _ =
  assert_equal [ [ 1; 2; 3 ]; [ 3; 4; 5 ]; [ 5; 6; 7 ] ]
  @@ wining_sequence 3 1 [ 1; 2; 3; 4; 5; 6; 7; 8 ];
  assert_equal [ [ 2; 3; 4 ]; [ 4; 5; 6 ]; [ 6; 7; 8 ] ]
  @@ wining_sequence 3 2 [ 1; 2; 3; 4; 5; 6; 7; 8 ];
  assert_equal [ [ 2; 3; 4; 5 ]; [ 4; 5; 6; 7 ] ]
  @@ wining_sequence 4 1 [ 1; 2; 3; 4; 5; 6; 7; 8 ];
  assert_equal [ [ 1; 2; 3; 4 ]; [ 3; 4; 5; 6 ]; [ 5; 6; 7; 8 ] ]
  @@ wining_sequence 4 2 [ 1; 2; 3; 4; 5; 6; 7; 8 ]

let test_pair _ =
  assert_equal 1 @@ pair (1, 2) 1;
  assert_equal 2 @@ pair (1, 2) 2

let test_player2_first_move _ =
  assert_equal (0, 3) @@ player2_first_move (0, 0);
  assert_equal (0, 2) @@ player2_first_move (0, 1);
  assert_equal (0, 3) @@ player2_first_move (0, 2);
  assert_equal (1, 3) @@ player2_first_move (0, 3);
  assert_equal (0, 3) @@ player2_first_move (0, 4);
  assert_equal (0, 4) @@ player2_first_move (0, 5);
  assert_equal (0, 3) @@ player2_first_move (0, 6)

let test_ai_is_valid_move _ =
  assert_equal false @@ ai_is_valid_move (0, 0) [ (0, 0) ];
  assert_equal true @@ ai_is_valid_move (0, 2) [ (0, 0) ];
  assert_equal false @@ ai_is_valid_move (-1, 2) [ (0, 0) ];
  assert_equal false @@ ai_is_valid_move (0, 7) [ (0, 0) ];
  assert_equal false @@ ai_is_valid_move (7, 7) [ (0, 0) ];
  assert_equal false @@ ai_is_valid_move (1, -1) [ (0, 0) ]

let test_get_last_n_moves _ =
  assert_equal [] @@ get_last_n_moves 5 [];
  assert_equal [] @@ get_last_n_moves 5 [ (0, 0) ];
  assert_equal [] @@ get_last_n_moves 5 [ (0, 0); (1, 1) ];
  assert_equal [ (0, 0); (1, 1); (2, 2) ]
  @@ get_last_n_moves 5 [ (0, 0); (1, 1); (2, 2) ];
  assert_equal [ (0, 0); (1, 1); (2, 2); (3, 3) ]
  @@ get_last_n_moves 5 [ (0, 0); (1, 1); (2, 2); (3, 3) ];
  assert_equal [ (0, 0); (1, 1); (2, 2); (3, 3) ]
  @@ get_last_n_moves 4 [ (0, 0); (1, 1); (2, 2); (3, 3) ];
  assert_equal [ (0, 0); (1, 1); (2, 2); (3, 3); (4, 4) ]
  @@ get_last_n_moves 5 [ (0, 0); (1, 1); (2, 2); (3, 3); (4, 4) ]

let test_history_to_board _ =
  assert_equal
    [
      [ 1; 0; 0; 1; 0; 0; 0 ];
      [ 0; 0; 0; 2; 0; 0; 0 ];
      [ 0; 0; 0; 0; 0; 0; 0 ];
      [ 0; 0; 0; 0; 0; 0; 0 ];
      [ 0; 0; 0; 0; 0; 0; 0 ];
      [ 0; 0; 0; 0; 0; 0; 0 ];
    ]
  @@ history_to_board [ (0, 3); (1, 3); (0, 0) ];
  assert_equal
    [
      [ 1; 1; 1; 1; 0; 0; 0 ];
      [ 0; 0; 0; 2; 0; 0; 0 ];
      [ 0; 0; 0; 2; 0; 0; 0 ];
      [ 0; 0; 0; 2; 0; 0; 0 ];
      [ 0; 0; 0; 0; 0; 0; 0 ];
      [ 0; 0; 0; 0; 0; 0; 0 ];
    ]
  @@ history_to_board [ (0, 3); (1, 3); (0, 0); (2, 3); (0, 1); (3, 3); (0, 2) ]

let almost_full_h =
  [
    (0, 0);
    (0, 1);
    (0, 2);
    (0, 3);
    (0, 4);
    (0, 5);
    (0, 6);
    (1, 0);
    (1, 1);
    (1, 2);
    (1, 3);
    (1, 4);
    (1, 5);
    (1, 6);
    (2, 0);
    (2, 1);
    (2, 2);
    (2, 3);
    (2, 4);
    (2, 5);
    (2, 6);
    (3, 0);
    (3, 1);
    (3, 2);
    (3, 3);
    (3, 4);
    (3, 5);
    (3, 6);
    (4, 0);
    (4, 1);
    (4, 2);
    (4, 3);
    (4, 4);
    (4, 5);
    (4, 6);
    (5, 0);
    (5, 1);
    (5, 2);
    (5, 3);
    (5, 4);
    (5, 5);
  ]

let full_h = almost_full_h @ [ (5, 6) ]

let test_ai_dist_move _ =
  assert_equal (5, 6) @@ ai_dist_move standard_distribution almost_full_h

module Pos = struct
  type t = int * int [@@deriving compare, sexp]
end

module Pos_grams = N_grams (Pos)

let abc =
  Pos_grams.ngrams 4 1
    [ (0, 1); (0, 2); (0, 3); (0, 4); (0, 5); (0, 6); (0, 7) ]

let abb =
  Pos_grams.ngrams 4 1
    [ (0, 1); (0, 2); (0, 3); (0, 4); (0, 5); (0, 6); (0, 7) ]

let bcd =
  Pos_grams.ngrams 4 2
    [ (0, 1); (0, 2); (0, 3); (0, 4); (0, 5); (0, 6); (0, 7) ]

let cde = Pos_grams.ngrams 4 1 [ (0, 4); (0, 5); (0, 6); (0, 7) ]

let bad_dist =
  Pos_grams.ngrams 4 1 [ (-1, -1); (7, 7); (8, 8); (-1, -1); (-1, -1) ]

let test_ai_move _ =
  assert_equal (0, 5)
  @@ ai_move [ (0, 2); (0, 3); (0, 4) ] 3 abc standard_distribution 10;
  assert_equal (0, 4)
  @@ ai_move [ (0, 1); (0, 2); (0, 3) ] 3 bcd standard_distribution 10;
  assert_equal (0, 3) @@ ai_move [] 3 bcd standard_distribution 10;
  assert_equal (1, 3) @@ ai_move [ (0, 3) ] 3 bcd standard_distribution 10;
  assert_equal (2, 3)
  @@ ai_move [ (0, 3); (1, 3) ] 3 bcd standard_distribution 10

let test_player1_second_move _ =
  assert_equal (1, 3) @@ player1_second_move (0, 0);
  assert_equal (1, 1) @@ player1_second_move (0, 1);
  assert_equal (1, 2) @@ player1_second_move (0, 2);
  assert_equal (2, 3) @@ player1_second_move (1, 3);
  assert_equal (1, 4) @@ player1_second_move (0, 4);
  assert_equal (1, 5) @@ player1_second_move (0, 5);
  assert_equal (1, 3) @@ player1_second_move (0, 6)

let test_new_invalid _ =
  let invalid_p2 _ = player2_first_move (-1, -1) in
  assert_raises (Invalid_argument "invalid input") invalid_p2;
  let invalid_p1 _ = player1_second_move (-1, -1) in
  assert_raises (Invalid_argument "invalid input") invalid_p1;
  let invalid_dist _ = distribution_maker 0 0 0 0 0 0 0 in
  assert_raises (Invalid_argument "invalid distribution") invalid_dist;
  let invalid_pair _ = pair (1, 1) 3 in
  assert_raises (Invalid_argument "wrong n") invalid_pair;
  let invalid_moves _ = get_last_n_moves 1 [] in
  assert_raises (Invalid_argument "n is designed to be greater than 2")
    invalid_moves;
  let invalid_moves' _ = get_last_n_moves 21 [] in
  assert_raises (Invalid_argument "n is designed to be less than 20")
    invalid_moves';
  let invalid_ai_dist _ =
    ai_dist_move standard_distribution (almost_full_h @ [ (5, 6) ])
  in
  assert_raises (Invalid_argument "invalid history") invalid_ai_dist;
  let invalid_around _ = around_last_move [ (-1, -1) ] in
  assert_raises (Invalid_argument "invalid history") invalid_around;
  let invalid_sequence _ = wining_sequence 0 3 [] in
  assert_raises (Invalid_argument "winner has wrong value") invalid_sequence

let test_merge_distribution _ =
  assert_equal
    [
      [ (0, 1); (0, 2); (0, 3) ];
      [ (0, 2); (0, 3); (0, 4) ];
      [ (0, 3); (0, 4); (0, 5) ];
      [ (0, 4); (0, 5); (0, 6) ];
    ]
  @@ (merge_distribution abc bcd |> Map.keys);
  assert_equal [ [ (0, 2); (0, 3); (0, 4) ]; [ (0, 4); (0, 5); (0, 6) ] ]
  @@ (merge_distribution abc cde |> Map.keys);
  assert_equal
    [
      [ (0, 2); (0, 3); (0, 4) ];
      [ (0, 4); (0, 5); (0, 6) ];
      [ (7, 7); (8, 8); (-1, -1) ];
    ]
  @@ (merge_distribution bad_dist abc |> Map.keys);
  assert_equal [ [ (0, 2); (0, 3); (0, 4) ]; [ (0, 4); (0, 5); (0, 6) ] ]
  @@ (merge_distribution abb abc |> Map.keys)

let test_distribution_maker _ =
  assert_equal 0 @@ distribution_maker 100 0 0 0 0 0 0;
  assert_equal 1 @@ distribution_maker 0 100 0 0 0 0 0;
  assert_equal 2 @@ distribution_maker 0 0 100 0 0 0 0;
  assert_equal 3 @@ distribution_maker 0 0 0 100 0 0 0;
  assert_equal 4 @@ distribution_maker 0 0 0 0 100 0 0;
  assert_equal 5 @@ distribution_maker 0 0 0 0 0 100 0;
  assert_equal 6 @@ distribution_maker 0 0 0 0 0 0 100

let new_lib_test =
  "new lib test"
  >: test_list
       [
         "Desome" >:: test_desome;
         "Invalid Desome" >:: test_invalid_desome;
         "N Gram" >:: test_ngrams;
         "Test Distribution Maker" >:: test_distribution_maker;
         "Merge Distribution" >:: test_merge_distribution;
         " ALL invalid cases" >:: test_new_invalid;
         "AI Move" >:: test_ai_move;
         "AI Dist Move" >:: test_ai_dist_move;
         "History to Board" >:: test_history_to_board;
         "Winning Sequence" >:: test_wining_sequence;
         "Pair" >:: test_pair;
         "Player1 Second Move" >:: test_player1_second_move;
         "Player2 First Move" >:: test_player2_first_move;
         "Test Is Valid Move" >:: test_ai_is_valid_move;
         "Test Get Last N Moves" >:: test_get_last_n_moves;
       ]

let game_over_board1 =
  [
    [ 0; 1; 1; 1; 1; 0; 0 ];
    [ 0; 2; 2; 2; 0; 0; 0 ];
    [ 0; 0; 0; 0; 0; 0; 0 ];
    [ 0; 0; 0; 0; 0; 0; 0 ];
    [ 0; 0; 0; 0; 0; 0; 0 ];
    [ 0; 0; 0; 0; 0; 0; 0 ];
  ]

let game_over_board2 =
  [
    [ 0; 1; 2; 0; 0; 0; 0 ];
    [ 0; 1; 2; 0; 0; 0; 0 ];
    [ 0; 1; 2; 0; 0; 0; 0 ];
    [ 0; 1; 0; 0; 0; 0; 0 ];
    [ 0; 0; 0; 0; 0; 0; 0 ];
    [ 0; 0; 0; 0; 0; 0; 0 ];
  ]

let game_over_board3 =
  [
    [ 0; 1; 2; 1; 2; 0; 0 ];
    [ 0; 2; 1; 2; 2; 0; 0 ];
    [ 0; 0; 0; 1; 1; 0; 0 ];
    [ 0; 0; 0; 0; 1; 0; 0 ];
    [ 0; 0; 0; 0; 0; 0; 0 ];
    [ 0; 0; 0; 0; 0; 0; 0 ];
  ]

let game_over_board4 =
  [
    [ 0; 1; 2; 2; 2; 2; 0 ];
    [ 0; 1; 1; 0; 0; 0; 0 ];
    [ 0; 1; 0; 0; 0; 0; 0 ];
    [ 0; 0; 0; 0; 0; 0; 0 ];
    [ 0; 0; 0; 0; 0; 0; 0 ];
    [ 0; 0; 0; 0; 0; 0; 0 ];
  ]

let game_over_basic _ =
  set_player 1;
  assert_equal (true, 1) @@ is_game_over game_over_board1 [];
  set_player 1;
  assert_equal (true, 1) @@ is_game_over game_over_board2 [];
  set_player 1;
  assert_equal (true, 1) @@ is_game_over game_over_board3 [];
  set_player 2;
  assert_equal (true, 2) @@ is_game_over game_over_board4 []

(* Actual game played with myself *)
let game_over_board5 =
  [
    [ 1; 2; 1; 1; 2; 1; 1 ];
    [ 2; 2; 1; 2; 1; 2; 2 ];
    [ 1; 1; 1; 2; 2; 1; 1 ];
    [ 0; 2; 2; 1; 2; 1; 2 ];
    [ 0; 0; 0; 0; 0; 2; 1 ];
    [ 0; 0; 0; 0; 0; 0; 2 ];
  ]

let game_over_board6 =
  [
    [ 1; 2; 1; 1; 2; 1; 1 ];
    [ 2; 2; 1; 2; 1; 2; 2 ];
    [ 1; 1; 1; 2; 2; 1; 1 ];
    [ 0; 2; 2; 1; 2; 1; 1 ];
    [ 0; 0; 0; 0; 2; 2; 0 ];
    [ 0; 0; 0; 0; 0; 0; 0 ];
  ]

let game_over_board7 =
  [
    [ 1; 2; 1; 1; 2; 1; 1 ];
    [ 2; 2; 1; 2; 1; 2; 2 ];
    [ 1; 1; 1; 2; 2; 1; 1 ];
    [ 1; 2; 2; 1; 2; 1; 2 ];
    [ 0; 0; 0; 0; 2; 2; 1 ];
    [ 0; 0; 0; 0; 0; 0; 0 ];
  ]

let game_over_complex _ =
  set_player 2;
  assert_equal (true, 2) @@ is_game_over game_over_board5 [];
  set_player 1;
  assert_equal (true, 1) @@ is_game_over game_over_board6 [];
  set_player 1;
  assert_equal (true, 1) @@ is_game_over game_over_board7 [];
  set_player 1;
  assert_equal (true, -1) @@ is_game_over empty full_h

let move_board1 =
  [
    [ 0; 1; 2; 2; 2; 0; 0 ];
    [ 0; 1; 1; 0; 0; 0; 0 ];
    [ 0; 1; 2; 0; 0; 0; 0 ];
    [ 0; 2; 1; 0; 0; 0; 0 ];
    [ 0; 1; 2; 0; 0; 0; 0 ];
    [ 0; 2; 1; 0; 0; 0; 0 ];
  ]

let move_board1' =
  [
    [ 0; 1; 2; 2; 2; 0; 0 ];
    [ 0; 1; 1; 1; 0; 0; 0 ];
    [ 0; 1; 2; 0; 0; 0; 0 ];
    [ 0; 2; 1; 0; 0; 0; 0 ];
    [ 0; 1; 2; 0; 0; 0; 0 ];
    [ 0; 2; 1; 0; 0; 0; 0 ];
  ]

let move_board1'' =
  [
    [ 0; 1; 2; 2; 2; 0; 0 ];
    [ 0; 1; 1; 1; 2; 0; 0 ];
    [ 0; 1; 2; 0; 0; 0; 0 ];
    [ 0; 2; 1; 0; 0; 0; 0 ];
    [ 0; 1; 2; 0; 0; 0; 0 ];
    [ 0; 2; 1; 0; 0; 0; 0 ];
  ]

let move_board2 =
  [
    [ 0; 1; 2; 2; 2; 1; 0 ];
    [ 0; 1; 1; 0; 1; 2; 0 ];
    [ 0; 1; 2; 0; 2; 1; 0 ];
    [ 0; 2; 1; 0; 2; 1; 0 ];
    [ 0; 1; 2; 0; 1; 2; 0 ];
    [ 0; 2; 1; 0; 2; 2; 0 ];
  ]

let make_move_invalid _ =
  set_player 1;
  assert_equal None @@ make_move move_board1 1;
  set_player 1;
  assert_equal None @@ make_move move_board1 2;
  set_player 1;
  assert_equal None @@ make_move move_board2 1;
  set_player 1;
  assert_equal None @@ make_move move_board2 2;
  set_player 1;
  assert_equal None @@ make_move move_board2 4;
  set_player 1;
  assert_equal None @@ make_move move_board2 5

let make_move_valid _ =
  set_player 1;
  assert_equal (Some move_board1') @@ make_move move_board1 3;
  set_player 2;
  assert_equal (Some move_board1'') @@ make_move move_board1' 4

let change_board1 =
  [
    [ 0; 1; 2; 2; 2; 1; 0 ];
    [ 0; 1; 1; 0; 1; 2; 0 ];
    [ 0; 1; 2; 0; 2; 1; 0 ];
    [ 0; 2; 1; 0; 2; 1; 0 ];
    [ 0; 1; 2; 0; 1; 2; 0 ];
    [ 0; 2; 1; 0; 2; 2; 0 ];
  ]

let change_board1' =
  [
    [ 0; 1; 2; 2; 2; 1; 0 ];
    [ 0; 1; 1; 0; 1; 2; 0 ];
    [ 0; 1; 2; 0; 2; 1; 0 ];
    [ 0; 2; 1; 1; 2; 1; 0 ];
    [ 0; 1; 2; 0; 1; 2; 0 ];
    [ 0; 2; 1; 0; 2; 2; 0 ];
  ]

let change_board1'' =
  [
    [ 0; 1; 2; 2; 2; 1; 0 ];
    [ 0; 1; 1; 0; 1; 2; 0 ];
    [ 0; 1; 2; 0; 2; 1; 0 ];
    [ 0; 2; 1; 1; 2; 1; 0 ];
    [ 0; 1; 2; 0; 1; 2; 0 ];
    [ 0; 2; 1; 0; 2; 2; 0 ];
  ]

let change_position _ =
  assert_equal change_board1' @@ change_pos 0 0 (3, 3) 1 change_board1;
  assert_equal change_board1'' @@ change_pos 0 0 (6, 5) 0 change_board1'

let set_player _ =
  set_player 1;
  assert_equal !curr_player @@ 1;
  set_player 2;
  assert_equal !curr_player @@ 2

let test_invalid_position _ =
  let invalid_get _ = get_pos (-1, -1) move_board1 in
  assert_raises (Failure "Invalid position") @@ invalid_get;
  let invalid_get' _ = get_pos (-1, 8) move_board1 in
  assert_raises (Failure "Invalid position") @@ invalid_get';
  let invalid_get'' _ = get_pos (10, -1) move_board1 in
  assert_raises (Failure "Invalid position") @@ invalid_get'';
  let invalid_row _ = change_row 8 2 [ 1; 0; 0; 1; 2; 1; 1 ] in
  assert_raises (Failure "Invalid position") @@ invalid_row;
  let invalid_row' _ = change_row 20 2 [ 1; 0; 0; 1; 2; 1; 1 ] in
  assert_raises (Failure "Invalid position") @@ invalid_row';
  let invalid_position _ = change_pos 0 0 (6, 7) 0 move_board1 in
  assert_raises (Failure "Invalid position") @@ invalid_position;
  let invalid_position' _ = change_pos 0 0 (-2, 7) 0 move_board1 in
  assert_raises (Failure "Invalid position") @@ invalid_position';
  let invalid_position'' _ = change_pos 0 0 (0, -3) 0 move_board1 in
  assert_raises (Failure "Invalid position") @@ invalid_position''

let invalid_board1 = [ [ 1; 0; 2 ]; [ 0; 0; 1 ]; [ 1; 1; 1 ] ]

let invalid_board2 = [ [ 1 ] ]

let invalid_board3 = [ [] ]

let test_invalid_board _ =
  let invalid_board _ = change_pos 0 0 (4, 4) 2 invalid_board1 in
  assert_raises (Failure "Invalid board") @@ invalid_board;
  let invalid_board' _ = change_pos 0 0 (4, 4) 2 invalid_board2 in
  assert_raises (Failure "Invalid board") @@ invalid_board';
  let invalid_board'' _ = change_pos 0 0 (4, 4) 2 invalid_board3 in
  assert_raises (Failure "Invalid board") @@ invalid_board''

let board_test =
  "Board tests"
  >: test_list
       [
         "Game over basic" >:: game_over_basic;
         "Game over complex" >:: game_over_complex;
         "Make move invalid" >:: make_move_invalid;
         "Make move valid" >:: make_move_valid;
         "Change position" >:: change_position;
         "Set player" >:: set_player;
         "Invalid position" >:: test_invalid_position;
         "Invalid board" >:: test_invalid_board;
       ]

let series =
  "Assignment4 P1 & P2 Tests" >::: [ basic_lib_test; new_lib_test; board_test ]

let () = run_test_tt_main series
