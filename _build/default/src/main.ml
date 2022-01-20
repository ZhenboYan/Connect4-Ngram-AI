#1 "src/main.eml.ml"
open Core
open Board
open Lib

module Pos = struct
  type t = int * int [@@deriving compare, sexp]
end

module Pos_grams = N_grams (Pos)

module Dist = struct
  type t = Pos_grams.distribution [@@deriving sexp]
end

module Write_Distribution = Dist

let sexp_to_map filename =
  Write_Distribution.t_of_sexp (Sexp.load_sexp filename)

(* let ai_dist = Pos_grams.ngrams 1 1 [] *)
let ai_dist = sexp_to_map "player1.txt"

let ai_first = 1

let tempBoard = ref [[]]

let winner = ref 0

let emptyBoard = [[0; 0; 0; ai_first; 0; 0; 0];
                  [0; 0; 0; 0; 0; 0; 0];
                  [0; 0; 0; 0; 0; 0; 0];
                  [0; 0; 0; 0; 0; 0; 0];
                  [0; 0; 0; 0; 0; 0; 0];
                  [0; 0; 0; 0; 0; 0; 0]]

let show_form ?message request =
let ___eml_buffer = Buffer.create 4096 in
(Buffer.add_string ___eml_buffer "<html>\r\n<body>\r\n");
#39 "src/main.eml.ml"
    let rec displayRow row =

#40 "src/main.eml.ml"
      match row with

#41 "src/main.eml.ml"
      | [] -> 

(Buffer.add_string ___eml_buffer "        <span>|</span></br>\r\n");
#43 "src/main.eml.ml"
      | 0 :: tl -> 

(Buffer.add_string ___eml_buffer "        <span style=\"margin: 0 auto\" >| --</span>\r\n");
#45 "src/main.eml.ml"
      displayRow tl

#46 "src/main.eml.ml"
      | 1 :: tl -> 

(Buffer.add_string ___eml_buffer "        <span style=\"margin: 0 autor\" >| O </span>\r\n");
#48 "src/main.eml.ml"
      displayRow tl

#49 "src/main.eml.ml"
      | 2 :: tl -> 

(Buffer.add_string ___eml_buffer "        <span style=\"margin: 0 auto\" >| X </span>\r\n");
#51 "src/main.eml.ml"
      displayRow tl

#52 "src/main.eml.ml"
      | _ :: tl -> 

(Buffer.add_string ___eml_buffer "        <span style=\"margin: 0 auto\" >| ! </span>\r\n");
#54 "src/main.eml.ml"
      displayRow tl

#55 "src/main.eml.ml"
    in

#56 "src/main.eml.ml"
    let rec displayTable gameboard =

#57 "src/main.eml.ml"
    match gameboard with

#58 "src/main.eml.ml"
    | hd :: tl -> displayRow hd; displayTable tl;

#59 "src/main.eml.ml"
    | [] -> 

(Buffer.add_string ___eml_buffer "        </br>\r\n");
#61 "src/main.eml.ml"
    in

#62 "src/main.eml.ml"
    begin match message with

#63 "src/main.eml.ml"
    | None -> ()

#64 "src/main.eml.ml"
    | Some message ->

#65 "src/main.eml.ml"
        displayTable message

#66 "src/main.eml.ml"
    end;

#67 "src/main.eml.ml"
    begin match winner.contents with

#68 "src/main.eml.ml"
    | 0 ->

(Buffer.add_string ___eml_buffer "      ");
(Printf.bprintf ___eml_buffer "%s" (
#69 "src/main.eml.ml"
           Dream.form_tag ~action:"/" request 
));
(Buffer.add_string ___eml_buffer "\r\n      <p>Enter your move:</p></br>\r\n      <input name=\"move\" autofocus>\r\n    </form> \r\n");
#73 "src/main.eml.ml"
    | _ ->

(Buffer.add_string ___eml_buffer "      <p>Player ");
(Printf.bprintf ___eml_buffer "%i" (
#74 "src/main.eml.ml"
                    winner.contents 
));
(Buffer.add_string ___eml_buffer " is the <b>winner</b>!!!</p>\r\n");
#75 "src/main.eml.ml"
    end;

#76 "src/main.eml.ml"
    let game_history_length = List.length game_history.contents in

(Buffer.add_string ___eml_buffer "    <p> ");
(Printf.bprintf ___eml_buffer "%i" (
#77 "src/main.eml.ml"
            game_history_length 
));
(Buffer.add_string ___eml_buffer " moves so far!</p>\r\n  </body> \r\n  </html>\r\n");
(Buffer.contents ___eml_buffer)
#81 "src/main.eml.ml"

let () =
  Dream.run
  @@ Dream.logger
  @@ Dream.memory_sessions
  @@ Dream.router [

    Dream.get  "/"
      (fun request ->
        tempBoard := emptyBoard;
        set_player 1;
        if ai_first = 1 then game_history := [(0, 3)] else game_history := [];
        winner := 0;
        if ai_first = 1 then curr_player := 2 else curr_player := 1;
        Dream.html (show_form ~message:(List.rev tempBoard.contents) request));

    Dream.post "/"
      (fun request ->
        match%lwt Dream.form request with
        | `Ok ["move", move] ->
          let moveCol = int_of_string move in
          if moveCol <= 6 && moveCol >= 0 then
          (match make_move tempBoard.contents moveCol with
          | None -> ()
          | Some newBoard -> tempBoard := newBoard; 
          let gameOver, gameWinner = is_game_over newBoard game_history.contents in
          if curr_player.contents = 1 then set_player 2
          else set_player 1;
          (match gameOver with
          | false -> winner := winner.contents;
              let _, x = ai_move (List.rev game_history.contents) 12 ai_dist standard_distribution 20 in
              (match make_move tempBoard.contents x with
              | None -> ()
              | Some ainewBoard -> tempBoard := ainewBoard;
              let ai_game_over, ai_game_winner = is_game_over ainewBoard game_history.contents in
              (match ai_game_over with
              | false -> winner := winner.contents;
              | true -> winner := ai_game_winner;););
          | true -> winner := gameWinner;);
          if curr_player.contents = 1 then set_player 2
          else set_player 1;)
          else tempBoard := tempBoard.contents;
          Dream.html (show_form ~message:(List.rev tempBoard.contents) request)
        | _ ->
          Dream.empty `Bad_Request);

  ]
  @@ Dream.not_found
