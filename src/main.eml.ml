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
  <html>
  <body>
%   let rec displayRow row =
%     match row with
%     | [] -> 
        <span>|</span></br>
%     | 0 :: tl -> 
        <span style="margin: 0 auto" >| --</span>
%     displayRow tl
%     | 1 :: tl -> 
        <span style="margin: 0 autor" >| O </span>
%     displayRow tl
%     | 2 :: tl -> 
        <span style="margin: 0 auto" >| X </span>
%     displayRow tl
%     | _ :: tl -> 
        <span style="margin: 0 auto" >| ! </span>
%     displayRow tl
%   in
%   let rec displayTable gameboard =
%   match gameboard with
%   | hd :: tl -> displayRow hd; displayTable tl;
%   | [] -> 
        </br>
%   in
%   begin match message with
%   | None -> ()
%   | Some message ->
%       displayTable message
%   end;
%   begin match winner.contents with
%   | 0 ->
      <%s! Dream.form_tag ~action:"/" request %>
        <p>Enter your move:</p></br>
        <input name="move" autofocus>
      </form> 
%   | _ ->
      <p>Player <%i winner.contents %> is the <b>winner</b>!!!</p>
%   end;
%   let game_history_length = List.length game_history.contents in
    <p> <%i game_history_length %> moves so far!</p>
    </body> 
    </html>

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
