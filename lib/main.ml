open! Core
open! Async
open! Game_strategies_common_lib

module Exercises = struct
  (* Here are some functions which know how to create a couple different
     kinds of games *)

  module Direction = struct
    type t =
      | Horizontal
      | Vertical
      | DiagonalUp
      | DiagonalDown
    [@@deriving enumerate]
  end

  let empty_game = Game.empty Game.Game_kind.Tic_tac_toe
  (* let empty_gomoku_game = Game.empty Game.Game_kind.Omok *)

  let place_piece (game : Game.t) ~piece ~position : Game.t =
    let board = Map.set game.board ~key:position ~data:piece in
    { game with board }
  ;;

  (* win for x
     X | O | X
     ---------
     O | O | X
     ---------
     O | X | X *)
  let win_for_x =
    let open Game in
    empty_game
    |> place_piece ~piece:Piece.X ~position:{ Position.row = 0; column = 0 }
    |> place_piece ~piece:Piece.O ~position:{ Position.row = 1; column = 0 }
    |> place_piece ~piece:Piece.X ~position:{ Position.row = 2; column = 2 }
    |> place_piece ~piece:Piece.O ~position:{ Position.row = 2; column = 0 }
    |> place_piece ~piece:Piece.X ~position:{ Position.row = 2; column = 1 }
    |> place_piece ~piece:Piece.O ~position:{ Position.row = 1; column = 1 }
    |> place_piece ~piece:Piece.X ~position:{ Position.row = 0; column = 2 }
    |> place_piece ~piece:Piece.O ~position:{ Position.row = 0; column = 1 }
    |> place_piece ~piece:Piece.X ~position:{ Position.row = 1; column = 2 }
  ;;

  (* draw
     X | O | X
     ---------
     O | O | X
     ---------
     O | X | O *)
  let draw_game =
    let open Game in
    empty_game
    |> place_piece ~piece:Piece.X ~position:{ Position.row = 0; column = 0 }
    |> place_piece ~piece:Piece.O ~position:{ Position.row = 1; column = 0 }
    |> place_piece ~piece:Piece.O ~position:{ Position.row = 2; column = 2 }
    |> place_piece ~piece:Piece.O ~position:{ Position.row = 2; column = 0 }
    |> place_piece ~piece:Piece.X ~position:{ Position.row = 2; column = 1 }
    |> place_piece ~piece:Piece.O ~position:{ Position.row = 1; column = 1 }
    |> place_piece ~piece:Piece.X ~position:{ Position.row = 0; column = 2 }
    |> place_piece ~piece:Piece.O ~position:{ Position.row = 0; column = 1 }
    |> place_piece ~piece:Piece.X ~position:{ Position.row = 1; column = 2 }
  ;;

  (* horizontal o win
     X | O | X
     ---------
     O | O | O
     ---------
     O | X | O *)
  let horizontal_o_win =
    let open Game in
    empty_game
    |> place_piece ~piece:Piece.X ~position:{ Position.row = 0; column = 0 }
    |> place_piece ~piece:Piece.O ~position:{ Position.row = 0; column = 1 }
    |> place_piece ~piece:Piece.X ~position:{ Position.row = 0; column = 2 }
    |> place_piece ~piece:Piece.O ~position:{ Position.row = 1; column = 0 }
    |> place_piece ~piece:Piece.O ~position:{ Position.row = 1; column = 1 }
    |> place_piece ~piece:Piece.O ~position:{ Position.row = 1; column = 2 }
    |> place_piece ~piece:Piece.O ~position:{ Position.row = 2; column = 0 }
    |> place_piece ~piece:Piece.X ~position:{ Position.row = 2; column = 1 }
    |> place_piece ~piece:Piece.O ~position:{ Position.row = 2; column = 2 }
  ;;

  (* diagonal down o win
     O | O | X
     ---------
     X | O | O
     ---------
     O | X | O *)
  let diagonal_down_o_win =
    let open Game in
    empty_game
    |> place_piece ~piece:Piece.O ~position:{ Position.row = 0; column = 0 }
    |> place_piece ~piece:Piece.O ~position:{ Position.row = 0; column = 1 }
    |> place_piece ~piece:Piece.X ~position:{ Position.row = 0; column = 2 }
    |> place_piece ~piece:Piece.X ~position:{ Position.row = 1; column = 0 }
    |> place_piece ~piece:Piece.O ~position:{ Position.row = 1; column = 1 }
    |> place_piece ~piece:Piece.O ~position:{ Position.row = 1; column = 2 }
    |> place_piece ~piece:Piece.O ~position:{ Position.row = 2; column = 0 }
    |> place_piece ~piece:Piece.X ~position:{ Position.row = 2; column = 1 }
    |> place_piece ~piece:Piece.O ~position:{ Position.row = 2; column = 2 }
  ;;

  (* non win
     X |   |
     ---------
     O |   |
     ---------
     O |   | X
  *)
  let non_win =
    let open Game in
    empty_game
    |> place_piece ~piece:Piece.X ~position:{ Position.row = 0; column = 0 }
    |> place_piece ~piece:Piece.O ~position:{ Position.row = 1; column = 0 }
    |> place_piece ~piece:Piece.X ~position:{ Position.row = 2; column = 2 }
    |> place_piece ~piece:Piece.O ~position:{ Position.row = 2; column = 0 }
  ;;

  (* non win 2
     X | O | O
     ---------
     O | O |
     ---------
     |   | X
  *)
  let _non_win_2 =
    let open Game in
    empty_game
    |> place_piece ~piece:Piece.X ~position:{ Position.row = 0; column = 0 }
    |> place_piece ~piece:Piece.O ~position:{ Position.row = 0; column = 1 }
    |> place_piece ~piece:Piece.O ~position:{ Position.row = 0; column = 2 }
    |> place_piece ~piece:Piece.O ~position:{ Position.row = 1; column = 0 }
    |> place_piece ~piece:Piece.O ~position:{ Position.row = 1; column = 1 }
    |> place_piece ~piece:Piece.X ~position:{ Position.row = 2; column = 2 }
  ;;

  (* test losing move
     X |   | O
     ---------
     | X |
     ---------
     |   |
  *)
  let test_losing_move =
    let open Game in
    empty_game
    |> place_piece ~piece:Piece.X ~position:{ Position.row = 0; column = 0 }
    |> place_piece ~piece:Piece.O ~position:{ Position.row = 0; column = 2 }
    |> place_piece ~piece:Piece.X ~position:{ Position.row = 1; column = 1 }
  ;;

  let illegal_game =
    let open Game in
    empty_game
    |> place_piece ~piece:Piece.X ~position:{ Position.row = 0; column = 3 }
  ;;

  let print_game (game : Game.t) =
    let pos_map = game.board in
    let len = Game.Game_kind.board_length game.game_kind in
    let board_list =
      List.init len ~f:(fun row ->
        List.init len ~f:(fun col ->
          match Map.find pos_map { row; column = col } with
          | Some piece ->
            (match piece with Game.Piece.O -> "O" | Game.Piece.X -> "X")
          | None -> " "))
    in
    List.iteri board_list ~f:(fun row col_list ->
      List.iteri col_list ~f:(fun col piece ->
        print_string piece;
        if not (col = len - 1) then print_string " | " else print_newline ());
      let line_str = String.init (4 * len) ~f:(fun _num -> '-') in
      if not (row = len - 1) then print_endline line_str)
  ;;

  let%expect_test "print_win_for_x" =
    print_game win_for_x;
    [%expect
      {|
      X | O | X
      ---------
      O | O | X
      ---------
      O | X | X
      |}];
    return ()
  ;;

  let%expect_test "print_non_win" =
    print_game non_win;
    [%expect
      {|
      X |   |
      ---------
      O |   |
      ---------
      O |   | X
      |}];
    return ()
  ;;

  let all_moves (game : Game.t) =
    let len = Game.Game_kind.board_length game.game_kind in
    List.concat
      (List.init len ~f:(fun row ->
         List.init len ~f:(fun col -> Game.Position.{ row; column = col })))
  ;;

  (* Exercise 1 *)
  let available_moves (game : Game.t) : Game.Position.t list =
    let pos_map = game.board in
    let all_moves = all_moves game in
    List.filter all_moves ~f:(fun possible_move ->
      not (Map.mem pos_map possible_move))
  ;;

  let rec create_winning_list
    ~(position : Game.Position.t)
    ~len
    ~(direction : Direction.t)
    : Game.Position.t list
    =
    if len = 1
    then [ position ]
    else
      [ position ]
      @ create_winning_list
          ~position:
            (match direction with
             | Horizontal -> Game.Position.right position
             | Vertical -> Game.Position.down position
             | DiagonalUp -> Game.Position.up (Game.Position.right position)
             | DiagonalDown ->
               Game.Position.down (Game.Position.right position))
          ~len:(len - 1)
          ~direction
  ;;

  (* Exercise 2 *)
  let evaluate (game : Game.t) : Game.Evaluation.t =
    let pos_map = game.board in
    let previous_moves = Map.keys pos_map in
    (* tries to find out of bounds moves *)
    match
      List.find previous_moves ~f:(fun move ->
        not (Game.Position.in_bounds move ~game_kind:game.game_kind))
    with
    | Some _move -> Game.Evaluation.Illegal_move
    | None ->
      let win_len = Game.Game_kind.win_length game.game_kind in
      (match
         List.find previous_moves ~f:(fun position ->
           (* partial application of function*)
           let create_winning_list =
             create_winning_list ~position ~len:win_len
           in
           let check_lists =
             List.map Direction.all ~f:(fun direction ->
               create_winning_list ~direction)
           in
           List.exists check_lists ~f:(fun potential_list ->
             let (one_piece : Game.Piece.t) =
               Map.find_exn pos_map (List.hd_exn potential_list)
             in
             List.for_all potential_list ~f:(fun check_pos ->
               Map.mem pos_map check_pos
               && Game.Piece.equal one_piece (Map.find_exn pos_map check_pos))))
       with
       | Some (position : Game.Position.t) ->
         let winning_piece = Map.find_exn pos_map position in
         Game.Evaluation.Game_over { winner = Some winning_piece }
       | None ->
         if (* if no moves available then game over, no winner *)
            List.is_empty (available_moves game)
         then Game_over { winner = None }
         else Game.Evaluation.Game_continues)
  ;;

  let rec in_a_row (game : Game.t) ~(me : Game.Piece.t) ~(len : int) : int =
    let pos_map = game.board in
    let previous_moves = Map.keys pos_map in
    if len = 1
    then 0
    else (
      match
        List.find previous_moves ~f:(fun position ->
          (* partial application of function*)
          let create_winning_list = create_winning_list ~position ~len in
          let check_lists =
            List.map Direction.all ~f:(fun direction ->
              create_winning_list ~direction)
          in
          List.exists check_lists ~f:(fun potential_list ->
            let (one_piece : Game.Piece.t) =
              Map.find_exn pos_map (List.hd_exn potential_list)
            in
            List.for_all potential_list ~f:(fun check_pos ->
              Map.mem pos_map check_pos
              && Game.Piece.equal one_piece (Map.find_exn pos_map check_pos))))
      with
      | Some { row; column } ->
        if Game.Piece.equal me (Map.find_exn pos_map { row; column })
        then (len * 100) - abs (7 - row) - abs (7 - column)
        else (-1 * len * 100) + abs (7 - row) + abs (7 - column)
      | None -> in_a_row game ~me ~len:(len - 1))
  ;;

  (* Exercise 3 *)
  let winning_moves ~(me : Game.Piece.t) (game : Game.t)
    : Game.Position.t list
    =
    let available_move_list = available_moves game in
    List.filter available_move_list ~f:(fun position ->
      let new_board = place_piece game ~piece:me ~position in
      match evaluate new_board with
      | Game.Evaluation.Game_over { winner } ->
        (match winner with
         | Some piece -> Game.Piece.equal piece me
         | None -> false)
      | _ -> false)
  ;;

  (* Exercise 4 *)
  let losing_moves ~(me : Game.Piece.t) (game : Game.t)
    : Game.Position.t list
    =
    let available_moves = available_moves game in
    let opp_win_moves = winning_moves ~me:(Game.Piece.flip me) game in
    (* print_endline "opponent winning moves:";
       print_s [%message (opp_win_moves : Game.Position.t list)]; *)
    let win_len = List.length opp_win_moves in
    if win_len > 1
    then available_moves
    else if win_len = 0
    then []
    else
      List.filter available_moves ~f:(fun move ->
        not
          (List.exists opp_win_moves ~f:(fun opp_move ->
             Game.Position.equal move opp_move)))
  ;;

  let available_moves_that_do_not_immediately_lose
    ~(me : Game.Piece.t)
    (game : Game.t)
    =
    let available_moves_list = available_moves game in
    (* print_endline "available moves:";
       print_s [%sexp (available_moves_list : Game.Position.t list)]; *)
    let losing_moves_list = losing_moves ~me game in
    (* print_endline "losing moves:";
       print_s [%sexp (losing_moves_list : Game.Position.t list)]; *)
    List.filter available_moves_list ~f:(fun move ->
      not
        (List.exists losing_moves_list ~f:(fun losing_move ->
           Game.Position.equal losing_move move)))
  ;;

  let score_game ~(cur_piece : Game.Piece.t) (game : Game.t) : int =
    match evaluate game with
    | Game_over { winner } ->
      (match winner with
       | Some piece ->
         (* ignore cur_piece; *)
         (* Int.max_value *)
         if Game.Piece.equal piece cur_piece
         then Int.max_value
         else Int.min_value
       | None -> 0)
    | Game_continues ->
      (match game.game_kind with
       | Tic_tac_toe -> 0
       | Omok ->
         (* let pos_map = game.board in
            Map.counti pos_map ~f:(fun ~key:position ~data:check_piece ->
            let surrounding_functions = Game.Position.all_offsets in
            let surrounding_pieces = List.map surrounding_functions ~f:(fun offset ->
            offset position
            ) in

            if (Game.Piece.equal cur_piece check_piece) && (Map.existsi pos_map ~f:(fun ~key:position2 ~data:

            )
            ) then
            true
            else
            false
            ) *)
         in_a_row
           game
           ~me:cur_piece
           ~len:(Game.Game_kind.win_length game.game_kind))
    | Illegal_move -> 0
  ;;

  let rec minimax
    ~(game : Game.t)
    ~depth
    ~(cur_piece : Game.Piece.t)
    (maximizing_player : bool)
    : int
    =
    let game_over =
      match evaluate game with
      | Game_over { winner } ->
        ignore winner;
        true
      | _ -> false
    in
    if depth = 0 || game_over
    then score_game game ~cur_piece
    else (
      let possible_moves =
        available_moves_that_do_not_immediately_lose ~me:cur_piece game
      in
      if maximizing_player
      then (
        let score_list =
          List.map possible_moves ~f:(fun move ->
            minimax
              ~game:(place_piece game ~piece:cur_piece ~position:move)
              ~cur_piece:(Game.Piece.flip cur_piece)
              ~depth:(depth - 1)
              false)
        in
        match List.max_elt score_list ~compare:Int.compare with
        | Some max_elem -> max_elem
        | None -> Int.min_value)
      else (
        (* minimizing player *)
        let score_list =
          List.map possible_moves ~f:(fun move ->
            minimax
              ~game:(place_piece game ~piece:cur_piece ~position:move)
              ~cur_piece:(Game.Piece.flip cur_piece)
              ~depth:(depth - 1)
              true)
        in
        match List.min_elt score_list ~compare:Int.compare with
        | Some max_elem -> max_elem
        | None -> Int.max_value))
  ;;

  let exercise_one =
    Command.async
      ~summary:"Exercise 1: Where can I move?"
      (let%map_open.Command () = return () in
       fun () ->
         print_game win_for_x;
         let moves = available_moves win_for_x in
         print_endline "win_for_x moves: ";
         print_s [%sexp (moves : Game.Position.t list)];
         print_game non_win;
         let moves = available_moves non_win in
         print_endline "non_win moves: ";
         print_s [%sexp (moves : Game.Position.t list)];
         return ())
  ;;

  let exercise_two =
    Command.async
      ~summary:"Exercise 2: Is the game over?"
      (let%map_open.Command () = return () in
       fun () ->
         print_endline "x should win:";
         let evaluation = evaluate win_for_x in
         print_s [%sexp (evaluation : Game.Evaluation.t)];
         print_endline "game should continue:";
         let evaluation = evaluate non_win in
         print_s [%sexp (evaluation : Game.Evaluation.t)];
         print_endline "empty board, game should continue:";
         let evaluation = evaluate empty_game in
         print_s [%sexp (evaluation : Game.Evaluation.t)];
         print_endline "draw, should be game over no winner:";
         let evaluation = evaluate draw_game in
         print_s [%sexp (evaluation : Game.Evaluation.t)];
         print_endline "horizontal o win:";
         let evaluation = evaluate horizontal_o_win in
         print_s [%sexp (evaluation : Game.Evaluation.t)];
         print_endline "diagonal down o win:";
         let evaluation = evaluate diagonal_down_o_win in
         print_s [%sexp (evaluation : Game.Evaluation.t)];
         print_endline "illegal game:";
         let evaluation = evaluate illegal_game in
         print_s [%sexp (evaluation : Game.Evaluation.t)];
         return ())
  ;;

  let piece_flag =
    let open Command.Param in
    flag
      "piece"
      (required (Arg_type.create Game.Piece.of_string))
      ~doc:
        ("PIECE "
         ^ (Game.Piece.all
            |> List.map ~f:Game.Piece.to_string
            |> String.concat ~sep:", "))
  ;;

  let exercise_three =
    Command.async
      ~summary:"Exercise 3: Is there a winning move?"
      (let%map_open.Command () = return ()
       and piece = piece_flag in
       fun () ->
         (* let winning_moves_1 = winning_moves ~me:piece non_win in
            print_s [%sexp (winning_moves_1 : Game.Position.t list)];
            let winning_moves_2 = winning_moves ~me:piece non_win_2 in
            print_s [%sexp (winning_moves_2 : Game.Position.t list)]; *)
         print_endline
           "testing available moves that don't immediately lose: should be \
            (1, 1)";
         let dont_lose =
           available_moves_that_do_not_immediately_lose ~me:piece non_win
         in
         print_endline "available moves that dont immediately lose:";
         print_s [%sexp (dont_lose : Game.Position.t list)];
         print_endline
           "testing available moves that don't immediately lose: should be \
            all";
         let dont_lose_2 =
           available_moves_that_do_not_immediately_lose ~me:piece empty_game
         in
         print_endline "available moves that dont immediately lose:";
         print_s [%sexp (dont_lose_2 : Game.Position.t list)];
         return ())
  ;;

  let exercise_four =
    Command.async
      ~summary:"Exercise 4: Is there a losing move?"
      (let%map_open.Command () = return ()
       and piece = piece_flag in
       fun () ->
         let losing_moves_1 = losing_moves ~me:piece test_losing_move in
         print_s [%sexp (losing_moves_1 : Game.Position.t list)];
         return ())
  ;;

  let shrink_moves ~(game : Game.t) ~possible_moves =
    match game.game_kind with
    | Tic_tac_toe -> possible_moves
    | Omok ->
      let move_list = Map.keys game.board in
      let row_list =
        List.map move_list ~f:(fun Game.Position.{ row; column = _ } -> row)
      in
      let col_list =
        List.map move_list ~f:(fun Game.Position.{ row = _; column } ->
          column)
      in
      let min_row =
        match List.min_elt row_list ~compare:Int.compare with
        | Some row -> row
        | None -> 7
      in
      let max_row =
        match List.max_elt row_list ~compare:Int.compare with
        | Some row -> row
        | None -> 7
      in
      let min_col =
        match List.min_elt col_list ~compare:Int.compare with
        | Some row -> row
        | None -> 7
      in
      let max_col =
        match List.max_elt col_list ~compare:Int.compare with
        | Some row -> row
        | None -> 7
      in
      let radius = 3 in
      List.filter possible_moves ~f:(fun Game.Position.{ row; column } ->
        row >= min_row - radius
        && row <= max_row + radius
        && column >= min_col - radius
        && column <= max_col + radius)
  ;;

  let handle_turn (_client : unit) (query : Rpcs.Take_turn.Query.t)
    : Rpcs.Take_turn.Response.t Deferred.t
    =
    (* let%bind () = delay 10 in *)
    (* print_s [%message "Received query" (query : Echo.Query.t)]; *)
    let piece = query.you_play in
    let game = query.game in
    let game_kind = game.game_kind in
    let depth = match game_kind with Tic_tac_toe -> 9 | Omok -> 1 in
    (* changes possible moves to be a win, move that doesn't lose, or any available move*)
    let available_moves_that_dont_lose =
      available_moves_that_do_not_immediately_lose game ~me:piece
    in
    let win_moves = winning_moves game ~me:piece in
    let possible_moves =
      if not (List.is_empty win_moves)
      then win_moves
      else if not (List.is_empty available_moves_that_dont_lose)
      then available_moves_that_dont_lose
      else available_moves game
    in
    (* print_s [%message (possible_moves : Game.Position.t list)]; *)
    (* shrinks moves even further for Omok *)
    let new_moves = shrink_moves ~game ~possible_moves in
    (* print_s [%message (possible_moves : Game.Position.t list)]; *)
    (* print_s [%message (new_moves : Game.zPosition.t list)]; *)
    let move_priority_list =
      List.map new_moves ~f:(fun move ->
        let prio =
          minimax
            ~game:(place_piece game ~piece ~position:move)
            ~cur_piece:(Game.Piece.flip piece)
            ~depth
            false
        in
        move, prio)
    in
    print_game game;
    print_s [%message (move_priority_list : (Game.Position.t * int) list)];
    let position =
      List.max_elt move_priority_list ~compare:(fun tuple1 tuple2 ->
        let min1 = match tuple1 with _, prio -> prio in
        let min2 = match tuple2 with _, prio2 -> prio2 in
        Int.compare min1 min2)
    in
    let position_answer =
      match position with
      | Some tuple -> (match tuple with move, _ -> move)
      | None -> Game.Position.{ row = -1; column = -1 }
    in
    let (response : Rpcs.Take_turn.Response.t) =
      if Game.Game_kind.equal game_kind Omok && Map.is_empty game.board
      then { piece; position = { row = 7; column = 7 } }
      else { piece; position = position_answer }
    in
    return response
  ;;

  (*
     let gomoku =
    Command.async
      ~summary:"Exercise 4: Is there a losing move?"
      (let%map_open.Command () = return ()
       and piece = piece_flag in
       fun () ->
         let open Game in
         let board =
           empty_gomoku_game
           |> place_piece
                ~piece:Piece.X
                ~position:{ Position.row = 7; column = 7 }
         in
         let piece = Game.Piece.O in
         let (query : Rpcs.Take_turn.Query.t) = {game = board; you_play = piece}
         return ())
  ;; *)

  let command =
    Command.group
      ~summary:"Exercises"
      [ "one", exercise_one
      ; "two", exercise_two
      ; "three", exercise_three
      ; "four", exercise_four (* ; "gomoku", gomoku *)
      ]
  ;;
end

let implementations =
  Rpc.Implementations.create_exn
    ~on_unknown_rpc:`Close_connection
    ~implementations:
      [ Rpc.Rpc.implement Rpcs.Take_turn.rpc Exercises.handle_turn ]
;;

let command_play =
  Command.async
    ~summary:"Play"
    (let%map_open.Command () = return ()
     (*and controller = flag "-controller" (required host_and_port) ~doc:"_
       host_and_port of controller" *)
     and port = flag "-port" (required int) ~doc:"_ port to listen on" in
     fun () ->
       (* We should start listing on the supplied [port], ready to handle
          incoming queries for [Take_turn] and [Game_over]. We should also
          connect to the controller and send a [Start_game] to initiate the
          game. *)
       let%bind server =
         Rpc.Connection.serve
           ~implementations
           ~initial_connection_state:(fun _client_identity _client_addr ->
             (* This constructs the "client" values which are passed to the
                implementation function above. We're just using unit for
                now. *)
             ())
           ~where_to_listen:(Tcp.Where_to_listen.of_port port)
           ()
       in
       Tcp.Server.close_finished server)
;;

(* (* ignore controller; *) ignore port; return ()) *)

let command =
  Command.group
    ~summary:"Game Strategies"
    [ "play", command_play; "exercises", Exercises.command ]
;;
