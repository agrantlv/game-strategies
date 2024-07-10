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
  let non_win_2 =
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
      let line_str = String.init (3 * len) ~f:(fun _num -> '-') in
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
    if List.length opp_win_moves > 1
    then available_moves
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
    let losing_moves_list = losing_moves ~me game in
    List.filter available_moves_list ~f:(fun move ->
      not
        (List.exists losing_moves_list ~f:(fun losing_move ->
           Game.Position.equal losing_move move)))
  ;;

  (* let minimax ~game ~depth ~(me : Game.Piece.t) =

     ;; *)

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
         let winning_moves_1 = winning_moves ~me:piece non_win in
         print_s [%sexp (winning_moves_1 : Game.Position.t list)];
         let winning_moves_2 = winning_moves ~me:piece non_win_2 in
         print_s [%sexp (winning_moves_2 : Game.Position.t list)];
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

  let command =
    Command.group
      ~summary:"Exercises"
      [ "one", exercise_one
      ; "two", exercise_two
      ; "three", exercise_three
      ; "four", exercise_four
      ]
  ;;
end

let handle_turn (_client : unit) (_query : Rpcs.Take_turn.Query.t) =
  (* let%bind () = delay 10 in *)
  (* print_s [%message "Received query" (query : Echo.Query.t)]; *)
  let piece = Game.Piece.X in
  let position = Game.Position.{ row = 0; column = 0 } in
  let (response : Rpcs.Take_turn.Response.t) = { piece; position } in
  return response
;;

let implementations =
  Rpc.Implementations.create_exn
    ~on_unknown_rpc:`Close_connection
    ~implementations:[ Rpc.Rpc.implement Rpcs.Take_turn.rpc handle_turn ]
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
