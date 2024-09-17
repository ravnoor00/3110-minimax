open Final_3110.State
open OUnit2
open Final_3110.Minimax
open Final_3110.Easy

let in_prog_status _ =
  let board = create_board () in
  assert (get_status board = InProgress)

let vertical_win_red_board =
  let board = create_board () in
  let board = make_move board Red 0 in
  let board = make_move board Blue 2 in
  let board = make_move board Red 0 in
  let board = make_move board Blue 2 in
  let board = make_move board Red 0 in
  let board = make_move board Blue 2 in
  make_move board Red 0

let vertical_win_blue_board =
  let board = create_board () in
  let board = make_move board Blue 0 in
  let board = make_move board Red 2 in
  let board = make_move board Blue 0 in
  let board = make_move board Red 2 in
  let board = make_move board Blue 0 in
  let board = make_move board Red 2 in
  make_move board Blue 0

let diagonal_win_board_blue =
  let board = create_board () in
  let board = make_move board Red 1 in
  let board = make_move board Blue 0 in
  let board = make_move board Red 2 in
  let board = make_move board Blue 1 in
  let board = make_move board Red 2 in
  let board = make_move board Blue 3 in
  let board = make_move board Red 3 in
  let board = make_move board Blue 2 in
  let board = make_move board Red 3 in
  make_move board Blue 3

let diagonal_win_board_red =
  let board = create_board () in
  let board = make_move board Blue 1 in
  let board = make_move board Red 0 in
  let board = make_move board Blue 2 in
  let board = make_move board Red 1 in
  let board = make_move board Blue 2 in
  let board = make_move board Red 3 in
  let board = make_move board Blue 3 in
  let board = make_move board Red 2 in
  let board = make_move board Blue 3 in
  make_move board Red 3

let draw_results_board =
  let board = create_board () in
  let board = make_move board Blue 0 in
  let board = make_move board Blue 0 in
  let board = make_move board Blue 0 in
  let board = make_move board Red 0 in
  let board = make_move board Red 0 in
  let board = make_move board Red 0 in
  let board = make_move board Red 1 in
  let board = make_move board Red 1 in
  let board = make_move board Red 1 in
  let board = make_move board Blue 1 in
  let board = make_move board Blue 1 in
  let board = make_move board Blue 1 in
  let board = make_move board Blue 2 in
  let board = make_move board Blue 2 in
  let board = make_move board Blue 2 in
  let board = make_move board Red 2 in
  let board = make_move board Red 2 in
  let board = make_move board Red 2 in
  let board = make_move board Red 3 in
  let board = make_move board Red 3 in
  let board = make_move board Red 3 in
  let board = make_move board Blue 3 in
  let board = make_move board Blue 3 in
  let board = make_move board Blue 3 in
  let board = make_move board Blue 4 in
  let board = make_move board Blue 4 in
  let board = make_move board Blue 4 in
  let board = make_move board Red 4 in
  let board = make_move board Red 4 in
  let board = make_move board Red 4 in
  let board = make_move board Red 5 in
  let board = make_move board Red 5 in
  let board = make_move board Red 5 in
  let board = make_move board Blue 5 in
  let board = make_move board Blue 5 in
  let board = make_move board Blue 5 in
  let board = make_move board Blue 6 in
  let board = make_move board Blue 6 in
  let board = make_move board Blue 6 in
  let board = make_move board Red 6 in
  let board = make_move board Red 6 in
  let board = make_move board Red 6 in
  let board = make_move board Red 7 in
  let board = make_move board Red 7 in
  let board = make_move board Red 7 in
  let board = make_move board Blue 7 in
  let board = make_move board Blue 7 in
  let board = make_move board Blue 7 in
  let board = make_move board Blue 8 in
  let board = make_move board Blue 8 in
  let board = make_move board Blue 8 in
  let board = make_move board Red 8 in
  let board = make_move board Red 8 in
  make_move board Red 8

let horizontal_win_board_red =
  let board = create_board () in
  let board = make_move board Red 0 in
  let board = make_move board Red 1 in
  let board = make_move board Red 2 in
  make_move board Red 3

let horizontal_win_board_blue =
  let board = create_board () in
  let board = make_move board Blue 0 in
  let board = make_move board Blue 1 in
  let board = make_move board Blue 2 in
  make_move board Blue 3

let horizontal_win_red _ =
  assert (get_status horizontal_win_board_red = Won Red)

let horizontal_win_blue _ =
  assert (get_status horizontal_win_board_blue = Won Blue)

let vertical_win_red _ = assert (get_status vertical_win_red_board = Won Red)
let vertical_win_blue _ = assert (get_status vertical_win_blue_board = Won Blue)
let diagonal_win_blue _ = assert (get_status diagonal_win_board_blue = Won Blue)
let draw _ = assert (get_status draw_results_board = Draw)
let diagonal_win_red _ = assert (get_status diagonal_win_board_red = Won Red)

let empty_board _ =
  let board = create_board () in
  assert (Array.length board = 6);
  Array.iter
    (fun row ->
      assert (Array.length row = 9);
      Array.iter (fun cell -> assert (cell = None)) row)
    board

let string_horizontal_board_red _ =
  assert (
    ".........\n.........\n.........\n.........\n.........\nRRRR.....\n"
    = string_of_board horizontal_win_board_red)

let string_board_empty _ =
  assert (
    ".........\n.........\n.........\n.........\n.........\n.........\n"
    = string_of_board (create_board ()))

let string_vertical_board_red _ =
  assert (
    string_of_board vertical_win_red_board
    = ".........\n.........\nR........\nR.B......\nR.B......\nR.B......\n")

let string_diagonal_board_blue _ =
  assert (
    string_of_board diagonal_win_board_blue
    = ".........\n.........\n...B.....\n..BR.....\n.BRR.....\nBRRB.....\n")

let invalid_move _ =
  let board = create_board () in
  assert (get_status board = InProgress);
  try
    let _ = make_move board Red (-1) in
    assert_failure "Expected\nInvalidMove exception"
  with
  | InvalidMove _ -> ()
  | _ -> assert_failure "Unexpected exception occurred"

let is_valid_move_true _ =
  let board = create_board () in
  assert (is_valid_move board 0 = true)

let is_valid_vertical_false _ =
  let board = create_board () in
  let board = make_move board Red 0 in
  let board = make_move board Blue 0 in
  let board = make_move board Red 0 in
  let board = make_move board Blue 0 in
  let board = make_move board Red 0 in
  let board = make_move board Blue 0 in
  assert (is_valid_move board 0 = false)

let is_valid_horizontal_false _ =
  let board = create_board () in
  assert (is_valid_move board 10 = false)

let test_copy_board _ =
  assert (vertical_win_red_board = copy_board vertical_win_red_board)

let score_draw_game _ =
  let board = draw_results_board in
  assert (score board = 0)

let string_player_red _ = assert (string_of_player Red = "R")
let string_player_blue _ = assert (string_of_player Blue = "B")

let best_move_depth_1 _ =
  let board = create_board () in
  let player = Red in
  let depth = 1 in
  let col = best_move board player depth 0 in
  assert (is_valid_move board col)

let best_move_depth_2 _ =
  let board = create_board () in
  let player = Blue in
  let depth = 2 in
  let col = best_move board player depth 0 in
  assert (is_valid_move board col)

let reset_scores () =
  red_score := 0;
  blue_score := 0

let score_check _ =
  reset_scores ();
  let board = create_board () in
  let _ = make_move board Red 0 in
  let _ = make_move board Red 1 in
  let _ = make_move board Red 2 in
  let _ = make_move board Red 3 in
  assert_equal (get_status board) (Won Red);
  assert_equal !red_score 1;
  assert_equal !blue_score 0;
  reset_scores ()

let score_check_blue _ =
  reset_scores ();
  let board = create_board () in
  let _ = make_move board Blue 0 in
  let _ = make_move board Blue 1 in
  let _ = make_move board Blue 2 in
  let _ = make_move board Blue 3 in
  assert_equal (get_status board) (Won Blue);
  assert_equal !red_score 0;
  assert_equal !blue_score 1;
  reset_scores ()

let diff_score_calculation _ =
  reset_scores ();
  let board = create_board () in
  let _ = make_move board Red 0 in
  let _ = make_move board Red 1 in
  let _ = make_move board Red 2 in
  let _ = make_move board Red 3 in
  assert_equal (get_status board) (Won Red);
  assert_equal !red_score 1;
  assert_equal !blue_score 0;
  let board = create_board () in
  let _ = make_move board Blue 0 in
  let _ = make_move board Blue 1 in
  let _ = make_move board Blue 2 in
  let _ = make_move board Blue 3 in
  assert_equal (get_status board) (Won Blue);
  assert_equal !red_score 1;
  assert_equal !blue_score 1;
  reset_scores ()

let test_empty_board_score _ =
  let board = create_board () in
  assert_equal (score board) 0

let test_red_win_board_score _ =
  assert_equal (score horizontal_win_board_red) max_int

let test_blue_win_board_score _ =
  assert_equal (score horizontal_win_board_blue) min_int

let test_draw_board_score _ = assert_equal (score draw_results_board) 0

let test_in_progress_board_score _ =
  let board = create_board () in
  let _ = make_move board Red 0 in
  let _ = make_move board Blue 1 in
  assert_equal (score board) 0

let test_mixed_board_score _ =
  let board = create_board () in
  let _ = make_move board Red 0 in
  let _ = make_move board Blue 1 in
  let _ = make_move board Red 2 in
  let _ = make_move board Blue 3 in
  let _ = make_move board Red 4 in
  let _ = make_move board Blue 5 in
  let _ = make_move board Red 6 in
  assert_equal (score board) 0

let score_empty_board _ = assert (score (create_board ()) = 0)

let copy_board_diff _ =
  let original_board = create_board () in
  let copied_board = copy_board original_board in
  original_board.(0).(0) <- Some Red;
  assert (original_board <> copied_board)

let valid_move_partial_column _ =
  let board = create_board () in
  let _ = make_move board Red 0 in
  let _ = make_move board Blue 0 in
  let _ = make_move board Red 0 in
  let _ = make_move board Blue 0 in
  assert (is_valid_move board 0 = true)

let test_suite =
  "test suite"
  >::: [
         "check red score" >:: score_check;
         "status in progress" >:: in_prog_status;
         "winning vertically red" >:: vertical_win_red;
         "winning vertically blue" >:: vertical_win_blue;
         "winning diagonally blue" >:: diagonal_win_blue;
         "winning diagonally red" >:: diagonal_win_red;
         "empty board" >:: empty_board;
         "player draw" >:: draw;
         "string of empty board" >:: string_board_empty;
         "string of vertical win board" >:: string_vertical_board_red;
         "string of diagonal win board" >:: string_diagonal_board_blue;
         "invalid move" >:: invalid_move;
         "winning horizontally red" >:: horizontal_win_red;
         "winning horizontally blue" >:: horizontal_win_blue;
         "valid move is true" >:: is_valid_move_true;
         "invalid vertical move is false" >:: is_valid_vertical_false;
         "invalid horizontal move is false" >:: is_valid_horizontal_false;
         "testing copy of board" >:: test_copy_board;
         "horizontal string of board" >:: string_horizontal_board_red;
         "draw game score 0" >:: score_draw_game;
         "red player string" >:: string_player_red;
         "blue player string" >:: string_player_blue;
         "depth 1" >:: best_move_depth_1;
         "depth 2" >:: best_move_depth_2;
         "score checking" >:: score_check;
         "score checking for blue " >:: score_check_blue;
         "different score checking" >:: diff_score_calculation;
         "test_empty_board_score" >:: test_empty_board_score;
         "test_red_win_board_score" >:: test_red_win_board_score;
         "test_blue_win_board_score" >:: test_blue_win_board_score;
         "test_draw_board_score" >:: test_draw_board_score;
         "test_in_progress_board_score" >:: test_in_progress_board_score;
         "test_mixed_board_score" >:: test_mixed_board_score;
         "empty board score" >:: score_empty_board;
         "not same copy board" >:: copy_board_diff;
         "valid move for a partial column" >:: valid_move_partial_column;
       ]

let _ = run_test_tt_main test_suite
