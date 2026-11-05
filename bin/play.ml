(* The bias experiment in the terminal. Each finished game is saved straight away, so you
   can stop at any time. A game you quit halfway is not saved. *)
open Bias

exception Quit

let default_path = "results/my_games.csv"

let load path =
  if not (Sys.file_exists path) then []
  else
    let channel = open_in path in
    let rec read acc =
      match input_line channel with
      | line -> read (line :: acc)
      | exception End_of_file ->
          close_in channel;
          List.rev acc
    in
    match read [] with [] -> [] | _header :: lines -> List.map of_csv lines

let save path rows =
  let is_new = not (Sys.file_exists path) in
  let channel = open_out_gen [ Open_append; Open_creat ] 0o644 path in
  if is_new then output_string channel (csv_header ^ "\n");
  List.iter (fun row -> output_string channel (to_csv row ^ "\n")) rows;
  close_out channel

let ask prompt =
  print_string prompt;
  flush stdout;
  match read_line () with
  | exception End_of_file -> raise Quit
  | line -> if String.trim line = "q" then raise Quit else line

let numbers line =
  String.split_on_char ' ' line
  |> List.filter (fun word -> word <> "")
  |> List.map int_of_string_opt

(* First step: nothing has been seen, so only quotes are asked for. *)
let rec read_decision step =
  let prompt = if step = 0 then "  bid ask > " else "  P(value is 1100) in %, bid, ask > " in
  let parsed =
    match (step, numbers (ask prompt)) with
    | 0, [ Some bid; Some ask ] -> Some (0.5, bid, ask)
    | _, [ Some percent; Some bid; Some ask ] -> Some (float_of_int percent /. 100., bid, ask)
    | _ -> None
  in
  match parsed with
  | Some decision when valid_decision decision -> decision
  | _ ->
      Printf.printf "  Need whole numbers: percent from 1 to 99, and %d < bid < ask < %d.\n" low high;
      read_decision step

let human_player () : decide =
  let position = ref 0 in
  fun ~step ~last ->
    (match last with
    | Some (Order_book.Buy, price) ->
        decr position;
        Printf.printf "  A trader BOUGHT from you at %d. Your position: %+d\n" price !position
    | Some (Order_book.Sell, price) ->
        incr position;
        Printf.printf "  A trader SOLD to you at %d. Your position: %+d\n" price !position
    | None -> ());
    read_decision step

let intro () =
  Printf.printf
    "BIAS EXPERIMENT\n\n\
     The asset is secretly worth %d or %d, equally likely. %d traders arrive, one at a time.\n\
     Some traders know the value and trade only at a profit. The rest flip a coin.\n\
     Each game tells you the share of traders who know the value.\n\n\
     At each step, give your honest probability that the value is %d, then the tightest\n\
     bid and ask you think would not lose money on average.\n\
     Type q at any prompt to stop. Answer from your own judgement, without a calculator:\n\
     the experiment measures your intuition, so notes or formulas would invalidate it.\n"
    low high steps high

let () =
  let path = if Array.length Sys.argv > 1 then Sys.argv.(1) else default_path in
  intro ();
  let seeder = Random.State.make_self_init () in
  let rec next game =
    Printf.printf "\nGAME %d: about %.0f%% of traders know the value.\n" (game + 1)
      (100. *. alpha_for_game game);
    let rows =
      play_game ~game ~seed:(Random.State.bits seeder) (human_player ())
    in
    save path rows;
    (* The Bayesian answers stay hidden, so playing does not teach them. *)
    Printf.printf "  The value was %d. Your profit this game: %+d.\n"
      (List.hd rows).value (game_pnl rows);
    ignore (ask "Press Enter for the next game, or q to stop > ");
    next (game + 1)
  in
  let played = List.fold_left (fun acc r -> max acc (r.game + 1)) 0 (load path) in
  try next played
  with Quit -> Printf.printf "\nStopped. Finished games are saved in %s.\n" path
