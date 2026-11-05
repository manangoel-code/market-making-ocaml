(* Compiled to JavaScript, so the browser game runs the same OCaml as the terminal game.

   A game asks for each decision through a callback, but a web page cannot pause a
   function while it waits for a click. A game is fully determined by its seed and
   decisions, so each call replays it with the decisions so far and stops at the first
   one missing. A game has only 8 steps, so replaying costs nothing. *)
open Js_of_ocaml
open Bias

exception Need_input of (Order_book.side * int) option

(* Decisions arrive as "percent bid ask" separated by ";". *)
let parse_decisions text =
  String.split_on_char ';' text
  |> List.filter (fun part -> part <> "")
  |> List.map (fun part ->
         Scanf.sscanf part "%d %d %d" (fun percent bid ask ->
             (float_of_int percent /. 100., bid, ask)))
  |> Array.of_list

let json_number x = if Float.is_nan x then "null" else Printf.sprintf "%.17g" x

let json_trade taker_bought price =
  Printf.sprintf {|{"side":"%s","price":%d}|} (if taker_bought then "buy" else "sell") price

let play_step game seed decisions_text =
  let decisions = parse_decisions decisions_text in
  let decide ~step ~last =
    if step < Array.length decisions then decisions.(step) else raise (Need_input last)
  in
  match play_game ~game ~seed decide with
  | exception Need_input last ->
      let trade =
        match last with
        | None -> "null"
        | Some (side, price) -> json_trade (side = Order_book.Buy) price
      in
      Printf.sprintf {|{"done":false,"step":%d,"last":%s}|} (Array.length decisions) trade
  | rows ->
      let final = List.nth rows (List.length rows - 1) in
      Printf.sprintf {|{"done":true,"step":%d,"last":%s,"value":%d,"pnl":%d,"csv":"%s"}|} steps
        (json_trade final.taker_bought final.price)
        final.value (game_pnl rows)
        (String.concat "\\n" (List.map to_csv rows))

let measure stat games =
  let lo, hi = bootstrap stat games in
  Printf.sprintf {|{"value":%s,"low":%s,"high":%s}|}
    (json_number (stat games)) (json_number lo) (json_number hi)

let report csv =
  let rows =
    String.split_on_char '\n' csv
    |> List.filter (fun line -> String.trim line <> "")
    |> List.map of_csv
  in
  let games = group_games rows in
  let bins =
    calibration games
    |> List.map (fun (lo, hi, count, stated, observed) ->
           Printf.sprintf {|{"low":%s,"high":%s,"count":%d,"stated":%s,"observed":%s}|}
             (json_number lo) (json_number hi) count (json_number stated) (json_number observed))
  in
  let parts keep =
    let pnl, spread, adverse = decomposition keep games in
    Printf.sprintf {|{"pnl":%s,"spread":%s,"adverse":%s}|} (json_number pnl) (json_number spread)
      (json_number adverse)
  in
  Printf.sprintf
    {|{"games":%d,"beliefs":%d,"brier":%s,"bayesBrier":%s,"overconfidence":%s,"slope":%s,"spreadRatio":%s,"calibration":[%s],"all":%s,"informed":%s,"noise":%s}|}
    (List.length games)
    (List.length games * (steps - 1))
    (measure brier games) (measure bayes_brier games) (measure overconfidence games)
    (measure update_slope games) (measure spread_ratio games) (String.concat "," bins)
    (parts (fun _ -> true))
    (parts (fun r -> r.informed))
    (parts (fun r -> not r.informed))

let () =
  Js.Unsafe.set Js.Unsafe.global "HiddenValueEngine"
    (Js.Unsafe.obj
       [|
         ( "alpha",
           Js.Unsafe.inject (Js.wrap_callback (fun game -> alpha_for_game game)) );
         ( "steps", Js.Unsafe.inject steps );
         ( "low", Js.Unsafe.inject low );
         ( "high", Js.Unsafe.inject high );
         ( "header", Js.Unsafe.inject (Js.string csv_header) );
         ( "valid",
           Js.Unsafe.inject
             (Js.wrap_callback (fun percent bid ask ->
                  Js.bool (valid_decision (float_of_int percent /. 100., bid, ask)))) );
         ( "step",
           Js.Unsafe.inject
             (Js.wrap_callback (fun game seed decisions ->
                  Js.string (play_step game seed (Js.to_string decisions)))) );
         ( "report",
           Js.Unsafe.inject
             (Js.wrap_callback (fun csv -> Js.string (report (Js.to_string csv)))) );
       |])
