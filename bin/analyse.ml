(* Reports your bias from your saved games. With --bots, reports on simulated players
   instead, to show how precisely 30 games can measure a known bias. *)
open Bias

let minimum_games = 30

let load path =
  let channel = open_in path in
  ignore (input_line channel);
  let rec read acc =
    match input_line channel with
    | line -> read (of_csv line :: acc)
    | exception End_of_file ->
        close_in channel;
        List.rev acc
  in
  group_games (read [])

let measure name stat games =
  let lo, hi = bootstrap stat games in
  Printf.printf "%-38s %7.3f  [%6.3f, %6.3f]\n" name (stat games) lo hi

let report games =
  Printf.printf "%d games, %d stated beliefs. Brackets are 95%% bootstrap intervals.\n"
    (List.length games)
    (List.length games * (steps - 1));
  if List.length games < minimum_games then
    Printf.printf "WARNING: fewer than %d games, so treat these as provisional.\n" minimum_games;
  print_endline "\nBELIEFS";
  measure "Brier score, you (lower is better)" brier games;
  measure "Brier score, exact Bayes, same games" bayes_brier games;
  measure "Overconfidence (0 = calibrated)" overconfidence games;
  measure "Update slope (1 = correct)" update_slope games;
  print_endline "  Above 1 is real overreaction, but its size is understated: inputs stop at 1% and 99%.";
  print_endline "\nCALIBRATION\nyou said        count  mean said  actually high";
  List.iter
    (fun (lo, hi, count, stated, observed) ->
      if count > 0 then
        Printf.printf "%3.0f%% to %3.0f%%   %5d  %8.1f%%  %12.1f%%\n" (100. *. lo) (100. *. hi)
          count (100. *. stated) (100. *. observed))
    (calibration games);
  print_endline "\nQUOTES";
  measure "Your spread / break-even spread" spread_ratio games;
  print_endline "\nPROFIT PER FILL   pnl  spread_capture  adverse_selection";
  List.iter
    (fun (label, keep) ->
      let pnl, spread, adverse = decomposition keep games in
      Printf.printf "%-14s %6.2f  %14.2f  %17.2f\n" label pnl spread adverse)
    [
      ("all traders", fun _ -> true);
      ("informed", fun r -> r.informed);
      ("noise", fun r -> not r.informed);
    ]

let bots () =
  print_endline "SIMULATED PLAYERS, NOT REAL DATA. 30 games each, logit noise 0.5.";
  print_endline "true slope  measured  95% interval      overconfidence  95% interval";
  List.iter
    (fun slope ->
      let games = simulate ~games:minimum_games ~slope ~noise:0.5 ~seed:11 in
      let lo, hi = bootstrap update_slope games in
      let o_lo, o_hi = bootstrap overconfidence games in
      Printf.printf "%10.1f  %8.3f  [%5.3f, %5.3f]  %14.3f  [%6.3f, %6.3f]\n" slope
        (update_slope games) lo hi (overconfidence games) o_lo o_hi)
    [ 0.5; 0.7; 1.0; 1.3; 1.5 ];
  (* One dataset can be unlucky, so check the method itself: over many datasets, a 95%
     interval should contain the true slope about 95% of the time. *)
  let datasets = 100 in
  Printf.printf "\nOver %d simulated datasets of %d games:\n" datasets minimum_games;
  print_endline "true slope  mean measured  interval contains truth  mean interval width";
  List.iter
    (fun slope ->
      let results =
        List.init datasets (fun seed ->
            let games = simulate ~games:minimum_games ~slope ~noise:0.5 ~seed in
            let lo, hi = bootstrap update_slope games in
            (update_slope games, lo <= slope && slope <= hi, hi -. lo))
      in
      let average f =
        List.fold_left (fun acc r -> acc +. f r) 0. results /. float_of_int datasets
      in
      Printf.printf "%10.1f  %13.3f  %22.0f%%  %19.3f\n" slope
        (average (fun (m, _, _) -> m))
        (average (fun (_, hit, _) -> if hit then 100. else 0.))
        (average (fun (_, _, w) -> w)))
    [ 0.5; 1.0; 1.5 ]

let () =
  match Array.to_list Sys.argv with
  | [ _; "--bots" ] -> bots ()
  | args ->
      let path = match args with [ _; path ] -> path | _ -> "results/my_games.csv" in
      if Sys.file_exists path then report (load path)
      else Printf.printf "No games found at %s. Play first with bin/play.exe.\n" path
