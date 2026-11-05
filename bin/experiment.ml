(* Measures adverse selection. Fixed seed, so every run prints the same numbers. *)
open Market_sim

let games = 20_000
let steps = 20
let config alpha = { low = 900; high = 1100; alpha; steps }

(* Half the distance between the two values: the most a wrong fair value can cost. *)
let half_range = 100.
let rng = Random.State.make [| 20261101 |]
let play_many config strategy = Array.init games (fun _ -> play rng config strategy)
let mean xs = Array.fold_left ( +. ) 0. xs /. float_of_int (Array.length xs)

let std xs =
  let m = mean xs in
  let squares = Array.fold_left (fun acc x -> acc +. ((x -. m) ** 2.)) 0. xs in
  sqrt (squares /. float_of_int (Array.length xs - 1))

(* Half-width of a 95% confidence interval. Games are independent, so each game is
   one observation, even though fills within a game are not independent. *)
let ci xs = 1.96 *. std xs /. sqrt (float_of_int (Array.length xs))
let pnls results = Array.map (fun g -> float_of_int (game_pnl g)) results
let all_fills results = Array.of_list (List.concat_map (fun g -> g.fills) (Array.to_list results))
let over fills measure = mean (Array.map measure fills)

let fixed_quotes_match_theory () =
  print_endline "A. Fixed quotes: profit per fill against theory (half spread - alpha * 100)";
  print_endline "alpha  half_spread  theory  measured  95% CI          theory in CI";
  let inside = ref 0 and total = ref 0 in
  List.iter
    (fun alpha ->
      List.iter
        (fun half_spread ->
          let per_fill =
            Array.map
              (fun x -> x /. float_of_int steps)
              (pnls (play_many (config alpha) (Fixed { half_spread })))
          in
          let theory = float_of_int half_spread -. (alpha *. half_range) in
          let m = mean per_fill and w = ci per_fill in
          let hit = abs_float (m -. theory) <= w in
          incr total;
          if hit then incr inside;
          Printf.printf "%5.2f  %11d  %6.1f  %8.2f  [%6.2f, %6.2f]  %s\n" alpha half_spread
            theory m (m -. w) (m +. w)
            (if hit then "yes" else "NO"))
        [ 10; 30; 50 ])
    [ 0.; 0.1; 0.2; 0.3; 0.5 ];
  Printf.printf "Theory inside the interval in %d of %d rows.\n\n" !inside !total

let learning_against_fixed () =
  print_endline "B. Break-even fixed quotes against Bayesian learning, per game";
  print_endline "alpha  strategy  mean_pnl  95% CI    std_pnl  half_spread  abs_inventory";
  List.iter
    (fun alpha ->
      let row name strategy =
        let results = play_many (config alpha) strategy in
        let p = pnls results in
        Printf.printf "%5.2f  %-8s  %8.2f  +/-%5.2f  %7.1f  %11.2f  %13.2f\n" alpha name
          (mean p) (ci p) (std p)
          (over (all_fills results) spread_capture)
          (mean (Array.map (fun g -> float_of_int (abs g.inventory)) results))
      in
      row "fixed" (Fixed { half_spread = int_of_float (alpha *. half_range) });
      row "bayes" (Bayes { assumed_alpha = alpha }))
    [ 0.1; 0.3; 0.5 ];
  print_newline ()

let who_causes_the_loss () =
  print_endline "C. Who causes the loss, at alpha = 0.30, per fill";
  print_endline "strategy  taker     share  spread_capture  adverse_selection  pnl";
  let alpha = 0.3 in
  List.iter
    (fun (name, strategy) ->
      let fills = all_fills (play_many (config alpha) strategy) in
      List.iter
        (fun (label, who) ->
          let group =
            Array.of_list (List.filter (fun f -> f.counterparty = who) (Array.to_list fills))
          in
          Printf.printf "%-8s  %-8s  %5.3f  %14.2f  %17.2f  %6.2f\n" name label
            (float_of_int (Array.length group) /. float_of_int (Array.length fills))
            (over group spread_capture) (over group adverse_selection) (over group pnl))
        [ ("informed", Informed); ("noise", Noise) ])
    [ ("fixed", Fixed { half_spread = 30 }); ("bayes", Bayes { assumed_alpha = alpha }) ];
  print_newline ()

let wrong_belief () =
  print_endline "D. Bayesian market maker with a wrong belief about informed traders";
  print_endline "true alpha = 0.30";
  print_endline "assumed_alpha  mean_pnl  95% CI    half_spread";
  List.iter
    (fun assumed_alpha ->
      let results = play_many (config 0.3) (Bayes { assumed_alpha }) in
      let p = pnls results in
      Printf.printf "%13.2f  %8.2f  +/-%5.2f  %11.2f\n" assumed_alpha (mean p) (ci p)
        (over (all_fills results) spread_capture))
    [ 0.; 0.05; 0.1; 0.2; 0.3; 0.4; 0.5; 0.7 ]

(* Theory says a correct Bayesian market maker earns exactly 0, but only with continuous
   prices. Same market with finer ticks: results are in units of the original tick. *)
let rounding_check () =
  print_endline "\nE. Correct Bayesian market maker at alpha = 0.50, with finer price ticks";
  print_endline "ticks_per_unit  mean_pnl  95% CI    informed_share";
  List.iter
    (fun scale ->
      let config = { low = 900 * scale; high = 1100 * scale; alpha = 0.5; steps } in
      let results = play_many config (Bayes { assumed_alpha = 0.5 }) in
      let p = Array.map (fun x -> x /. float_of_int scale) (pnls results) in
      let fills = all_fills results in
      let informed =
        List.length (List.filter (fun f -> f.counterparty = Informed) (Array.to_list fills))
      in
      Printf.printf "%14d  %8.2f  +/-%5.2f  %14.3f\n" scale (mean p) (ci p)
        (float_of_int informed /. float_of_int (Array.length fills)))
    [ 1; 10; 100; 1000 ]

let () =
  Printf.printf "%d games of %d takers each. Values 900 or 1100.\n\n" games steps;
  fixed_quotes_match_theory ();
  learning_against_fixed ();
  who_causes_the_loss ();
  wrong_belief ();
  rounding_check ()
