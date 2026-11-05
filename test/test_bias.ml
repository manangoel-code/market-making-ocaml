open Bias

let exact name law = QCheck2.Test.make ~count:1 ~name QCheck2.Gen.unit (fun () -> law ())
let close a b tolerance = abs_float (a -. b) < tolerance
let games ~slope = simulate ~games:600 ~slope ~noise:0. ~seed:5

let correct_quotes_at_half : decide = fun ~step:_ ~last:_ -> (0.5, 999, 1001)

let scripted_game = play_game ~game:1 ~seed:42 correct_quotes_at_half

let one_row_per_step =
  exact "every game has one row per step" (fun () -> List.length scripted_game = steps)

let first_belief_is_uninformative =
  exact "step 0 is recorded as 50% whatever the player returns" (fun () ->
      let rows = play_game ~game:1 ~seed:42 (fun ~step:_ ~last:_ -> (0.9, 999, 1001)) in
      (List.hd rows).stated = 0.5)

(* Game 1 has alpha 0.3. By hand: after a buy the belief is 0.65, after a sell 0.35. *)
let benchmark_is_exact =
  exact "the benchmark matches a hand calculation after the first trade" (fun () ->
      match scripted_game with
      | first :: second :: _ ->
          first.bayes = 0.5
          && close second.bayes (if first.taker_bought then 0.65 else 0.35) 1e-9
      | _ -> false)

let csv_round_trip =
  exact "rows survive being saved and loaded" (fun () ->
      List.for_all (fun r -> of_csv (to_csv r) = r) scripted_game)

let invalid_quotes_rejected =
  exact "quotes outside the two possible values are rejected" (fun () ->
      List.for_all
        (fun decision -> not (valid_decision decision))
        [ (0.5, 900, 1000); (0.5, 950, 1100); (0.5, 1000, 1000); (0., 950, 1050); (1., 950, 1050) ])

let hand_scored =
  exact "Brier and overconfidence match a hand calculation" (fun () ->
      let base = List.hd scripted_game in
      let row step stated value = { base with step; stated; bayes = stated; value } in
      (* Step 0 is excluded. 80% and high: (0.2)^2 = 0.04, right. 80% and low: (0.8)^2 = 0.64,
         wrong. Brier is 0.34, confidence 0.8, accuracy 0.5, so overconfidence is 0.3. *)
      let game = [ [ row 0 0.5 high; row 1 0.8 high; row 2 0.8 low ] ] in
      close (brier game) 0.34 1e-9 && close (overconfidence game) 0.3 1e-9)

(* Bots with a known bias: the measures must recover what was built in. *)
let recovers_correct =
  exact "a Bayesian bot has slope 1 and overconfidence 0" (fun () ->
      let g = games ~slope:1. in
      close (update_slope g) 1. 0.03 && close (overconfidence g) 0. 0.02)

let recovers_conservative =
  exact "a conservative bot has slope 0.5 and is underconfident" (fun () ->
      let g = games ~slope:0.5 in
      close (update_slope g) 0.5 0.03 && overconfidence g < -0.03)

let recovers_overreaction =
  exact "an overreacting bot has slope above 1 and is overconfident" (fun () ->
      let g = games ~slope:1.5 in
      update_slope g > 1.2 && overconfidence g > 0.03)

let bayes_is_calibrated =
  exact "exact Bayes has a Brier score no worse than a biased bot" (fun () ->
      let g = games ~slope:1.5 in
      bayes_brier g < brier g && bayes_brier g < 0.25)

let bayes_matches_calibration_table =
  exact "for a Bayesian bot, what it says matches what happens in every bin" (fun () ->
      List.for_all
        (fun (_, _, count, stated, observed) -> count < 100 || close stated observed 0.03)
        (calibration (simulate ~games:6000 ~slope:1. ~noise:0. ~seed:5)))

(* Game 1 has alpha 0.3, so at a 50% belief the break-even half spread is 30. *)
let spread_ratio_by_hand =
  exact "a half spread of 60 where break-even is 30 gives a ratio of 2" (fun () ->
      let row = { (List.hd scripted_game) with bid = 940; ask = 1060; bayes = 0.5 } in
      close (spread_ratio [ [ row ] ]) 2. 1e-9)

let calibration_counts_all =
  exact "calibration bins count every informative row once" (fun () ->
      let g = games ~slope:1. in
      let total = List.fold_left (fun acc (_, _, n, _, _) -> acc + n) 0 (calibration g) in
      total = 600 * (steps - 1))

let spread_ratio_of_break_even =
  exact "a bot quoting break-even at the correct belief has a spread ratio near 1" (fun () ->
      close (spread_ratio (games ~slope:1.)) 1. 0.05)

let () =
  QCheck_base_runner.run_tests_main
    [
      one_row_per_step;
      first_belief_is_uninformative;
      benchmark_is_exact;
      csv_round_trip;
      invalid_quotes_rejected;
      hand_scored;
      recovers_correct;
      recovers_conservative;
      recovers_overreaction;
      bayes_is_calibrated;
      bayes_matches_calibration_table;
      spread_ratio_by_hand;
      calibration_counts_all;
      spread_ratio_of_break_even;
    ]
