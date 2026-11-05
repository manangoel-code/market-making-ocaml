open Market_sim

let config alpha = { low = 900; high = 1100; alpha; steps = 20 }

(* A random seed, true alpha and strategy, so the laws hold for every kind of game. *)
let scenario =
  QCheck2.Gen.(
    let* seed = int_range 0 1_000_000 in
    let* alpha = float_range 0. 0.9 in
    let* strategy =
      oneof
        [
          map (fun half_spread -> Fixed { half_spread }) (int_range 1 150);
          map (fun assumed_alpha -> Bayes { assumed_alpha }) (float_range 0. 0.9);
        ]
    in
    return (seed, alpha, strategy))

let print_scenario (seed, alpha, strategy) =
  let name =
    match strategy with
    | Fixed { half_spread } -> Printf.sprintf "Fixed %d" half_spread
    | Bayes { assumed_alpha } -> Printf.sprintf "Bayes %f" assumed_alpha
  in
  Printf.sprintf "seed %d, alpha %f, %s" seed alpha name

let test name law =
  QCheck2.Test.make ~count:500 ~name ~print:print_scenario scenario (fun (seed, alpha, strategy) ->
      law (play (Random.State.make [| seed |]) (config alpha) strategy) (seed, alpha, strategy))

let close a b = abs_float (a -. b) < 1e-6

let decomposition_adds_up =
  test "pnl = spread capture + adverse selection on every fill" (fun game _ ->
      List.for_all (fun f -> close (pnl f) (spread_capture f +. adverse_selection f)) game.fills)

(* Two separate calculations: one from the fills, one from cash and inventory. *)
let accounts_agree =
  test "fill pnl adds up to cash + inventory * value" (fun game _ ->
      let total = List.fold_left (fun acc f -> acc +. pnl f) 0. game.fills in
      close total (float_of_int (game_pnl game)))

let informed_never_lose =
  test "the market maker loses on every informed fill" (fun game _ ->
      List.for_all (fun f -> f.counterparty <> Informed || pnl f < 0.) game.fills)

let seed_reproduces =
  test "the same seed replays the same game" (fun game (seed, alpha, strategy) ->
      game = play (Random.State.make [| seed |]) (config alpha) strategy)

let quotes_never_cross =
  QCheck2.Test.make ~count:1000 ~name:"bid is below ask at any belief and alpha"
    QCheck2.Gen.(pair (float_range 0.001 0.999) (float_range 0. 0.9))
    (fun (p, assumed_alpha) ->
      let bid, ask = quotes (config 0.) (Bayes { assumed_alpha }) p in
      bid < ask)

let exact name law = QCheck2.Test.make ~count:1 ~name QCheck2.Gen.unit (fun () -> law ())

(* Worked by hand: (1 + 0.3) / 2 = 0.65, so 900 + 0.65 * 200 = 1030. *)
let first_quotes =
  exact "first Bayesian quotes at alpha 0.3 are 970 and 1030" (fun () ->
      quotes (config 0.3) (Bayes { assumed_alpha = 0.3 }) 0.5 = (970, 1030))

let opposite_trades_cancel =
  exact "a buy then a sell returns the belief to where it was" (fun () ->
      let assumed_alpha = 0.3 in
      let p = update_belief ~assumed_alpha 0.2 Order_book.Buy in
      close (update_belief ~assumed_alpha p Order_book.Sell) 0.2)

let no_informed_no_learning =
  exact "with alpha 0 a trade reveals nothing" (fun () ->
      close (update_belief ~assumed_alpha:0. 0.7 Order_book.Buy) 0.7)

(* A frozen belief also breaks even, so the break-even test cannot see this bug. *)
let belief_follows_trades =
  exact "the fair value rises after a buy and falls after a sell" (fun () ->
      let rng = Random.State.make [| 3 |] in
      let game = play rng (config 0.3) (Bayes { assumed_alpha = 0.3 }) in
      let rec follows = function
        | a :: (b :: _ as rest) ->
            (* The market maker selling means the taker bought. *)
            (match a.mm_side with
            | Order_book.Sell -> b.fair > a.fair
            | Order_book.Buy -> b.fair < a.fair)
            && follows rest
        | _ -> true
      in
      List.length game.fills > 1 && follows game.fills)

(* Statistical, but the seed is fixed, so it cannot fail at random. *)
let mean_pnl_per_fill config strategy =
  let rng = Random.State.make [| 7 |] in
  let games = List.init 5000 (fun _ -> play rng config strategy) in
  let total = List.fold_left (fun acc g -> acc + game_pnl g) 0 games in
  float_of_int total /. float_of_int (5000 * config.steps)

let fixed_theory =
  exact "fixed quotes earn half spread - alpha * 100 per fill" (fun () ->
      abs_float (mean_pnl_per_fill (config 0.3) (Fixed { half_spread = 50 }) -. 20.) < 1.)

let bayes_breaks_even =
  exact "a correct Bayesian market maker breaks even" (fun () ->
      abs_float (mean_pnl_per_fill (config 0.3) (Bayes { assumed_alpha = 0.3 })) < 1.)

let () =
  QCheck_base_runner.run_tests_main
    [
      decomposition_adds_up;
      accounts_agree;
      informed_never_lose;
      seed_reproduces;
      quotes_never_cross;
      first_quotes;
      opposite_trades_cancel;
      no_informed_no_learning;
      belief_follows_trades;
      fixed_theory;
      bayes_breaks_even;
    ]
