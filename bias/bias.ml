let low = 900
let high = 1100
let steps = 8
let alpha_for_game game = [| 0.1; 0.3; 0.5 |].(game mod 3)

type row = {
  game : int;
  seed : int;
  step : int;
  alpha : float;
  stated : float;
  bayes : float;
  bid : int;
  ask : int;
  taker_bought : bool;
  informed : bool;
  price : int;
  value : int;
}

type decide = step:int -> last:(Order_book.side * int) option -> float * int * int

let config alpha = { Market_sim.low; high; alpha; steps }

let valid_decision (stated, bid, ask) =
  stated > 0. && stated < 1. && low < bid && bid < ask && ask < high

let taker_side (fill : Market_sim.fill) =
  match fill.mm_side with Order_book.Buy -> Order_book.Sell | Order_book.Sell -> Order_book.Buy

let expected_value p = float_of_int low +. (p *. float_of_int (high - low))

let play_game ~game ~seed (decide : decide) =
  let alpha = alpha_for_game game in
  let rng = Random.State.make [| seed |] in
  let bayes = ref 0.5 and decisions = ref [] in
  let quote ~step ~(last : Market_sim.fill option) =
    (match last with
    | Some fill ->
        bayes := Market_sim.update_belief ~assumed_alpha:alpha !bayes (taker_side fill)
    | None -> ());
    let last = Option.map (fun (f : Market_sim.fill) -> (taker_side f, f.fill_price)) last in
    let stated, bid, ask = decide ~step ~last in
    let stated = if step = 0 then 0.5 else stated in
    if not (valid_decision (stated, bid, ask)) then invalid_arg "Bias: invalid decision";
    decisions := (!bayes, stated, bid, ask) :: !decisions;
    { Market_sim.bid; ask; fair = expected_value stated }
  in
  let result = Market_sim.play_with rng (config alpha) quote in
  List.map2
    (fun (fill : Market_sim.fill) (bayes, stated, bid, ask) ->
      {
        game;
        seed;
        step = fill.step;
        alpha;
        stated;
        bayes;
        bid;
        ask;
        taker_bought = taker_side fill = Order_book.Buy;
        informed = fill.counterparty = Market_sim.Informed;
        price = fill.fill_price;
        value = fill.true_value;
      })
    result.fills
    (List.rev !decisions)

(* Rebuilt so that profit is always calculated by the simulator's own formulas. *)
let fill_of_row row =
  {
    Market_sim.step = row.step;
    counterparty = (if row.informed then Market_sim.Informed else Market_sim.Noise);
    mm_side = (if row.taker_bought then Order_book.Sell else Order_book.Buy);
    fill_price = row.price;
    fair = expected_value row.stated;
    true_value = row.value;
  }

let game_pnl rows =
  List.fold_left
    (fun acc row -> acc + int_of_float (Market_sim.pnl (fill_of_row row)))
    0 rows

let csv_header = "game,seed,step,alpha,stated,bayes,bid,ask,taker_bought,informed,price,value"

let to_csv r =
  Printf.sprintf "%d,%d,%d,%.17g,%.17g,%.17g,%d,%d,%b,%b,%d,%d" r.game r.seed r.step r.alpha
    r.stated r.bayes r.bid r.ask r.taker_bought r.informed r.price r.value

let of_csv line =
  match String.split_on_char ',' (String.trim line) with
  | [ game; seed; step; alpha; stated; bayes; bid; ask; taker_bought; informed; price; value ] ->
      {
        game = int_of_string game;
        seed = int_of_string seed;
        step = int_of_string step;
        alpha = float_of_string alpha;
        stated = float_of_string stated;
        bayes = float_of_string bayes;
        bid = int_of_string bid;
        ask = int_of_string ask;
        taker_bought = bool_of_string taker_bought;
        informed = bool_of_string informed;
        price = int_of_string price;
        value = int_of_string value;
      }
  | _ -> invalid_arg ("Bias.of_csv: bad line: " ^ line)

let group_games rows =
  let ids = List.sort_uniq compare (List.map (fun r -> r.game) rows) in
  List.map (fun id -> List.filter (fun r -> r.game = id) rows) ids

let mean = function
  | [] -> nan
  | xs -> List.fold_left ( +. ) 0. xs /. float_of_int (List.length xs)

let informative games = List.filter (fun r -> r.step > 0) (List.concat games)
let outcome r = if r.value = high then 1. else 0.

let brier_of belief games =
  mean (List.map (fun r -> (belief r -. outcome r) ** 2.) (informative games))

let brier = brier_of (fun r -> r.stated)
let bayes_brier = brier_of (fun r -> r.bayes)

let overconfidence games =
  mean
    (List.map
       (fun r ->
         let confidence = max r.stated (1. -. r.stated) in
         (* An exact 50% picks no side, so it counts as half right. *)
         let correct =
           if r.stated = 0.5 then 0.5
           else if r.stated > 0.5 = (r.value = high) then 1.
           else 0.
         in
         confidence -. correct)
       (informative games))

let clip p = Float.min 0.99 (Float.max 0.01 p)
let logit p = log (p /. (1. -. p))
let clipped_logit p = logit (clip p)

(* Least squares through the origin. No intercept, because seeing no new evidence
   should cause no movement. *)
let update_slope games =
  let rec pairs acc = function
    | a :: (b :: _ as rest) ->
        let human = clipped_logit b.stated -. clipped_logit a.stated in
        let bayes = clipped_logit b.bayes -. clipped_logit a.bayes in
        pairs ((human, bayes) :: acc) rest
    | _ -> acc
  in
  let all = List.concat_map (pairs []) games in
  let cross = List.fold_left (fun acc (h, b) -> acc +. (h *. b)) 0. all in
  let square = List.fold_left (fun acc (_, b) -> acc +. (b *. b)) 0. all in
  cross /. square

let half_spread r = float_of_int (r.ask - r.bid) /. 2.

let break_even_half_spread r =
  let bid, ask =
    Market_sim.quotes (config r.alpha) (Market_sim.Bayes { assumed_alpha = r.alpha }) r.bayes
  in
  float_of_int (ask - bid) /. 2.

let spread_ratio games =
  let rows = List.concat games in
  mean (List.map half_spread rows) /. mean (List.map break_even_half_spread rows)

let calibration games =
  let rows = informative games in
  List.map
    (fun i ->
      let lo = float_of_int i /. 5. and hi = float_of_int (i + 1) /. 5. in
      let inside r = r.stated >= lo && r.stated < hi in
      let group = List.filter inside rows in
      ( lo,
        hi,
        List.length group,
        mean (List.map (fun r -> r.stated) group),
        mean (List.map outcome group) ))
    [ 0; 1; 2; 3; 4 ]

let decomposition keep games =
  let fills = List.map fill_of_row (List.filter keep (List.concat games)) in
  ( mean (List.map Market_sim.pnl fills),
    mean (List.map Market_sim.spread_capture fills),
    mean (List.map Market_sim.adverse_selection fills) )

let bootstrap stat games =
  let games = Array.of_list games in
  let n = Array.length games in
  let rng = Random.State.make [| 1 |] in
  let replicates =
    Array.init 2000 (fun _ ->
        stat (List.init n (fun _ -> games.(Random.State.int rng n))))
  in
  Array.sort compare replicates;
  (replicates.(50), replicates.(1949))

(* Box-Muller: turns two uniform random numbers into one standard normal number. *)
let gaussian rng =
  let u = 1. -. Random.State.float rng 1. and v = Random.State.float rng 1. in
  sqrt (-2. *. log u) *. cos (2. *. Float.pi *. v)

let bot rng ~alpha ~slope ~noise : decide =
  let belief = ref 0.5 in
  fun ~step:_ ~last ->
    (match last with
    | Some (side, _) -> belief := Market_sim.update_belief ~assumed_alpha:alpha !belief side
    | None -> ());
    let shifted = (slope *. logit !belief) +. (noise *. gaussian rng) in
    let percent = Float.round (100. /. (1. +. exp (-.shifted))) in
    let stated = Float.min 99. (Float.max 1. percent) /. 100. in
    let bid, ask =
      Market_sim.quotes (config alpha) (Market_sim.Bayes { assumed_alpha = alpha }) stated
    in
    let bid = max (low + 1) (min bid (high - 2)) in
    (stated, bid, max (bid + 1) (min ask (high - 1)))

let simulate ~games ~slope ~noise ~seed =
  let rng = Random.State.make [| seed |] in
  List.init games (fun game ->
      let decide = bot rng ~alpha:(alpha_for_game game) ~slope ~noise in
      play_game ~game ~seed:(Random.State.bits rng) decide)
