open Order_book

type config = { low : int; high : int; alpha : float; steps : int }

type strategy =
  | Fixed of { half_spread : int }
  | Bayes of { assumed_alpha : float }

type counterparty = Informed | Noise

type fill = {
  step : int;
  counterparty : counterparty;
  mm_side : side;
  fill_price : int;
  fair : float;
  true_value : int;
}

type game = { value : int; fills : fill list; cash : int; inventory : int }
type decision = { bid : int; ask : int; fair : float }

let valid_alpha a = a >= 0. && a < 1.

let validate_config config =
  if config.low <= 0 || config.high <= config.low || config.steps < 0 then
    invalid_arg "Market_sim: need 0 < low < high and steps >= 0";
  if not (valid_alpha config.alpha) then invalid_arg "Market_sim: alpha must be in [0, 1)"

let validate_strategy config strategy =
  match strategy with
  | Fixed { half_spread } ->
      if half_spread < 1 || (config.low + config.high) / 2 - half_spread <= 0 then
        invalid_arg "Market_sim: half_spread must be at least 1 and keep the bid positive"
  | Bayes { assumed_alpha } ->
      if not (valid_alpha assumed_alpha) then
        invalid_arg "Market_sim: assumed_alpha must be in [0, 1)"

(* Bayes' rule. If the value is high, a taker buys with probability (1 + a) / 2:
   every informed taker buys (a) and half of the noise takers buy ((1 - a) / 2).
   If the value is low, it buys with probability (1 - a) / 2. Selling mirrors this. *)
let update_belief ~assumed_alpha:a p side =
  let likely = (1. +. a) /. 2. and unlikely = (1. -. a) /. 2. in
  let if_high, if_low =
    match side with Buy -> (likely, unlikely) | Sell -> (unlikely, likely)
  in
  p *. if_high /. ((p *. if_high) +. ((1. -. p) *. if_low))

let expected_value config p =
  float_of_int config.low +. (p *. float_of_int (config.high - config.low))

let quotes config strategy p =
  match strategy with
  | Fixed { half_spread } ->
      let mid = (config.low + config.high) / 2 in
      (mid - half_spread, mid + half_spread)
  | Bayes { assumed_alpha } ->
      (* Quote what the asset is worth if the next taker trades at that quote, so the
         quote already allows for what the trade would reveal. *)
      let quote side =
        int_of_float
          (Float.round (expected_value config (update_belief ~assumed_alpha p side)))
      in
      let bid = quote Sell in
      (* Rounding can make them equal, and the market maker must not trade with itself. *)
      (bid, max (quote Buy) (bid + 1))

(* The taker's side and limit price, or None if an informed taker sees no profit. *)
let draw_taker rng config value ~bid ~ask =
  if Random.State.float rng 1. < config.alpha then
    if ask < value then Some (Informed, Buy, value)
    else if bid > value then Some (Informed, Sell, value)
    else None
  else if Random.State.bool rng then Some (Noise, Buy, ask)
  else Some (Noise, Sell, bid)

let play_with rng config quote =
  validate_config config;
  let value = if Random.State.bool rng then config.high else config.low in
  let rec run step book last cash inventory fills =
    if step = config.steps then { value; fills = List.rev fills; cash; inventory }
    else
      let { bid; ask; fair } = quote ~step ~last in
      if bid >= ask then invalid_arg "Market_sim: bid must be below ask";
      let bid_id = 3 * step and ask_id = (3 * step) + 1 in
      let book, _ = submit book { id = bid_id; side = Buy; price = bid; qty = 1 } in
      let book, _ = submit book { id = ask_id; side = Sell; price = ask; qty = 1 } in
      let requote book = cancel (cancel book bid_id) ask_id in
      match draw_taker rng config value ~bid ~ask with
      | None -> run (step + 1) (requote book) None cash inventory fills
      | Some (counterparty, side, price) -> (
          let order = { id = (3 * step) + 2; side; price; qty = 1 } in
          let book, trades = submit book order in
          match trades with
          | [ trade ] ->
              let mm_bought = trade.buy_id = bid_id in
              let fill =
                {
                  step;
                  counterparty;
                  mm_side = (if mm_bought then Buy else Sell);
                  fill_price = trade.price;
                  fair;
                  true_value = value;
                }
              in
              run (step + 1) (requote book) (Some fill)
                (if mm_bought then cash - trade.price else cash + trade.price)
                (if mm_bought then inventory + 1 else inventory - 1)
                (fill :: fills)
          | _ -> failwith "Market_sim: a taker must cross exactly one quote")
  in
  run 0 empty None 0 0 []

let play rng config strategy =
  validate_config config;
  validate_strategy config strategy;
  let belief = ref 0.5 in
  play_with rng config (fun ~step:_ ~last ->
      (match (strategy, last) with
      | Bayes { assumed_alpha }, Some fill ->
          (* The taker is on the other side of the market maker. *)
          let taker = match fill.mm_side with Buy -> Sell | Sell -> Buy in
          belief := update_belief ~assumed_alpha !belief taker
      | _ -> ());
      let bid, ask = quotes config strategy !belief in
      { bid; ask; fair = expected_value config !belief })

let sign fill = match fill.mm_side with Buy -> 1. | Sell -> -1.
let spread_capture f = sign f *. (f.fair -. float_of_int f.fill_price)
let adverse_selection f = sign f *. (float_of_int f.true_value -. f.fair)
let pnl f = sign f *. float_of_int (f.true_value - f.fill_price)
let game_pnl g = g.cash + (g.inventory * g.value)
