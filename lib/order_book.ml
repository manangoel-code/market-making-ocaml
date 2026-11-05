module Levels = Map.Make (Int)

type side = Buy | Sell
type order = { id : int; side : side; price : int; qty : int }
type trade = { buy_id : int; sell_id : int; price : int; qty : int }

(* Each side maps a price to its queue of orders, oldest first.
   A price with no orders is removed, so a queue in the map is never empty. *)
type t = { bid_levels : order list Levels.t; ask_levels : order list Levels.t }

let empty = { bid_levels = Levels.empty; ask_levels = Levels.empty }
let best_bid book = Option.map fst (Levels.max_binding_opt book.bid_levels)
let best_ask book = Option.map fst (Levels.min_binding_opt book.ask_levels)

(* Replace the queue at one price on one side. *)
let set_level book side price queue =
  let update levels =
    if queue = [] then Levels.remove price levels
    else Levels.add price queue levels
  in
  match side with
  | Buy -> { book with bid_levels = update book.bid_levels }
  | Sell -> { book with ask_levels = update book.ask_levels }

(* The best resting price the incoming order could trade against, and its queue. *)
let best_opposite book incoming =
  match incoming.side with
  | Buy -> Levels.min_binding_opt book.ask_levels
  | Sell -> Levels.max_binding_opt book.bid_levels

let crosses incoming resting_price =
  match incoming.side with
  | Buy -> incoming.price >= resting_price
  | Sell -> incoming.price <= resting_price

let make_trade incoming resting price qty =
  match incoming.side with
  | Buy -> { buy_id = incoming.id; sell_id = resting.id; price; qty }
  | Sell -> { buy_id = resting.id; sell_id = incoming.id; price; qty }

(* Trade against the best price, oldest order first, until the incoming order is
   filled or no longer crosses. Trades are collected newest first. *)
let rec fill book incoming trades =
  match best_opposite book incoming with
  | Some (price, resting :: rest) when incoming.qty > 0 && crosses incoming price ->
      let qty = min incoming.qty resting.qty in
      let queue =
        if qty = resting.qty then rest
        else { resting with qty = resting.qty - qty } :: rest
      in
      fill
        (set_level book resting.side price queue)
        { incoming with qty = incoming.qty - qty }
        (make_trade incoming resting price qty :: trades)
  | _ -> (book, incoming, trades)

let level_of book side price =
  let levels =
    match side with Buy -> book.bid_levels | Sell -> book.ask_levels
  in
  Option.value (Levels.find_opt price levels) ~default:[]

let submit book (order : order) =
  if order.price <= 0 || order.qty <= 0 then
    invalid_arg "Order_book.submit: price and quantity must be positive";
  let book, remainder, trades = fill book order [] in
  let book =
    if remainder.qty = 0 then book
    else
      (* shortcut: appending is O(queue length); fine at game scale. *)
      let queue = level_of book remainder.side remainder.price @ [ remainder ] in
      set_level book remainder.side remainder.price queue
  in
  (book, List.rev trades)

let cancel book id =
  let remove levels =
    Levels.filter_map
      (fun _ queue ->
        match List.filter (fun (o : order) -> o.id <> id) queue with
        | [] -> None
        | queue -> Some queue)
      levels
  in
  {
    bid_levels = remove book.bid_levels;
    ask_levels = remove book.ask_levels;
  }

(* Bindings come lowest price first, so bids reverse the levels but not the queues. *)
let bids book =
  List.concat_map snd (List.rev (Levels.bindings book.bid_levels))

let asks book = List.concat_map snd (Levels.bindings book.ask_levels)
