open Order_book

let sum_qty orders = List.fold_left (fun acc (o : order) -> acc + o.qty) 0 orders

(* Random order flow around one price, so that many orders cross. *)
let order_flow =
  QCheck2.Gen.(list_size (int_range 0 200) (triple bool (int_range 95 105) (int_range 1 20)))

let print_flow = QCheck2.Print.(list (triple bool int int))

let to_orders flow =
  List.mapi
    (fun id (is_buy, price, qty) ->
      { id; side = (if is_buy then Buy else Sell); price; qty })
    flow

let replay orders =
  List.fold_left
    (fun (book, trades) order ->
      let book, new_trades = submit book order in
      (book, trades @ new_trades))
    (empty, []) orders

let test name law =
  QCheck2.Test.make ~count:1000 ~name ~print:print_flow order_flow (fun flow ->
      law (to_orders flow))

(* The best bid must stay below the best ask: if they met, they should have traded. *)
let never_crossed =
  test "book is never crossed" (fun orders ->
      let uncrossed book =
        match (best_bid book, best_ask book) with
        | Some bid, Some ask -> bid < ask
        | _ -> true
      in
      let check (book, ok) order =
        let book, _ = submit book order in
        (book, ok && uncrossed book)
      in
      snd (List.fold_left check (empty, true) orders))

(* Every submitted unit either traded or still rests: nothing is created or lost. *)
let quantity_conserved =
  test "quantity is conserved on each side" (fun orders ->
      let book, trades = replay orders in
      let submitted side =
        sum_qty (List.filter (fun (o : order) -> o.side = side) orders)
      in
      let traded = List.fold_left (fun acc (t : trade) -> acc + t.qty) 0 trades in
      submitted Buy = sum_qty (bids book) + traded
      && submitted Sell = sum_qty (asks book) + traded)

(* Nobody pays more, or sells for less, than their limit. *)
let limits_respected =
  test "trades respect both limit prices" (fun orders ->
      let _, trades = replay orders in
      let limit id = (List.find (fun (o : order) -> o.id = id) orders).price in
      List.for_all
        (fun (t : trade) -> t.price <= limit t.buy_id && t.price >= limit t.sell_id)
        trades)

(* The earlier order rested, so it sets the price. *)
let trades_at_resting_price =
  test "trades happen at the older order's price" (fun orders ->
      let _, trades = replay orders in
      let limit id = (List.find (fun (o : order) -> o.id = id) orders).price in
      List.for_all
        (fun (t : trade) -> t.price = limit (min t.buy_id t.sell_id))
        trades)

(* Within a price the queue is in arrival order. Ids are given in arrival order. *)
let queues_in_time_order =
  test "resting orders keep time priority" (fun orders ->
      let book, _ = replay orders in
      let rec sorted = function
        | (a : order) :: (b :: _ as rest) ->
            (a.price <> b.price || a.id < b.id) && sorted rest
        | _ -> true
      in
      sorted (bids book) && sorted (asks book))

let cancel_removes_all =
  test "cancelling every order empties the book" (fun orders ->
      let book, _ = replay orders in
      let book =
        List.fold_left (fun b (o : order) -> cancel b o.id) book orders
      in
      bids book = [] && asks book = [])

let time_priority_example =
  QCheck2.Test.make ~count:1 ~name:"older order at a price fills first" QCheck2.Gen.unit
    (fun () ->
      let book, _ = submit empty { id = 1; side = Buy; price = 99; qty = 10 } in
      let book, _ = submit book { id = 2; side = Buy; price = 99; qty = 5 } in
      let _, trades = submit book { id = 3; side = Sell; price = 99; qty = 12 } in
      trades
      = [
          { buy_id = 1; sell_id = 3; price = 99; qty = 10 };
          { buy_id = 2; sell_id = 3; price = 99; qty = 2 };
        ])

let price_priority_example =
  QCheck2.Test.make ~count:1 ~name:"better price fills first" QCheck2.Gen.unit (fun () ->
      let book, _ = submit empty { id = 1; side = Sell; price = 102; qty = 5 } in
      let book, _ = submit book { id = 2; side = Sell; price = 101; qty = 5 } in
      let _, trades = submit book { id = 3; side = Buy; price = 102; qty = 7 } in
      trades
      = [
          { buy_id = 3; sell_id = 2; price = 101; qty = 5 };
          { buy_id = 3; sell_id = 1; price = 102; qty = 2 };
        ])

let () =
  QCheck_base_runner.run_tests_main
    [
      never_crossed;
      quantity_conserved;
      limits_respected;
      trades_at_resting_price;
      queues_in_time_order;
      cancel_removes_all;
      time_priority_example;
      price_priority_example;
    ]
