(* A short scripted session: run it to watch orders rest and then match. *)
open Order_book

let print_book book =
  let print label (o : order) = Printf.printf "  %s %3d @ %d (#%d)\n" label o.qty o.price o.id in
  List.iter (print "ask") (List.rev (asks book));
  print_endline "  ---";
  List.iter (print "bid") (bids book)

let step book (order : order) =
  let name = match order.side with Buy -> "BUY" | Sell -> "SELL" in
  Printf.printf "\n#%d %s %d @ %d\n" order.id name order.qty order.price;
  let book, trades = submit book order in
  List.iter
    (fun (t : trade) ->
      Printf.printf "  trade: %d @ %d (buyer #%d, seller #%d)\n" t.qty t.price t.buy_id t.sell_id)
    trades;
  print_book book;
  book

let () =
  let orders =
    [
      { id = 1; side = Buy; price = 99; qty = 10 };
      { id = 2; side = Buy; price = 99; qty = 5 };
      { id = 3; side = Sell; price = 101; qty = 8 };
      (* Crosses the bids: #1 is older than #2, so it fills first. *)
      { id = 4; side = Sell; price = 98; qty = 12 };
      (* Takes all of #3 at 101, and the remaining 4 rest as the new best bid. *)
      { id = 5; side = Buy; price = 102; qty = 12 };
    ]
  in
  ignore (List.fold_left step empty orders)
