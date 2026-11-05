(** A limit order book with price-time priority.

    The book is immutable: [submit] and [cancel] return a new book and leave the
    old one unchanged, which makes every state easy to test. *)

type side = Buy | Sell

type order = {
  id : int;  (** Unique id, chosen by the caller. *)
  side : side;
  price : int;  (** Limit price in ticks, so money never needs floats. *)
  qty : int;  (** Remaining quantity. *)
}

type trade = {
  buy_id : int;
  sell_id : int;
  price : int;  (** Always the resting order's price. *)
  qty : int;
}

type t

val empty : t

(** Match the order against the book. Any quantity left over rests in the book.
    Returns the new book and the trades in the order they happened.
    @raise Invalid_argument if the price or quantity is not positive. *)
val submit : t -> order -> t * trade list

(** Remove a resting order. Unknown ids leave the book unchanged. *)
val cancel : t -> int -> t

val best_bid : t -> int option
val best_ask : t -> int option

(** Resting buy orders, best price first, oldest first within a price. *)
val bids : t -> order list

(** Resting sell orders, best price first, oldest first within a price. *)
val asks : t -> order list
