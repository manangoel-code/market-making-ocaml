(** One market maker quotes against bots, some of which know the hidden value.

    Each game: the asset is secretly worth [low] or [high], with equal chance.
    Each step the market maker quotes a bid and an ask for 1 unit, then one taker
    arrives. With probability [alpha] the taker is informed: it knows the value and
    trades only at a profit. Otherwise it is noise: it buys or sells on a coin flip.
    This is the Glosten-Milgrom (1985) model, which has exact answers to test against. *)

type config = {
  low : int;
  high : int;
  alpha : float;  (** True share of informed takers, in [0, 1). *)
  steps : int;  (** Takers per game. *)
}

type strategy =
  | Fixed of { half_spread : int }  (** Never learns: fixed quotes around the midpoint. *)
  | Bayes of { assumed_alpha : float }
      (** Updates its belief from each trade, believing the informed share is
          [assumed_alpha], and quotes the expected value given the next trade. *)

type counterparty = Informed | Noise

type fill = {
  step : int;
  counterparty : counterparty;
  mm_side : Order_book.side;  (** [Buy] if the market maker bought. *)
  fill_price : int;
  fair : float;  (** The market maker's expected value just before this trade. *)
  true_value : int;
}

type game = { value : int; fills : fill list; cash : int; inventory : int }

(** What a market maker does at one step. [fair] is its own expected value. *)
type decision = { bid : int; ask : int; fair : float }

(** Belief that the value is [high], after seeing a taker on the given side. *)
val update_belief : assumed_alpha:float -> float -> Order_book.side -> float

(** [(bid, ask)] for a belief that the value is [high]. *)
val quotes : config -> strategy -> float -> int * int

(** Play one game with quotes from any source, such as a person. [quote] is called once
    per step with the previous step's fill, if there was one.
    @raise Invalid_argument on an invalid config or if a bid is not below its ask. *)
val play_with :
  Random.State.t -> config -> (step:int -> last:fill option -> decision) -> game

(** Play one game. All randomness comes from the given state, so a seed reproduces it.
    @raise Invalid_argument on an invalid config or strategy. *)
val play : Random.State.t -> config -> strategy -> game

(** The per-fill decomposition: [pnl f = spread_capture f + adverse_selection f]. *)

(** What the fill earned against the market maker's own fair value. Set by the quote. *)
val spread_capture : fill -> float

(** How much the fair value was wrong against the market maker. Negative on average
    when takers are informed. *)
val adverse_selection : fill -> float

(** Profit on this unit, marked at the true value. *)
val pnl : fill -> float

(** Final cash plus inventory at the true value. *)
val game_pnl : game -> int
