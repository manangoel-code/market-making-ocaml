(** The bias experiment: a person plays the market maker and states a belief at each
    step. Every measure compares those beliefs with the exact Bayesian answer. *)

val low : int
val high : int
val steps : int

(** The informed share for a game. It cycles, so each value gets equal play. *)
val alpha_for_game : int -> float

(** One step of one game, as stored in the data file. *)
type row = {
  game : int;
  seed : int;
  step : int;
  alpha : float;
  stated : float;  (** The player's probability that the value is [high]. *)
  bayes : float;  (** The exact Bayesian probability, never shown during play. *)
  bid : int;
  ask : int;
  taker_bought : bool;
  informed : bool;
  price : int;
  value : int;
}

(** What a player sees: only the previous taker's side and price, never the value or
    whether that taker was informed. Returns [(probability of high, bid, ask)]. *)
type decide = step:int -> last:(Order_book.side * int) option -> float * int * int

(** Rules for a player's input: a probability strictly between 0 and 1, and
    [low < bid < ask < high], so an informed taker always has a profitable trade. *)
val valid_decision : float * int * int -> bool

(** Play one game, one row per step. The stated belief at step 0 is recorded as 0.5:
    nothing has been seen yet, so it carries no information. *)
val play_game : game:int -> seed:int -> decide -> row list

val game_pnl : row list -> int
val to_csv : row -> string
val of_csv : string -> row
val csv_header : string

(** Rows from a data file, grouped into games. *)
val group_games : row list -> row list list

(** {1 Measures} All take a list of games. Step 0 is excluded from them. *)

(** Mean squared error of a probability against the outcome. 0 is perfect; always
    saying 50% scores 0.25. [brier] uses stated beliefs, [bayes_brier] the benchmark. *)
val brier : row list list -> float

val bayes_brier : row list list -> float

(** Mean confidence minus the share of steps on the correct side. Above 0 is
    overconfident, below 0 is underconfident, and a calibrated player scores about 0. *)
val overconfidence : row list list -> float

(** How far beliefs move per unit of Bayesian movement, measured in log odds. 1 is
    correct, below 1 is too little (conservatism), above 1 is too much (overreaction).
    Both are clipped to 1%..99%, the range a player can enter. That cap makes strong
    overreaction read low: simulated players with slope 1.5 measure about 1.3. *)
val update_slope : row list list -> float

(** Mean half spread quoted, divided by the mean break-even half spread at the Bayesian
    belief. Below 1 means quoting too tight for the informed flow. *)
val spread_ratio : row list list -> float

(** [(bin low, bin high, count, mean stated, share where value was high)] in 5 bins. *)
val calibration : row list list -> (float * float * int * float * float) list

(** [(pnl, spread capture, adverse selection)] per fill, for the rows selected. *)
val decomposition : (row -> bool) -> row list list -> float * float * float

(** 95% percentile interval, resampling whole games because steps within one game are
    not independent. Fixed seed, so the same data gives the same interval. *)
val bootstrap : (row list list -> float) -> row list list -> float * float

(** {1 Bots with a known bias} These check that the measures recover the truth. *)

(** Bayesian belief with log odds multiplied by [slope], plus Gaussian noise of
    standard deviation [noise], rounded to a whole percent like a person's input. *)
val bot : Random.State.t -> alpha:float -> slope:float -> noise:float -> decide

val simulate : games:int -> slope:float -> noise:float -> seed:int -> row list list
