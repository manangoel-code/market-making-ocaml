# Market-making under hidden information

A simulated exchange in OCaml. A market maker quotes against bots, and some of the bots know the asset's
hidden value. The project measures what those informed traders cost the market maker and checks the
results against exact theory. A second part measures how a person's beliefs and quotes differ from the
Bayesian answer.

## What is in it

| Part | Folder | What it does |
|---|---|---|
| Order book | `lib/` | Matches orders by best price, then oldest first. The book is immutable. |
| Market simulation | `sim/` | The Glosten-Milgrom (1985) model: informed and coin-flip traders. |
| Adverse-selection measurement | `sim/` | Splits every trade's profit into spread capture plus adverse selection. |
| Bias experiment | `bias/`, `bin/play.ml`, `bin/analyse.ml` | A person plays the market maker; the report compares them with exact Bayes. |
| Tests | `test/` | 33 tests, including randomised property tests and theory checks. |

## Simulation results

20,000 games of 20 trades each, with a value of 900 or 1100. Alpha is the share of informed traders.
Full output: [`results/experiment.txt`](results/experiment.txt). The seed is fixed, so a rerun prints the same numbers.

With fixed quotes, profit per trade should equal `half spread - alpha * 100`. The theory value fell
inside the 95% interval in 14 of 15 settings, about what chance predicts.

The loss comes from the informed traders. At alpha 0.3 with a half spread of 30:

| Trader | Share of trades | Spread capture | Adverse selection | Profit per trade |
|---|---|---|---|---|
| Informed | 0.299 | 30.00 | -100.00 | -70.00 |
| Coin-flip | 0.701 | 30.00 | -0.18 | 29.82 |

A market maker that learns quotes a narrower spread and has a smaller standard deviation, but both
strategies break even on average:

| Alpha | Strategy | Mean profit per game (95% CI) | Standard deviation | Mean half spread |
|---|---|---|---|---|
| 0.3 | Fixed quotes | -0.86 ± 5.84 | 421.5 | 30.00 |
| 0.3 | Bayesian | -2.74 ± 2.79 | 201.2 | 16.07 |
| 0.5 | Fixed quotes | -0.10 ± 5.36 | 387.1 | 50.00 |
| 0.5 | Bayesian | 4.56 ± 1.35 | 97.1 | 16.52 |

The +4.56 comes from rounding to whole-number prices. With finer prices it falls within the interval
around 0 (section E of the results).

Assuming too few informed traders costs far more. With a true alpha of 0.3, a Bayesian market maker
that assumes 0.1 loses 275.55 per game, and one that assumes 0 loses 591.61.

## Human results

The experiment has not been played yet, so there is no data from a person.
[`results/simulated_method_check.txt`](results/simulated_method_check.txt) holds simulated players only,
used to check the measures.

## Limits

- There is no rival market maker, and coin-flip traders accept any price. A wider spread therefore always
  pays here, and any profit from overestimating informed traders depends on that assumption.
- Each trade is 1 unit. Inventory limits, latency and fees are not modelled.
- The bias experiment has one participant, who knows the design and isn't paid for accuracy. It can
  describe that person's choices but says nothing about other people.
- Measured on simulated players, the 95% slope interval contains the truth 88-92% of the time, and
  strong overreaction reads low because inputs stop at 1% and 99%.

## Run it

[Play the game in your browser](https://manangoel-code.github.io/market-making-ocaml/web/hidden_value.html).
Your games save in that browser only. `web/engine.ml` compiles the same OCaml to JavaScript, and `web/build.py`
puts it into one page, `web/hidden_value.html`.

To run everything else, you need [opam](https://opam.ocaml.org/) with `dune`, `qcheck-core` and `js_of_ocaml`.

```bash
opam exec -- dune test --force
```

```bash
opam exec -- dune exec bin/main.exe
```

```bash
opam exec -- dune exec bin/experiment.exe
```

```bash
opam exec -- dune exec bin/play.exe
```

```bash
opam exec -- dune exec bin/analyse.exe
```

## References

- Glosten, L. R., & Milgrom, P. R. (1985). Bid, ask and transaction prices in a specialist market with
  heterogeneously informed traders. *Journal of Financial Economics, 14*(1), 71-100.
- Brier, G. W. (1950). Verification of forecasts expressed in terms of probability. *Monthly Weather Review, 78*(1), 1-3.
- Erev, I., Wallsten, T. S., & Budescu, D. V. (1994). Simultaneous over- and underconfidence: The role of
  error in judgment processes. *Psychological Review, 101*(3), 519-527.
- Grether, D. M. (1980). Bayes rule as a descriptive model: The representativeness heuristic.
  *The Quarterly Journal of Economics, 95*(3), 537-557.
