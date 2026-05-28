# sport 0.5.0

----------------
## Changes:

* New `coef()`, `predict()`, `update()` and `as.list()` methods for objects
  returned by `glicko_run()`, `glicko2_run()`, `bbt_run()` and `dbl_run()`.
  `update()` continues training from existing ratings; `as.list()` produces a
  JSON-friendly representation for persistence.
* New `as.ratings()` generic for constructing a ratings object from previously
  fitted parameters, with methods for `numeric`, `data.frame` and `list`
  inputs (the latter completes the round-trip with `as.list()`).
* New `positional()` function returning per-player, per-position finishing
  probabilities for multi-competitor events via Plackett-Luce enumeration.
* Objects returned by the `*_run()` functions are now of class `"ratings"`
  (previously `"rating"`).
* `plot.ratings()` is deprecated and will be removed in a future release.
  Build your own plots from `coef(model)` and `model$trajectory` instead.
  As a result `ggplot2` has moved from `Imports` to `Suggests` and is no
  longer a hard dependency of the package.
* The `id` column in `model$trajectory` and `model$pairs` is now renamed to
  the LHS id variable from the formula (e.g. `event_id` for
  `place | event_id ~ player(rider)`), matching how the player/team columns
  already adopt their formula names.
* `as.ratings.list()` now forwards the full `settings` list (`weight`,
  `kappa`, `tau`, ...) into the reconstructed object, so a JSON round-trip
  via `as.list()` -> `jsonlite::toJSON()` -> `jsonlite::fromJSON()` ->
  `as.ratings()` -> `update()` resumes training with the original
  hyperparameters and weight column.
* `glicko2_run()` and `bbt_run()` now use `numeric(0)` defaults for
  `lambda`, `share`, and `weight` (matching `glicko_run()` and the internal
  dispatcher) so the `settings` attribute is consistent across methods.

# sport v0.2.2

----------------
## Changes:

* Fixes for `ggplot2` version 4.0.

# sport v0.2.1

----------------
## Changes:

* Fix CRAN issues.

# sport v0.2.0
----------------
### Changes:

* `sigma` argument replaced with `lambda`.
* removed `beta` from glicko `gamma` from bbt.
* all algorithms with players nested in teams.
* additional `share` argument for `player | team` events. 
* increased speed of the algorithms by rewriting event loop to c++.
* updated SGP data

# sport v0.1.2

----------------

### Changes:

* Corrected weight parameter in all functions.
