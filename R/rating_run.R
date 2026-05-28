#' Shared parameter documentation for the four `*_run()` functions.
#' Not exported; only its `@param` blocks are pulled in via `@inheritParams`.
#'
#' @param formula formula which specifies the model. RHS Allows only player
#' rating parameter and it should be specified in following manner:
#'
#' `rank | id ~ player(name)`.
#'
#' - `rank` player position in event.
#' - `id` event identifier in which pairwise comparison is assessed.
#' - `player(name)` name of the contestant. In this case `player(name)`
#'   helps algorithm point name of the column where player names are stored.
#'
#' Users can also specify formula in in different way:
#'
#' `rank | id ~ player(name|team)`. Which means that players are playing in
#' teams, and results are observed for teams not for players. For more see
#' vignette.
#'
#' @param method one of `c("glicko", "glicko2", "bbt", "dbl")`
#'
#' @param data data.frame which contains columns specified in formula, and
#'  optional columns defined by `lambda`, `weight`.
#'
#' @param r named vector of initial players ratings estimates. If not specified
#' then `r` will be created automatically for parameters specified in `formula`
#' with initial value `init_r`.
#'
#' @param rd rd named vector of initial rating deviation estimates. If not specified
#' then `rd` will be created automatically for parameters specified in `formula`
#' with initial value `init_rd`.
#'
#' @param sigma (only for glicko2) named vector of initial players ratings
#' estimates. If not specified then `sigma` will be created automatically for
#' parameters specified in `formula` with initial value `init_sigma`.
#'
#' @param lambda name of the column in `data` containing lambda values or one
#' constant value (eg. `lambda = colname` or `lambda = 0.5`).
#' Lambda impact prior variance, and uncertainty of the matchup result. The
#' higher lambda, the higher prior variance and more uncertain result of the
#' matchup. Higher lambda flattens chances of winning.
#'
#' @param share name of the column in `data` containing player share in team
#' efforts. It's used to first calculate combined rating of the team and
#' then redistribute ratings update back to players level. Warning - it should
#' be used only if formula is specified with players nested within teams (`player(player|team)`).
#'
#'
#' @param weight name of the column in `data` containing weights values or
#' one constant (eg. `weight = colname` or `weight = 0.5`).
#' Weights increasing (weight > 1) or decreasing (weight < 1) update change.
#' Higher weight increasing impact of event result on rating estimate.
#'
#' @param kappa controls `rd` shrinkage not to be greater than `rd*(1 - kappa)`.
#'  `kappa=1` means that  `rd` will not be decreased.
#' @param tau The system constant. Which constrains the change in volatility over
#'  time. Reasonable choices are between 0.3 and 1.2 (`default = 0.5`), though
#'  the system should be tested to decide which value results in greatest
#'  predictive accuracy. Smaller values of `tau` prevent the volatility measures
#'  from changing by large amounts, which in turn prevent enormous changes in
#'  ratings based on very improbable results. If the application of Glicko-2 is
#'  expected to involve extremely improbable collections of game outcomes, then
#'  `tau` should be set to a small value, even as small as, say, `tau= 0`.
#'
#' @param init_r initial values for `r` if not provided.
#' Defaults are method-specific, defined in `.defaults`.
#'
#' @param init_rd initial values for `rd` if not provided.
#' Defaults are method-specific, defined in `.defaults`.
#'
#' @param init_sigma initial values for `sigma` if not provided.
#' Defaults are method-specific, defined in `.defaults`.
#'
#' @param trajectory whether to record per-event ratings (`trajectory`) and
#' pairwise comparisons (`pairs`) in the returned object. Defaults to `TRUE`.
#' Set to `FALSE` to skip the per-event recording in C++ -- the result then
#' contains only the final `r`/`rd`/`sigma` vectors and `coefficients`, which
#' is faster and produces a smaller object suitable for prediction-only
#' workflows. Methods like [summary()], [print()], and `plot(players=)` that
#' rely on `pairs`/`trajectory` will error on such objects.
#'
#' @aliases .rating_run
#' @name dot-rating_run
#' @keywords internal
NULL



#' Glicko rating algorithm
#'
#' Glicko rating algorithm
#'
#' @inheritParams dot-rating_run
#'
#' @return
#' A `"ratings"` object (a plain list) with components:
#'
#' - `r` -- named numeric vector of player ratings.
#' - `rd` -- named numeric vector of rating deviations.
#' - `coefficients` -- matrix with columns `r` and `rd` (extractable via [coef()]).
#' - `trajectory` -- `data.table` tracking the evolution of ratings across
#'   events (only when `trajectory = TRUE`).
#' - `pairs` -- `data.table` of pairwise comparisons with predicted
#'   probability `P` and observed outcome `Y` (only when `trajectory = TRUE`).
#'
#' Attributes `method`, `formula`, and `settings` carry the model metadata.
#'
#' @examples
#' library(stats)
#'
#' # the simplest example
#' data <- data.frame(
#'   id = c(1, 1, 1, 1),
#'   team = c("A", "A", "B", "B"),
#'   player = c("a", "b", "c", "d"),
#'   rank_team = c(1, 1, 2, 2),
#'   rank_player = c(3, 4, 1, 2)
#' )
#'
#' # Example from Glickman
#' glicko <- glicko_run(
#'   data = data,
#'   formula = rank_player | id ~ player(player),
#'   r = setNames(c(1500.0, 1400.0, 1550.0, 1700.0), c("a", "b", "c", "d")),
#'   rd = setNames(c(200.0, 30.0, 100.0, 300.0), c("a", "b", "c", "d"))
#' )
#'
#' # nested matchup
#' glicko <- glicko_run(
#'   data = data,
#'   formula = rank_team | id ~ player(player | team)
#' )
#' @export
glicko_run <- function(data, formula,
                       r = numeric(0),
                       rd = numeric(0),
                       init_r = .defaults$glicko$init_r,
                       init_rd = .defaults$glicko$init_rd,
                       lambda = numeric(0),
                       share = numeric(0),
                       weight = numeric(0),
                       kappa = 0.5,
                       trajectory = TRUE) {
  args <- as.list(environment())
  g <- do.call(.run_player_ratings, c(list(method = "glicko"), args))
  settings <- args[c("init_r", "init_rd", "lambda", "share", "weight", "kappa")]
  .as_ratings_from_run(g, method = "glicko", formula = formula, settings = settings)
}

#' Glicko2 rating algorithm
#'
#' Glicko2 rating algorithm
#' @inheritParams dot-rating_run
#'
#' @return
#' A `"ratings"` object (a plain list) with components:
#'
#' - `r` -- named numeric vector of player ratings.
#' - `rd` -- named numeric vector of rating deviations.
#' - `sigma` -- named numeric vector of player volatilities.
#' - `coefficients` -- matrix with columns `r`, `rd`, and `sigma` (extractable
#'   via [coef()]).
#' - `trajectory` -- `data.table` tracking the evolution of ratings across
#'   events (only when `trajectory = TRUE`).
#' - `pairs` -- `data.table` of pairwise comparisons with predicted
#'   probability `P` and observed outcome `Y` (only when `trajectory = TRUE`).
#'
#' Attributes `method`, `formula`, and `settings` carry the model metadata.
#'
#' @examples
#' library(stats)
#'
#' # the simplest example
#' data <- data.frame(
#'   id = c(1, 1, 1, 1),
#'   team = c("A", "A", "B", "B"),
#'   player = c("a", "b", "c", "d"),
#'   rank_team = c(1, 1, 2, 2),
#'   rank_player = c(3, 4, 1, 2)
#' )
#'
#' # Example from Glickman
#' glicko2 <- glicko2_run(
#'   data = data,
#'   formula = rank_player | id ~ player(player),
#'   r = setNames(c(1500.0, 1400.0, 1550.0, 1700.0), c("a", "b", "c", "d")),
#'   rd = setNames(c(200.0, 30.0, 100.0, 300.0), c("a", "b", "c", "d"))
#' )
#'
#' # nested matchup
#' glicko2 <- glicko2_run(
#'   data = data,
#'   formula = rank_team | id ~ player(player | team)
#' )
#' @export
glicko2_run <- function(formula,
                        data,
                        r = numeric(0),
                        rd = numeric(0),
                        sigma = numeric(0),
                        lambda = numeric(0),
                        share = numeric(0),
                        weight = numeric(0),
                        init_r = .defaults$glicko2$init_r,
                        init_rd = .defaults$glicko2$init_rd,
                        init_sigma = .defaults$glicko2$init_sigma,
                        kappa = 0.5,
                        tau = 0.5,
                        trajectory = TRUE) {
  args <- as.list(environment())
  g <- do.call(.run_player_ratings, c(list(method = "glicko2"), args))
  settings <- args[c(
    "init_r", "init_rd", "init_sigma",
    "lambda", "share", "weight", "kappa", "tau"
  )]
  .as_ratings_from_run(g, method = "glicko2", formula = formula, settings = settings)
}

#' Bayesian Bradley-Terry
#'
#' Bayesian Bradley-Terry
#' @inheritParams dot-rating_run
#'
#' @return
#' A `"ratings"` object (a plain list) with components:
#'
#' - `r` -- named numeric vector of player ratings.
#' - `rd` -- named numeric vector of rating deviations.
#' - `coefficients` -- matrix with columns `r` and `rd` (extractable via [coef()]).
#' - `trajectory` -- `data.table` tracking the evolution of ratings across
#'   events (only when `trajectory = TRUE`).
#' - `pairs` -- `data.table` of pairwise comparisons with predicted
#'   probability `P` and observed outcome `Y` (only when `trajectory = TRUE`).
#'
#' Attributes `method`, `formula`, and `settings` carry the model metadata.
#'
#' @examples
#' library(stats)
#'
#' # the simplest example
#' data <- data.frame(
#'   id = c(1, 1, 1, 1),
#'   team = c("A", "A", "B", "B"),
#'   player = c("a", "b", "c", "d"),
#'   rank_team = c(1, 1, 2, 2),
#'   rank_player = c(3, 4, 1, 2)
#' )
#'
#' bbt <- bbt_run(
#'   data = data,
#'   formula = rank_player | id ~ player(player),
#'   r = setNames(c(25, 23.3, 25.83, 28.33), c("a", "b", "c", "d")),
#'   rd = setNames(c(4.76, 0.71, 2.38, 7.14), c("a", "b", "c", "d"))
#' )
#'
#' # nested matchup
#' bbt <- bbt_run(
#'   data = data,
#'   formula = rank_team | id ~ player(player | team)
#' )
#'
#' @export
bbt_run <- function(formula,
                    data,
                    r = numeric(0),
                    rd = numeric(0),
                    init_r = .defaults$bbt$init_r,
                    init_rd = .defaults$bbt$init_rd,
                    lambda = numeric(0),
                    share = numeric(0),
                    weight = numeric(0),
                    kappa = 0.5,
                    trajectory = TRUE) {
  args <- as.list(environment())
  g <- do.call(.run_player_ratings, c(list(method = "bbt"), args))
  settings <- args[c("init_r", "init_rd", "lambda", "share", "weight", "kappa")]
  .as_ratings_from_run(g, method = "bbt", formula = formula, settings = settings)
}

#' Dynamic Bayesian Logit
#'
#' Dynamic Bayesian Logit
#'
#' @inheritParams dot-rating_run
#' @param formula formula which specifies the model. Unlike other algorithms
#' in the packages (glicko_run, glicko2_run, bbt_run), this method doesn't allow
#' players nested in teams with `player(player | team)` and user should matchup
#' in formula using `player(player)`. DBL allows user specify multiple parameters
#' also in interaction with others.
#'
#' @return
#' A `"ratings"` object (a plain list) with components:
#'
#' - `r` -- named numeric vector of coefficient estimates.
#' - `rd` -- named numeric vector of coefficient standard deviations.
#' - `coefficients` -- matrix with columns `r` and `rd` (extractable via [coef()]).
#' - `trajectory` -- `data.table` tracking the evolution of ratings across
#'   events (only when `trajectory = TRUE`).
#' - `pairs` -- `data.table` of pairwise comparisons with predicted
#'   probability `P` and observed outcome `Y` (only when `trajectory = TRUE`).
#'
#' Attributes `method`, `formula`, and `settings` carry the model metadata.
#'
#' @section Naming of coefficients:
#' The names of `r`/`rd` depend on the formula:
#'
#' - For a single bare character term such as `~ player(name)`, names are the
#'   level values themselves (e.g. `"Tomasz Gollob"`). This matches the naming
#'   used by [glicko_run()], [glicko2_run()] and [bbt_run()], so user code can
#'   index `coef(model)[name, ]` uniformly across methods.
#' - For multi-term or interaction formulas, each coefficient is qualified by
#'   its source column to keep the keys unambiguous:
#'   - bare character column: `"colname=value"`
#'   - bare numeric column: `"colname"` (single coefficient)
#'   - char x char interaction: `"a=val_a:b=val_b"`
#'   - char x num interaction: `"a=val_a:b"`
#'   - num x num interaction: `"a*b"` (single coefficient)
#'
#' When supplying prior `r`/`rd` via the function arguments the names must
#' follow the same convention.
#'
#' @examples
#' # the simplest example
#'
#' data <- data.frame(
#'   id = c(1, 1, 1, 1),
#'   name = c("A", "B", "C", "D"),
#'   rank = c(3, 4, 1, 2),
#'   gate = c(1, 2, 3, 4),
#'   factor1 = c("a", "a", "b", "b"),
#'   factor2 = c("a", "b", "a", "b")
#' )
#'
#' # Single-term formula -- coefficients named "A", "B", "C", "D".
#' dbl <- dbl_run(
#'   data = data,
#'   formula = rank | id ~ player(name)
#' )
#'
#' # Multi-term formula -- coefficients named "name=A", ..., "factor1=a:gate", ...
#' dbl <- dbl_run(
#'   data = data,
#'   formula = rank | id ~ player(name) + gate * factor1
#' )
#' @export
dbl_run <- function(formula,
                    data,
                    r = NULL,
                    rd = NULL,
                    lambda = NULL,
                    weight = NULL,
                    kappa = 0.95,
                    init_r = .defaults$dbl$init_r,
                    init_rd = .defaults$dbl$init_rd,
                    trajectory = TRUE) {
  if (is.null(kappa)) kappa <- 0.0001

  # ---- 1. Validate inputs ---------------------------------------------------
  # DBL allows arbitrary RHS terms (`+`/`*`/`:`), so `only_team_term = FALSE`,
  # but it does not allow nesting players in teams, so `single = TRUE`.
  .validate_data(data)
  .validate_formula(formula)
  .validate_lhs(formula, data)
  .validate_rhs(formula, data, only_team_term = FALSE, single = TRUE)

  # ---- 2. Pull row-aligned vectors from `data` ------------------------------
  ev <- .extract_event_vars(formula, data)
  lambda_vec <- .resolve_vec_arg(lambda, data, "lambda", min = 0)
  weight_vec <- .resolve_vec_arg(weight, data, "weight", min = 0)

  # ---- 3. Build the DBL design matrices -------------------------------------
  # Parse formula into a per-term class table, then build the parameter-key
  # map (`map`), the numeric design matrix (`x`), and the coarse class vector
  # (`cls`). The unique parameter keys become the rownames of r/rd, so each
  # row of `data` accumulates updates into the right coefficient slot.
  terms <- .parse_term_classes(data, formula)
  map <- .build_term_map(data, terms)
  x <- .build_design_matrix(data, terms)
  cls <- .term_classes(data, terms)
  unique_params <- unname(unlist(apply(map, 2, unique)))

  r <- .resolve_r(r, init_r, unique_params, "term columns")
  rd <- .resolve_rd(rd, init_rd, unique_params, "term columns")
  .check_equal_names(r, rd)

  # ---- 4. Dispatch into C++ -------------------------------------------------
  g <- dbl(
    unique_id = ev$unique_id,
    id = ev$id_vec,
    rank = ev$rank_vec,
    team = ev$team_vec,
    map = as.matrix(map),
    x = as.matrix(x),
    cls = cls,
    r = r,
    rd = rd,
    lambda = lambda_vec,
    weight = weight_vec,
    kappa = kappa,
    record_trajectory = trajectory
  )

  # ---- 5. Post-process the C++ result ---------------------------------------
  if (trajectory) {
    g <- .format_dbl_trajectory(g, formula)
  } else {
    g$r <- NULL
    g$p <- NULL
  }

  settings <- list(
    init_r = init_r, init_rd = init_rd,
    lambda = lambda, weight = weight,
    kappa = kappa
  )
  .as_ratings_from_run(g, method = "dbl", formula = formula, settings = settings)
}



#' Run a player-rating algorithm (glicko / glicko2 / bbt).
#'
#' Validates inputs, builds the row-aligned vectors and the named `r`/`rd`
#' (and `sigma` for glicko2) starting points, then dispatches to the
#' corresponding C++ implementation. Trajectory post-processing is applied
#' before returning.
#'
#' This is the dispatcher for the three Bradley-Terry-family methods. DBL
#' lives in [dbl_run()] directly because it doesn't share the player/team
#' parameter model.
#'
#' @inheritParams dot-rating_run
#' @return Raw C++ result list (`final_r`, `final_rd`, optionally
#'   `final_sigma`, `r`, `p`) ready to feed into [as.ratings()].
#' @noRd
.run_player_ratings <- function(
  method,
  data,
  formula,
  r = numeric(0),
  rd = numeric(0),
  sigma = numeric(0),
  init_r = numeric(0),
  init_rd = numeric(0),
  init_sigma = numeric(0),
  lambda = numeric(0),
  share = numeric(0),
  weight = numeric(0),
  kappa = 0.5,
  tau = 0.5,
  trajectory = TRUE
) {
  # ---- 1. Validate inputs ---------------------------------------------------
  .validate_data(data)
  .validate_formula(formula)
  .validate_lhs(formula, data)
  .validate_rhs(formula, data, only_team_term = TRUE, single = FALSE)

  .check_single_argument(init_r, "init_r", min = 0)
  .check_single_argument(init_rd, "init_rd", min = 0)
  if (method == "glicko2") {
    .check_single_argument(init_sigma, "init_sigma", min = 0.00000000001)
    .check_single_argument(tau, "tau", min = 0.00000000001)
  }

  # ---- 2. Pull row-aligned vectors from `data` ------------------------------
  ev <- .extract_event_vars(formula, data)

  player <- .get_player_var(formula)
  .validate_variables_present(c(ev$rank, ev$id, ev$team, player), data)
  player_vec <- if (length(player) == 0) ev$team_vec else as.character(data[[player]])

  .check_integer_argument(ev$id_vec, ev$id)
  .check_integer_argument(ev$rank_vec, ev$rank)
  .check_string_argument(player_vec, player)
  .check_string_argument(ev$team_vec, ev$team)

  lambda_vec <- .resolve_vec_arg(lambda, data, "lambda", min = 0)
  weight_vec <- .resolve_vec_arg(weight, data, "weight", min = 0)
  share_vec <- .resolve_vec_arg(share, data, "share", min = 0, max = 1)

  # ---- 3. Resolve r/rd/sigma starting points --------------------------------
  # The "universe" of names that r/rd/sigma must cover: every player seen in
  # the data plus any names already supplied by the caller (so we can continue
  # from a previous run that included extra players not in this batch).
  unique_names <- unique(c(unlist(player_vec), names(r), names(rd), names(sigma)))
  r <- .resolve_r(r, init_r, unique_names, player)
  rd <- .resolve_rd(rd, init_rd, unique_names, player)
  sigma <- .resolve_sigma(sigma, init_sigma, unique_names, player, method)
  .check_equal_names(r, rd)

  # glicko/bbt don't use sigma/init_sigma/tau but the C++ signature requires
  # them -- pass empty/zero placeholders so the do.call() stays uniform.
  if (method != "glicko2") {
    sigma <- numeric(0)
    init_sigma <- 0.0
    tau <- 0.0
  }

  # ---- 4. Dispatch into C++ -------------------------------------------------
  g <- do.call(method, list(
    unique_id = ev$unique_id,
    id = ev$id_vec,
    rank = ev$rank_vec,
    team = ev$team_vec,
    player = player_vec,
    r = r,
    rd = rd,
    sigma = sigma,
    share = share_vec,
    lambda = lambda_vec,
    weight = weight_vec,
    init_r = init_r,
    init_rd = init_rd,
    init_sigma = init_sigma,
    kappa = kappa,
    tau = tau,
    record_trajectory = trajectory
  ))

  # ---- 5. Post-process the C++ result ---------------------------------------
  if (trajectory) {
    g <- .format_player_trajectory(g, formula, method)
  } else {
    g$r <- NULL
    g$p <- NULL
  }
  g
}

#' Flatten the C++ per-event trajectory output and rename generic columns.
#'
#' The C++ side always emits `team`/`player`/`sigma` columns regardless of
#' method or formula shape; this function:
#'   * `rbindlist`s the per-event lists into single `data.table`s,
#'   * renames the generic `team`/`player` columns back to the user's
#'     variable names from the formula,
#'   * drops the `sigma` placeholder for glicko/bbt (which don't use it).
#'
#' Used only for player-rating methods. DBL has a different shape and uses
#' `.format_dbl_trajectory()` instead.
#'
#' @param g raw C++ result list (with `$r` and `$p` per-event lists).
#' @param formula model formula (used to look up the original column names).
#' @param method one of `"glicko"`, `"glicko2"`, `"bbt"`.
#' @return The mutated `g` with `g$r` and `g$p` replaced by formatted
#'   `data.table`s.
#' @noRd
.format_player_trajectory <- function(g, formula, method) {
  ratings <- data.table::rbindlist(g$r)
  pairs <- data.table::rbindlist(g$p)
  rhs_terms <- .get_team_term_vars(formula)
  id_name <- .get_id_name(formula)

  if (length(rhs_terms) == 1) {
    # Flat `player(team)` case: drop the redundant `player` column and
    # rename `team` to the user's variable name.
    ratings <- ratings[, "player" := NULL]
    names(ratings)[names(ratings) == "team"] <- rhs_terms[1]
    names(pairs)[names(pairs) == "team"] <- rhs_terms[1]
  } else {
    # Nested `player(player | team)` case: rename both columns.
    names(ratings)[names(ratings) == "player"] <- rhs_terms[1]
    names(ratings)[names(ratings) == "team"] <- rhs_terms[2]
    names(pairs)[names(pairs) == "team"] <- rhs_terms[2]
  }

  if (length(id_name) == 1) {
    names(ratings)[names(ratings) == "id"] <- id_name
    names(pairs)[names(pairs) == "id"] <- id_name
  }

  # glicko/bbt don't have sigma -- drop the placeholder column the C++ side
  # always emits to keep the union of column shapes constant.
  if (method %in% c("glicko", "bbt")) ratings <- ratings[, "sigma" := NULL]

  g$r <- ratings
  g$p <- pairs
  g
}

#' Flatten the C++ per-event trajectory output for DBL.
#'
#' DBL's per-event tables don't have a `player` column (parameters are
#' formula terms, not players), so the renaming is simpler.
#' @param g raw C++ result list.
#' @param formula model formula.
#' @return The mutated `g` with `g$r` and `g$p` replaced by formatted
#'   `data.table`s.
#' @noRd
.format_dbl_trajectory <- function(g, formula) {
  ratings <- data.table::rbindlist(g$r)
  pairs <- data.table::rbindlist(g$p)
  rhs_term <- .get_team_term_vars(formula)[1]
  names(ratings)[names(ratings) == "team"] <- rhs_term
  names(pairs)[names(pairs) == "team"] <- rhs_term
  id_name <- .get_id_name(formula)
  if (length(id_name) == 1) {
    names(ratings)[names(ratings) == "id"] <- id_name
    names(pairs)[names(pairs) == "id"] <- id_name
  }
  g$r <- ratings
  g$p <- pairs
  g
}

#' Wrap raw C++ output as a `"ratings"` object via [as.ratings()].
#' @noRd
.as_ratings_from_run <- function(g, method, formula, settings) {
  as.ratings(
    x          = g$final_r,
    rd         = g$final_rd,
    sigma      = if (method == "glicko2") g$final_sigma else numeric(0),
    method     = method,
    formula    = formula,
    settings   = settings,
    trajectory = g$r,
    pairs      = g$p
  )
}
