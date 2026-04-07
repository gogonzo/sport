#' Coerce to a rating object
#'
#' Construct an `"estimates"` object from named vectors or a `data.frame`,
#' without running the full estimation. Useful when you already have fitted
#' ratings (e.g. from a previous run or an external source) and want to
#' [predict()] or continue estimation with a `*_run()` function.
#'
#' If `trajectory` and `pairs` are supplied they are attached to the returned
#' object (enabling [summary()], [print()], and rating-evolution plots);
#' otherwise the result is a predict-only "estimates" object.
#'
#' @param x object to coerce — a named numeric vector of ratings (`r`) or a
#'   `data.frame` (see Methods).
#' @param ... further arguments passed to methods.
#'
#' @return An object of class `"estimates"`.
#'
#' @seealso [coef.estimates()], [predict.estimates()],
#'   [glicko_run()], [glicko2_run()], [bbt_run()], [dbl_run()]
#' @export
as.sport <- function(x, ...) {
  UseMethod("as.sport")
}

#' @describeIn as.sport Coerce named numeric vectors to an `"estimates"` object.
#'
#' @param rd named numeric vector of rating deviations. Must have the same
#'   names as `x`.
#' @param method one of `"glicko"`, `"glicko2"`, `"bbt"`, `"dbl"`.
#' @param formula model formula (same format as in `*_run()` functions).
#' @param sigma named numeric vector of volatilities (Glicko-2 only).
#' @param init_r initial rating assigned to unknown players during
#'   [predict()]. Defaults to the method-specific value in `.defaults`.
#' @param init_rd initial rating deviation for unknown players. Defaults to
#'   the method-specific value in `.defaults`.
#' @param init_sigma initial sigma for unknown players (Glicko-2 only).
#' @param lambda column name or constant passed through to [predict()].
#' @param share column name or constant passed through to [predict()].
#' @param trajectory optional `data.table` of rating history (as returned in
#'   `model$r`). When provided together with `pairs`, the resulting object
#'   carries `$r` and `$pairs` for use with [summary()]/[plot()].
#' @param pairs optional `data.table` of pairwise comparisons (as returned in
#'   `model$pairs`).
#'
#' @examples
#' # Build predict-only estimates from vectors
#' r <- setNames(c(1500, 1400, 1550, 1700), c("a", "b", "c", "d"))
#' rd <- setNames(c(200, 30, 100, 300), c("a", "b", "c", "d"))
#' est <- as.sport(r, rd, method = "glicko", formula = rank | id ~ player(player))
#' class(est) # "estimates"
#'
#' # Predict
#' newdata <- data.frame(id = c(1, 1, 1, 1), player = c("a", "b", "c", "d"), rank = 1:4)
#' predict(est, newdata)
#'
#' # Extract coefficients
#' coef(est)
#' @export
as.sport.numeric <- function(x,
                             rd,
                             method,
                             formula,
                             sigma = numeric(0),
                             init_r = NULL,
                             init_rd = NULL,
                             init_sigma = NULL,
                             lambda = numeric(0),
                             share = numeric(0),
                             trajectory = NULL,
                             pairs = NULL,
                             ...) {
  r <- x
  method <- match.arg(method, c("glicko", "glicko2", "bbt", "dbl"))

  d <- .defaults[[method]]
  if (is.null(init_r)) init_r <- d$init_r
  if (is.null(init_rd)) init_rd <- d$init_rd
  if (is.null(init_sigma)) init_sigma <- d$init_sigma %||% 0.05

  if (method == "glicko2" && length(sigma) == 0) {
    sigma <- setNames(rep(init_sigma, length(r)), names(r))
  }

  settings <- list(
    init_r     = init_r,
    init_rd    = init_rd,
    init_sigma = init_sigma,
    lambda     = lambda,
    share      = share
  )

  has_traj <- !is.null(trajectory) && !is.null(pairs)
  g <- list(
    final_r = r,
    final_rd = rd,
    final_sigma = if (method == "glicko2") sigma else NULL,
    r = if (has_traj) trajectory else NULL,
    p = if (has_traj) pairs else NULL
  )

  .build_rating(
    g,
    method = method,
    formula = formula,
    settings = settings,
    trajectory = has_traj
  )
}

#' @describeIn as.sport Coerce a data.frame to estimates or rating.
#'
#' The `data.frame` must contain columns `r` and `rd` (and optionally
#' `sigma` for Glicko-2). Player names are resolved in order:
#' 1. Non-default rownames (if present).
#' 2. The first column that is not `r`, `rd`, or `sigma`.
#'
#' @examples
#' # from data.frame with rownames
#' df <- data.frame(r = c(1500, 1400), rd = c(200, 30), row.names = c("a", "b"))
#' est <- as.sport(df, method = "glicko", formula = rank | id ~ player(player))
#'
#' # from data.frame with a name column
#' df2 <- data.frame(player = c("a", "b"), r = c(1500, 1400), rd = c(200, 30))
#' est2 <- as.sport(df2, method = "glicko", formula = rank | id ~ player(player))
#' @export
as.sport.data.frame <- function(x,
                                method,
                                formula,
                                sigma = numeric(0),
                                init_r = NULL,
                                init_rd = NULL,
                                init_sigma = NULL,
                                lambda = numeric(0),
                                share = numeric(0),
                                trajectory = NULL,
                                pairs = NULL,
                                ...) {
  if (!all(c("r", "rd") %in% names(x))) {
    stop("data.frame must contain 'r' and 'rd' columns", call. = FALSE)
  }

  known_cols <- intersect(c("r", "rd", "sigma"), names(x))
  name_col <- setdiff(names(x), known_cols)

  has_real_rownames <- !is.null(rownames(x)) &&
    !identical(rownames(x), as.character(seq_len(nrow(x))))

  if (has_real_rownames) {
    players <- rownames(x)
  } else if (length(name_col) >= 1) {
    players <- as.character(x[[name_col[1]]])
  } else {
    stop(
      "Cannot determine player names: set rownames or include a name column",
      call. = FALSE
    )
  }

  r <- setNames(x$r, players)
  rd <- setNames(x$rd, players)
  s <- if ("sigma" %in% names(x)) setNames(x$sigma, players) else sigma

  as.sport.numeric(
    x          = r,
    rd         = rd,
    method     = method,
    formula    = formula,
    sigma      = s,
    init_r     = init_r,
    init_rd    = init_rd,
    init_sigma = init_sigma,
    lambda     = lambda,
    share      = share,
    trajectory = trajectory,
    pairs      = pairs
  )
}

#' @describeIn as.sport Reconstruct an `"estimates"` object from a plain
#'   list (typically the output of [as.list.estimates()] after a JSON
#'   round-trip via [jsonlite::fromJSON()] or similar). Expected fields:
#'   `method`, `formula` (character or formula), `r`, `rd`, optionally
#'   `sigma`, and optionally `settings`.
#'
#' @examples
#' # JSON round-trip
#' m <- glicko_run(rank | id ~ player(rider), data = gpheats[1:100, ])
#' \dontrun{
#'   payload <- jsonlite::toJSON(as.list(m), auto_unbox = TRUE)
#'   m2 <- as.sport(jsonlite::fromJSON(payload))
#'   identical(m$r, m2$r)
#' }
#' @export
as.sport.list <- function(x, ...) {
  if (is.null(x$method)) stop("list must contain `method`", call. = FALSE)
  if (is.null(x$formula)) stop("list must contain `formula`", call. = FALSE)
  if (is.null(x$r) || is.null(x$rd)) {
    stop("list must contain `r` and `rd`", call. = FALSE)
  }

  formula <- if (inherits(x$formula, "formula")) {
    x$formula
  } else {
    stats::as.formula(x$formula)
  }

  # JSON round-trip turns named numerics into named lists; unlist normalises
  # both shapes back to a named numeric vector.
  r  <- unlist(x$r)
  rd <- unlist(x$rd)
  sigma <- if (!is.null(x$sigma)) unlist(x$sigma) else numeric(0)

  settings <- x$settings %||% list()

  as.sport.numeric(
    x          = r,
    rd         = rd,
    method     = x$method,
    formula    = formula,
    sigma      = sigma,
    init_r     = settings$init_r,
    init_rd    = settings$init_rd,
    init_sigma = settings$init_sigma,
    lambda     = settings$lambda %||% numeric(0),
    share      = settings$share  %||% numeric(0)
  )
}

#' @describeIn as.sport Identity method — returns `x` unchanged.
#'
#' @examples
#' glicko <- glicko_run(
#'   data = gpheats[1:16, ],
#'   formula = rank | id ~ player(rider)
#' )
#' identical(as.sport(glicko), glicko)
#' @export
as.sport.estimates <- function(x, ...) {
  x
}
