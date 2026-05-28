#' Coerce to a rating object
#'
#' Construct a `"ratings"` object from named vectors or a `data.frame`,
#' without running the full estimation. Useful when you already have fitted
#' ratings (e.g. from a previous run or an external source) and want to
#' [predict()] or continue estimation with a `*_run()` function.
#'
#' If `trajectory` and `pairs` are supplied they are attached to the returned
#' object (enabling [summary()], [print()], and rating-evolution plots);
#' otherwise the result is a predict-only "ratings" object.
#'
#' @param x object to coerce -- a named numeric vector of ratings (`r`) or a
#'   `data.frame` (see Methods).
#' @param ... further arguments passed to methods.
#'
#' @return An object of class `"ratings"`.
#'
#' @seealso [coef.ratings()], [predict.ratings()],
#'   [glicko_run()], [glicko2_run()], [bbt_run()], [dbl_run()]
#' @export
as.ratings <- function(x, ...) {
  UseMethod("as.ratings")
}

#' @describeIn as.ratings Coerce named numeric vectors to a `"ratings"` object.
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
#' @param settings optional pre-built settings list. When supplied it overrides
#'   `init_r`/`init_rd`/`init_sigma`/`lambda`/`share`. Used internally by the
#'   `*_run()` functions to forward their resolved settings into the resulting
#'   object.
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
#' est <- as.ratings(r, rd, method = "glicko", formula = rank | id ~ player(player))
#' class(est) # "ratings"
#'
#' # Predict
#' newdata <- data.frame(id = c(1, 1, 1, 1), player = c("a", "b", "c", "d"), rank = 1:4)
#' predict(est, newdata)
#'
#' # Extract coefficients
#' coef(est)
#' @export
as.ratings.numeric <- function(x,
                             rd,
                             method,
                             formula,
                             sigma = numeric(0),
                             init_r = NULL,
                             init_rd = NULL,
                             init_sigma = NULL,
                             lambda = numeric(0),
                             share = numeric(0),
                             settings = NULL,
                             trajectory = NULL,
                             pairs = NULL,
                             ...) {
  r <- x
  method <- match.arg(method, c("glicko", "glicko2", "bbt", "dbl"))

  if (is.null(settings)) {
    d <- .defaults[[method]]
    if (is.null(init_r)) init_r <- d$init_r
    if (is.null(init_rd)) init_rd <- d$init_rd
    if (is.null(init_sigma)) init_sigma <- d$init_sigma %||% 0.05
    settings <- list(
      init_r     = init_r,
      init_rd    = init_rd,
      init_sigma = init_sigma,
      lambda     = lambda,
      share      = share
    )
  }

  if (method == "glicko2" && length(sigma) == 0) {
    sigma <- setNames(rep(settings$init_sigma, length(r)), names(r))
  }

  obj <- list(r = r, rd = rd)
  if (method == "glicko2") obj$sigma <- sigma
  obj$coefficients <- if (method == "glicko2") {
    cbind(r = r, rd = rd, sigma = sigma)
  } else {
    cbind(r = r, rd = rd)
  }
  if (!is.null(trajectory)) obj$trajectory <- trajectory
  if (!is.null(pairs)) obj$pairs <- pairs

  structure(
    obj,
    class    = "ratings",
    method   = method,
    formula  = formula,
    settings = settings
  )
}

#' @describeIn as.ratings Coerce a data.frame to a `"ratings"` object.
#'
#' The `data.frame` must contain columns `r` and `rd` (and optionally
#' `sigma` for Glicko-2). Player names are resolved in order:
#' 1. Non-default rownames (if present).
#' 2. The first column that is not `r`, `rd`, or `sigma`.
#'
#' @examples
#' # from data.frame with rownames
#' df <- data.frame(r = c(1500, 1400), rd = c(200, 30), row.names = c("a", "b"))
#' est <- as.ratings(df, method = "glicko", formula = rank | id ~ player(player))
#'
#' # from data.frame with a name column
#' df2 <- data.frame(player = c("a", "b"), r = c(1500, 1400), rd = c(200, 30))
#' est2 <- as.ratings(df2, method = "glicko", formula = rank | id ~ player(player))
#' @export
as.ratings.data.frame <- function(x,
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

  as.ratings.numeric(
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

#' @describeIn as.ratings Reconstruct a `"ratings"` object from a plain
#'   list (typically the output of [as.list.ratings()] after a JSON
#'   round-trip via [jsonlite::fromJSON()] or similar). Expected fields:
#'   `method`, `formula` (character or formula), `r`, `rd`, optionally
#'   `sigma`, and optionally `settings`.
#'
#' @examples
#' # JSON round-trip
#' m <- glicko_run(rank | id ~ player(rider), data = gpheats[1:100, ])
#' \dontrun{
#'   payload <- jsonlite::toJSON(as.list(m), auto_unbox = TRUE)
#'   m2 <- as.ratings(jsonlite::fromJSON(payload))
#'   identical(m$r, m2$r)
#' }
#' @export
as.ratings.list <- function(x, ...) {
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

  # Forward the full settings list as-is so JSON round-trips preserve every
  # field (`weight`, `kappa`, `tau`, ...) -- not just the ones the
  # `as.ratings.numeric` default-builder happens to know about. JSON turns
  # `numeric(0)` into `list()`; coerce numeric-typed entries back so
  # downstream consumers see the shapes they expect.
  settings <- x$settings
  if (length(settings) > 0) {
    numeric_keys <- c("init_r", "init_rd", "init_sigma", "kappa", "tau",
                      "lambda", "share")
    for (k in intersect(names(settings), numeric_keys)) {
      v <- settings[[k]]
      settings[[k]] <- if (length(v) == 0) numeric(0) else as.numeric(unlist(v))
    }
  } else {
    settings <- NULL
  }

  as.ratings.numeric(
    x        = r,
    rd       = rd,
    method   = x$method,
    formula  = formula,
    sigma    = sigma,
    settings = settings
  )
}

#' @describeIn as.ratings Identity method -- returns `x` unchanged.
#'
#' @examples
#' glicko <- glicko_run(
#'   data = gpheats[1:16, ],
#'   formula = rank | id ~ player(rider)
#' )
#' identical(as.ratings(glicko), glicko)
#' @export
as.ratings.ratings <- function(x, ...) {
  x
}
