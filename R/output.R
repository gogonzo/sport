#' @import data.table
#' @importFrom stats coef
NULL

#' Extract coefficients from a rating model
#'
#' Returns a matrix of estimated coefficients from an `"ratings"` object.
#' Each row corresponds to a player (or parameter in DBL models) and columns
#' contain the rating (`r`), rating deviation (`rd`), and for Glicko-2 models
#' the volatility (`sigma`).
#'
#' @param object an object of class `"ratings"` (returned by `*_run()`
#'   functions or by [as.ratings()]).
#' @param ... ignored.
#'
#' @return A numeric matrix with one row per player and columns:
#'
#' - `r` -- rating (location estimate).
#' - `rd` -- rating deviation (uncertainty / posterior SD).
#' - `sigma` -- volatility (Glicko-2 only).
#'
#' @examples
#' # Glicko model
#' glicko <- glicko_run(
#'   formula = rank | id ~ player(rider),
#'   data = gpheats[1:16, ]
#' )
#' coef(glicko)
#'
#' # Glicko-2 includes sigma column
#' glicko2 <- glicko2_run(
#'   formula = rank | id ~ player(rider),
#'   data = gpheats[1:16, ]
#' )
#' coef(glicko2)
#'
#' @seealso [glicko_run()], [glicko2_run()], [bbt_run()], [dbl_run()],
#'   [as.ratings()]
#' @export
coef.ratings <- function(object, ...) {
  object$coefficients
}

#' Summarize a rating model
#'
#' Produces a summary of the fitted rating model including overall accuracy,
#' per-player ratings, and model vs observed win probabilities. Requires
#' the model to have been built with `trajectory = TRUE`.
#'
#' @param object an object of class `"ratings"`.
#' @param ... ignored.
#'
#' @return A list with elements:
#'
#' - `formula` -- the model formula.
#' - `method` -- algorithm used (`"glicko"`, `"glicko2"`, `"bbt"`, or `"dbl"`).
#' - `Overall Accuracy` -- proportion of correctly predicted pairwise outcomes.
#' - `Number of pairs` -- total number of pairwise comparisons.
#' - `r` -- a `data.table` with per-player ratings (`r`, `rd`) and, for
#'   single-variable models, aggregated model/true win probabilities and
#'   accuracy.
#'
#' @examples
#' model <- glicko_run(
#'   formula = rank | id ~ player(rider),
#'   data = gpheats[1:102, ]
#' )
#' summary(model)
#'
#' @seealso [print.ratings()], [coef.ratings()], [plot.ratings()]
#' @export
summary.ratings <- function(object, ...) {
  if (is.null(object$pairs)) {
    stop("summary() requires a model fitted with trajectory = TRUE", call. = FALSE)
  }
  formula <- attr(object, "formula")
  method <- attr(object, "method")
  team_terms <- .get_team_term_vars(formula)
  team_term_name <- .get_team_var(formula)
  player_term_name <- .get_player_var(formula)

  model_probs_teams <- object$pairs[
    , .(
      `Model probability` = round(mean(P), 3),
      `True probability` = round(mean(Y), 3),
      `Accuracy` = round(mean((P > .5) == Y), 3),
      `pairings` = length(P)
    ),
    team_term_name
  ]

  acc <- object$pairs[, .(`acc` = mean((P > .5) == Y), `pairings` = length(P)), ]

  players_ratings <- data.table(
    team = names(object$r),
    r = round(object$r, 3),
    rd = round(object$rd, 3)
  )
  names(players_ratings)[1] <- if (length(player_term_name) == 0) {
    team_term_name
  } else {
    player_term_name
  }

  r <- if (method == "dbl" || length(team_terms) == 2) {
    players_ratings
  } else {
    merge(players_ratings, model_probs_teams, by = team_term_name)
  }

  out <- list(
    formula = formula,
    method = method,
    `Overall Accuracy` = acc$acc,
    `Number of pairs` = acc$pairings,
    r = r
  )
  out
}


#' Print a rating model
#'
#' Displays overall accuracy and a calibration table showing model vs true
#' probabilities across predicted probability intervals. Requires the model
#' to have been built with `trajectory = TRUE`.
#'
#' @param x an object of class `"ratings"`.
#' @param ... ignored.
#' @return Invisibly returns `0`.
#'
#' @seealso [summary.ratings()], [coef.ratings()], [plot.ratings()]
#' @export
print.ratings <- function(x, ...) {
  if (is.null(x$pairs)) {
    cat(
      paste("\nCall:", format(attr(x, "formula"))),
      "\n(no trajectory recorded -- use trajectory = TRUE to enable summaries)\n",
      sep = "\n"
    )
    return(invisible(0))
  }
  model_probs_intervals <- x$pairs[
    , .(
      `Model probability` = round(mean(P), 3),
      `True probability` = round(mean(Y), 3),
      `Accuracy` = round(mean((P > .5) == Y), 3),
      `n` = length(P)
    ),
    list(Interval = cut(P, breaks = seq(0, 1, by = 0.1), include.lowest = TRUE))
  ][order(Interval)]


  out <- x$pairs[, .(n = length(P), `accuracy` = mean((P > .5) == Y))]

  cat(
    paste("\nCall:", format(attr(x, "formula"))),
    paste("\nNumber of unique pairs:", out$n / 2),
    paste("\nAccuracy of the model:", round(out$accuracy, 2)),
    "\nTrue probabilities and Accuracy in predicted intervals:",
    sep = "\n"
  )
  print(model_probs_intervals)
  invisible(0)
}

#' Plot a rating model (deprecated)
#'
#' `r lifecycle::badge("deprecated")`
#'
#' This method is deprecated and will be removed in a future release. The
#' built-in plots are intentionally simple and existed only as a convenience;
#' users are encouraged to build their own plots from `coef(x)` and
#' `x$trajectory` using whichever plotting package they prefer. Removing this
#' method also lets `sport` drop `ggplot2` from its hard dependencies.
#'
#' Two plot modes:
#' - **Without `players`**: a dot-and-interval plot of the top `n` players
#'   by rating, with 95\% intervals based on `rd`.
#' - **With `players`**: line plot showing rating evolution across events
#'   for the specified players (requires `trajectory = TRUE`).
#'
#' Calling this function requires `ggplot2` to be installed.
#'
#' @param x an object of class `"ratings"`.
#' @param n number of top players to show (default 10). Ignored when
#'   `players` is specified.
#' @param players optional character vector of player names whose rating
#'   trajectory to plot.
#' @param ... ignored.
#'
#' @return A `ggplot` object.
#'
#' @seealso [coef.ratings()], [summary.ratings()]
#' @export
plot.ratings <- function(x, n = 10, players, ...) {
  .Deprecated(
    msg = paste(
      "`plot.ratings()` is deprecated and will be removed in a future release.",
      "Build your own plot from `coef(x)` and `x$trajectory`."
    )
  )
  if (!requireNamespace("ggplot2", quietly = TRUE)) {
    stop(
      "`plot.ratings()` requires the `ggplot2` package to be installed.",
      call. = FALSE
    )
  }

  formula <- attr(x, "formula")
  variable <- .get_team_term_vars(formula)[1]
  if (!missing(players)) {
    if (is.null(x$trajectory)) {
      stop("plot(players=) requires a model fitted with trajectory = TRUE", call. = FALSE)
    }
    data <- x$trajectory[x$trajectory[[variable]] %in% players, ]
    ggplot2::ggplot(
      data,
      ggplot2::aes(
        x = .data$id,
        y = .data$r,
        group = .data[[variable]],
        color = .data[[variable]]
      )
    ) +
      ggplot2::geom_line() +
      ggplot2::ggtitle("Ratings evolution") +
      ggplot2::theme_bw()
  } else {
    data <- data.frame(
      team = names(x$r),
      r = x$r,
      rd = x$rd,
      row.names = NULL,
      stringsAsFactors = FALSE
    )
    names(data)[1] <- variable

    data <- data[order(data$r, decreasing = TRUE), ][1:n, ]
    data[[variable]] <- stats::reorder(data[[variable]], rev(seq_len(nrow(data))))
    ggplot2::ggplot(data, ggplot2::aes(x = .data[[variable]], y = .data$r)) +
      ggplot2::ggtitle("Actual ratings") +
      ggplot2::geom_linerange(
        ggplot2::aes(
          ymin = .data$r - .data$rd * 1.98,
          ymax = .data$r + .data$rd * 1.98
        ),
        linewidth = 1 * 0.8,
        alpha = 0.4
      ) +
      ggplot2::geom_point(colour = "grey20", size = 1) +
      ggplot2::coord_flip() +
      ggplot2::scale_x_discrete(variable) +
      ggplot2::scale_y_continuous("r") +
      ggplot2::theme_bw()
  }
}
