#' @import data.table
#' @import ggplot2
#' @importFrom stats reorder coef
NULL

#' Extract coefficients from a rating model
#'
#' Returns a matrix of estimated coefficients from an `"estimates"` object.
#' Each row corresponds to a player (or parameter in DBL models) and columns
#' contain the rating (`r`), rating deviation (`rd`), and for Glicko-2 models
#' the volatility (`sigma`).
#'
#' @param object an object of class `"estimates"` (returned by `*_run()`
#'   functions or by [as.sport()]).
#' @param ... ignored.
#'
#' @return A numeric matrix with one row per player and columns:
#'
#' - `r` — rating (location estimate).
#' - `rd` — rating deviation (uncertainty / posterior SD).
#' - `sigma` — volatility (Glicko-2 only).
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
#'   [as.sport()]
#' @export
coef.estimates <- function(object, ...) {
  object$coefficients
}

#' Coerce a rating model to a plain list
#'
#' Returns a JSON-friendly representation of an `"estimates"` object: rating
#' vectors and metadata only. The cached `coefficients` matrix and any
#' `trajectory` / `pairs` data.tables are dropped — they are either regenerable
#' (`coefficients`) or too large to store as model parameters. The `formula` is
#' deparsed to a character string so the result serializes cleanly with
#' `jsonlite::toJSON()` or similar.
#'
#' Use [as.sport()] (the `list` method) to reconstruct an `"estimates"` object
#' from the result.
#'
#' @param x an `"estimates"` object.
#' @param ... ignored.
#'
#' @return A plain list with elements `method`, `formula` (character),
#'   `settings`, `r`, `rd`, and (for glicko2) `sigma`.
#'
#' @examples
#' m <- glicko_run(rank | id ~ player(rider), data = gpheats[1:100, ])
#' params <- as.list(m)
#' str(params)
#'
#' # Round-trip
#' m2 <- as.sport(params)
#' identical(m$r, m2$r)
#'
#' \dontrun{
#' # Persist to a database as JSON.
#' # Pass `digits = NA` so jsonlite preserves full floating-point precision —
#' # the default of 4 digits will quantise the ratings.
#' payload <- jsonlite::toJSON(as.list(m), auto_unbox = TRUE, digits = NA)
#' DBI::dbWriteTable(con, "models", data.frame(id = 1L, payload = payload))
#' }
#'
#' @seealso [as.sport()], [coef.estimates()]
#' @export
as.list.estimates <- function(x, ...) {
  formula <- attr(x, "formula")
  # Convert named numerics to named lists so jsonlite::toJSON serializes
  # them as JSON objects (preserving names) rather than bare arrays.
  out <- list(
    method   = attr(x, "method"),
    formula  = paste(deparse(formula), collapse = " "),
    settings = attr(x, "settings"),
    r        = as.list(x$r),
    rd       = as.list(x$rd)
  )
  if (!is.null(x$sigma)) out$sigma <- as.list(x$sigma)
  out
}

#' Summarize a rating model
#'
#' Produces a summary of the fitted rating model including overall accuracy,
#' per-player ratings, and model vs observed win probabilities. Requires
#' the model to have been built with `trajectory = TRUE`.
#'
#' @param object an object of class `"estimates"`.
#' @param ... ignored.
#'
#' @return A list with elements:
#'
#' - `formula` — the model formula.
#' - `method` — algorithm used (`"glicko"`, `"glicko2"`, `"bbt"`, or `"dbl"`).
#' - `Overall Accuracy` — proportion of correctly predicted pairwise outcomes.
#' - `Number of pairs` — total number of pairwise comparisons.
#' - `r` — a `data.table` with per-player ratings (`r`, `rd`) and, for
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
#' @seealso [print.estimates()], [coef.estimates()], [plot.estimates()]
#' @export
summary.estimates <- function(object, ...) {
  if (is.null(object$pairs)) {
    stop("summary() requires a model fitted with trajectory = TRUE", call. = FALSE)
  }
  formula <- attr(object, "formula")
  method <- attr(object, "method")
  team_terms <- extract_team_terms(formula)
  team_term_name <- get_team_name(formula)
  player_term_name <- get_player_name(formula)

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
#' @param x an object of class `"estimates"`.
#' @param ... ignored.
#' @return Invisibly returns `0`.
#'
#' @seealso [summary.estimates()], [coef.estimates()], [plot.estimates()]
#' @export
print.estimates <- function(x, ...) {
  if (is.null(x$pairs)) {
    cat(
      paste("\nCall:", format(attr(x, "formula"))),
      "\n(no trajectory recorded — use trajectory = TRUE to enable summaries)\n",
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

#' Plot a rating model
#'
#' Two plot modes:
#' - **Without `players`**: a dot-and-interval plot of the top `n` players
#'   by rating, with 95\% intervals based on `rd`.
#' - **With `players`**: line plot showing rating evolution across events
#'   for the specified players (requires `trajectory = TRUE`).
#'
#' @param x an object of class `"estimates"`.
#' @param n number of top players to show (default 10). Ignored when
#'   `players` is specified.
#' @param players optional character vector of player names whose rating
#'   trajectory to plot.
#' @param ... ignored.
#'
#' @return A `ggplot` object.
#'
#' @examples
#' glicko <- glicko_run(
#'   formula = rank | id ~ player(rider),
#'   data = gpheats[1:500, ]
#' )
#'
#' # Top 10 players by rating
#' plot(glicko)
#'
#' # Rating evolution for specific players
#' plot(glicko, players = c("Tomasz Gollob", "Tony Rickardsson"))
#'
#' @seealso [coef.estimates()], [summary.estimates()]
#' @export
plot.estimates <- function(x, n = 10, players, ...) {
  formula <- attr(x, "formula")
  variable <- extract_team_terms(formula)[1]
  if (!missing(players)) {
    if (is.null(x$trajectory)) {
      stop("plot(players=) requires a model fitted with trajectory = TRUE", call. = FALSE)
    }
    data <- x$trajectory[x$trajectory[[variable]] %in% players, ]
    ggplot(
      data,
      aes(
        x = .data$id,
        y = .data$r,
        group = .data[[variable]],
        color = .data[[variable]]
      )
    ) +
      geom_line() +
      ggtitle("Ratings evolution") +
      theme_bw()
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
    data[[variable]] <- reorder(data[[variable]], rev(seq_len(nrow(data))))
    ggplot(data, aes(x = .data[[variable]], y = .data$r)) +
      ggtitle("Actual ratings") +
      geom_linerange(
        aes(
          ymin = .data$r - .data$rd * 1.98,
          ymax = .data$r + .data$rd * 1.98
        ),
        linewidth = 1 * 0.8,
        alpha = 0.4
      ) +
      geom_point(colour = "grey20", size = 1) +
      coord_flip() +
      scale_x_discrete(variable) +
      scale_y_continuous("r") +
      theme_bw()
  }
}
