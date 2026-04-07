#' Predict pairwise probabilities from estimates
#'
#' Compute pairwise win probabilities for new matchups using an `"estimates"`
#' object. Players in `newdata` that are missing from the estimates are
#' automatically added with initial values (`init_r`, `init_rd`) and a
#' warning is issued.
#'
#' @param object an object of class `"estimates"` (from [as.sport()] or
#'   `model$estimates`).
#' @param newdata a `data.frame` containing the columns referenced by the
#'   model formula. The rank column may be omitted (defaults to 1).
#' @param ... ignored.
#'
#' @return A `data.table` with columns:
#'
#' - `id` — event identifier.
#' - `<player>` — player name (column name matches the formula).
#' - `opponent` — opponent name.
#' - `P` — predicted probability that player beats opponent.
#' - `Y` — predicted outcome (1 if P > 0.5, 0.5 if tied, 0 otherwise).
#'
#' @examples
#' r <- setNames(c(1500, 1400, 1550, 1700), c("a", "b", "c", "d"))
#' rd <- setNames(c(200, 30, 100, 300), c("a", "b", "c", "d"))
#' est <- as.sport(r, rd, method = "glicko", formula = rank | id ~ player(player))
#' predict(est, data.frame(id = c(1, 1, 1, 1), player = c("a", "b", "c", "d"), rank = 1:4))
#'
#' @seealso [as.sport()], [coef.estimates()]
#' @export
predict.estimates <- function(object, newdata, ...) {
  if (missing(newdata)) stop("newdata is requested to predict", call. = FALSE)

  method <- attr(object, "method")
  formula <- attr(object, "formula")
  settings <- attr(object, "settings")

  all_vars <- all.vars(formula)
  .is_newdata_consistent(
    c(.get_team_name(formula), .get_player_name(formula), all_vars[-1]),
    colnames(newdata)
  )

  if (!all_vars[1] %in% colnames(newdata)) {
    newdata[all_vars[1]] <- 1
  }

  rank <- .get_rank_name(formula)
  rank_vec <- as.integer(newdata[[rank]])

  id <- .get_id_name(formula)
  id_vec <- if (length(id) == 0) rep(1L, nrow(newdata)) else as.integer(newdata[[id]])

  team <- .get_team_name(formula)
  player <- .get_player_name(formula)
  team_vec <- as.character(newdata[[team]])
  player_vec <- if (length(player) == 0) team_vec else as.character(newdata[[player]])

  unique_id <- unique(id_vec)
  lambda_vec <- .initialize_vec(var = settings$lambda, data = newdata, argname = "lambda", min = 0)
  share_vec <- .initialize_vec(var = settings$share, data = newdata, argname = "share", min = 0, max = 1)

  r <- object$r
  rd_vec_model <- object$rd
  missing <- character(0)

  if (method != "dbl") {
    unique_names <- unique(player_vec)
    missing <- setdiff(unique_names, names(r))
    if (length(missing) > 0) {
      warning(
        sprintf(
          "Some names in newdata doesn't have a match in r.\n              Missing parameters will be added with init_r = %s",
          settings$init_r
        ),
        call. = FALSE
      )
      warning(
        sprintf(
          "Some names in newdata doesn't have a match in rd.\n              Missing parameters will be added with init_rd = %s",
          settings$init_rd
        ),
        call. = FALSE
      )
      r <- c(r, setNames(rep(settings$init_r, length(missing)), missing))
      rd_vec_model <- c(rd_vec_model, setNames(rep(settings$init_rd, length(missing)), missing))
    }
  }

  sigma_vec_model <- if (method == "glicko2") {
    s <- object$sigma
    if (length(missing) > 0) {
      s <- c(s, setNames(rep(settings$init_sigma, length(missing)), missing))
    }
    s
  } else {
    numeric(0)
  }

  out_p <- if (method == "glicko") {
    predict_glicko(
      unique_id = unique_id,
      id_vec = id_vec,
      rank_vec = rank_vec,
      team_vec = team_vec,
      player_vec = player_vec,
      r = r,
      rd = rd_vec_model,
      share_vec = share_vec,
      lambda_vec = lambda_vec
    )
  } else if (method == "glicko2") {
    predict_glicko2(
      unique_id = unique_id,
      id_vec = id_vec,
      rank_vec = rank_vec,
      team_vec = team_vec,
      player_vec = player_vec,
      r = r,
      rd = rd_vec_model,
      sigma = sigma_vec_model,
      share_vec = share_vec,
      lambda_vec = lambda_vec
    )
  } else if (method == "bbt") {
    predict_bbt(
      unique_id = unique_id,
      id_vec = id_vec,
      rank_vec = rank_vec,
      team_vec = team_vec,
      player_vec = player_vec,
      r = r,
      rd = rd_vec_model,
      share_vec = share_vec,
      lambda_vec = lambda_vec
    )
  } else if (method == "dbl") {
    terms <- .get_terms(newdata, formula)
    map <- .get_terms_map(newdata, terms)
    x <- .get_terms_mat(newdata, terms)

    predict_dbl(
      unique_id = unique_id,
      id_vec = id_vec,
      rank_vec = rank_vec,
      team_vec = team_vec,
      MAP = as.matrix(map),
      X = as.matrix(x),
      R = r,
      RD = rd_vec_model,
      lambda_vec = lambda_vec
    )
  }

  pairs <- data.table::rbindlist(out_p)

  rhs_terms <- .extract_team_terms(formula)
  if (length(rhs_terms) == 1) {
    names(pairs)[names(pairs) == "team"] <- rhs_terms[1]
  } else {
    names(pairs)[names(pairs) == "team"] <- rhs_terms[2]
  }

  pairs <- pairs[, Y := ifelse(P > .5, 1, ifelse(P == .5, .5, 0))]

  pairs
}


