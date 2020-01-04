#' Predict rating model
#'
#' Predict rating model
#' @param object of class rating
#' @param newdata data.frame with data to predict
#' @param ... optional arguments
#' @return probabilities of winning challenge by player over his opponent in all provided events.
#' @examples
#' glicko <- glicko_run(data = gpheats[1:16, ], 
#'                      formula = rank | id ~ player(rider))
#' predict(glicko, gpheats[17:20, ])
#' @export
predict.rating <- function(object, newdata, ...) {
  if (missing(newdata)) stop("newdata is requested to predict", call. = F)
  method <- attr(object, "method")
  formula <- attr(object, "formula")
  all_vars <- all.vars(formula)
  is_newdata_consistent(
    c(get_team_name(formula), get_player_name(formula), all_vars[-1]), 
    colnames(newdata)
  )
  
  if (!all_vars[1] %in% colnames(newdata)) {
    newdata[all_vars[1]] <- 1
  }
  
  model <- if (method == "glicko") {
      tryCatch(
        model <- glicko_run(
          formula = formula,
          data = newdata,
          r  = object$final_r,
          rd = object$final_rd,
          lambda = attr(object, "settings")$lambda,
          share  = attr(object, "settings")$share,
          weight = attr(object, "settings")$weight,
          init_r = attr(object, "settings")$init_r,
          init_rd = attr(object, "settings")$init_rd,
          kappa  = attr(object, "settings")$kappa
        )
      )
      
    } else if (method == "glicko2") {
      tryCatch(
        glicko2_run(
          formula = formula,
          data = newdata,
          r     = object$final_r,
          rd    = object$final_rd,
          sigma = object$final_sigma,
          lambda = attr(object, "settings")$lambda,
          share  = attr(object, "settings")$share,
          weight = attr(object, "settings")$weight,
          init_r = attr(object, "settings")$init_r,
          init_rd = attr(object, "settings")$init_rd,
          init_sigma = attr(object, "settings")$init_sigma,
          kappa  = attr(object, "settings")$kappa
        )
      )
    } else if (method == "bbt") {
      tryCatch(
        bbt_run(
          formula = formula,
          data = newdata,
          r  = object$final_r,
          rd = object$final_rd,
          lambda = attr(object, "settings")$lambda,
          share  = attr(object, "settings")$share,
          weight = attr(object, "settings")$weight,
          init_r = attr(object, "settings")$init_r,
          init_rd = attr(object, "settings")$init_rd,
          kappa  = attr(object, "settings")$kappa
        )
      )
    } else if (method == "dbl") {
      tryCatch(
        dbl_run(
          formula = formula,
          data = newdata,
          r  = object$final_r,
          rd = object$final_rd,
          lambda = attr(object, "settings")$lambda,
          weight = attr(object, "settings")$weight,
          init_r = attr(object, "settings")$init_r,
          init_rd = attr(object, "settings")$init_rd,
          kappa  = attr(object, "settings")$kappa
        )
      )
    }
  
  P <- model$pairs
  P <- P[, Y := ifelse(P > .5, 1, ifelse(P == .5, .5, 0))]

  return(P)
}
