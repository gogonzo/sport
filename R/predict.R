#' Predict rating model
#'
#' Predict rating model
#' @param object of class rating
#' @param newdata data.frame with data to predict
#' @param ... optional arguments
#' @return probabilities of winning challange by player over his opponent in all provided events.
#' @examples
#' glicko <- glicko_run(rank | id ~ team(rider), gpheats[1:16, ])
#' predict(glicko, gpheats[17:20, ])
#' @export
predict.rating <- function(object, newdata, ...) {
  if (missing(newdata)) stop("newdata is requested to predict", call. = F)
  method <- attr(object, "method")
  formula <- attr(object, "formula")
  all_vars <- terms(formula)

  #is_newdata_consistent(c(lhs, rhs), colnames(newdata))

  model <- if (method == "glicko") {
    browser()
    glicko_run(
      formula = formula,
      data = newdata,
      r  = object$final_r,
      rd = object$final_rd,
      lambda = attr(object, "settings")$lambda,
      share  = attr(object, "settings")$share,
      weight = attr(object, "settings")$weight,
      #idlab  = attr(object, "settings")$idlab,
      init_r = attr(object, "settings")$init_r,
      init_rd = attr(object, "settings")$init_rd,
      kappa  = attr(object, "settings")$kappa
    )

  } else if (method == "glicko2") {
    glicko2_run(
      formula = formula,
      data = newdata,
      r     = object$final_r,
      rd    = object$final_rd,
      sigma = object$final_sigma,
      lambda = attr(object, "settings")$lambda,
      share  = attr(object, "settings")$share,
      weight = attr(object, "settings")$weight,
      #idlab  = attr(object, "settings")$idlab,
      init_r = attr(object, "settings")$init_r,
      init_rd = attr(object, "settings")$init_rd,
      init_sigma = attr(object, "settings")$init_sigma,
      kappa  = attr(object, "settings")$kappa
    )
  } else if (method == "bbt") {
    bbt_run(
      formula = formula,
      data = newdata,
      r  = object$final_r,
      rd = object$final_rd,
      lambda = attr(object, "settings")$lambda,
      share  = attr(object, "settings")$share,
      weight = attr(object, "settings")$weight,
      #idlab  = attr(object, "settings")$idlab,
      init_r = attr(object, "settings")$init_r,
      init_rd = attr(object, "settings")$init_rd,
      kappa  = attr(object, "settings")$kappa
    )
  } else if (method == "dbl") {
    dbl_run(
      formula = formula,
      data = newdata,
      r  = object$final_r,
      rd = object$final_rd,
      lambda = attr(object, "settings")$lambda,
      weight = attr(object, "settings")$weight,
      #idlab  = attr(object, "settings")$idlab,
      init_r = attr(object, "settings")$init_r,
      init_rd = attr(object, "settings")$init_rd,
      kappa  = attr(object, "settings")$kappa
    )
  }


  return(model)
}
