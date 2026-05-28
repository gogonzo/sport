#' Continue training a rating model on new data
#'
#' Re-fits the same rating algorithm starting from the existing player ratings
#' in `object`. The method, formula, and settings are taken from the object's
#' attributes -- you only need to supply `newdata`.
#'
#' @param object a `"ratings"` object returned by `glicko_run()`,
#'   `glicko2_run()`, `bbt_run()`, `dbl_run()`, or [as.ratings()].
#' @param newdata a `data.frame` of new events to fit, with the same columns
#'   as the original training data.
#' @param trajectory whether to record per-event ratings/pairs in the result.
#' @param ... additional arguments forwarded to the underlying `*_run()`
#'   function (e.g. `kappa`, `tau`, `weight`).
#'
#' @return A new `"ratings"` object reflecting both the prior ratings and
#'   the updates from `newdata`.
#'
#' @examples
#' m1 <- glicko_run(
#'   formula = rank | id ~ player(rider),
#'   data = gpheats[1:100, ]
#' )
#' m2 <- update(m1, newdata = gpheats[101:200, ])
#'
#' @importFrom stats update
#'
#' @export
update.ratings <- function(object, newdata, trajectory = TRUE, ...) {
  if (missing(newdata)) {
    stop("`newdata` is required to continue estimation", call. = FALSE)
  }

  method <- attr(object, "method")
  formula <- attr(object, "formula")
  settings <- attr(object, "settings")

  fn <- switch(method,
    glicko = glicko_run,
    glicko2 = glicko2_run,
    bbt = bbt_run,
    dbl = dbl_run,
    stop("Unknown method '", method, "'", call. = FALSE)
  )

  args <- list(
    formula    = formula,
    data       = newdata,
    r          = object$r,
    rd         = object$rd,
    trajectory = trajectory
  )
  if (method == "glicko2" && !is.null(object$sigma)) {
    args$sigma <- object$sigma
  }
  forwarded <- c(
    "init_r", "init_rd", "init_sigma",
    "lambda", "share", "weight",
    "kappa", "tau"
  )
  for (key in forwarded) {
    if (!is.null(settings[[key]])) args[[key]] <- settings[[key]]
  }
  # Caller-supplied `...` overrides settings.
  args <- utils::modifyList(args, list(...))

  # Drop args the target function doesn't accept (e.g. init_sigma for glicko)
  accepted <- names(formals(fn))
  args <- args[names(args) %in% accepted]

  do.call(fn, args)
}
