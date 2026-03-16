#' @useDynLib sport
#' @importFrom Rcpp sourceCpp
NULL

if (getRversion() >= "2.15.1") {
  utils::globalVariables(c(".", "P", "Y", "Interval", ".data", ":="))
}

#' Default initial values per method
#' @noRd
.defaults <- list(
  glicko  = list(init_r = 1500,  init_rd = 350),
  glicko2 = list(init_r = 1500,  init_rd = 350, init_sigma = 0.05),
  bbt     = list(init_r = 25,    init_rd = 25 / 3),
  dbl     = list(init_r = 0,     init_rd = 1)
)
