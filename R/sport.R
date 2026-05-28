#' @useDynLib sport
#' @importFrom Rcpp sourceCpp
NULL

if (getRversion() >= "2.15.1") {
  utils::globalVariables(c(".", "P", "Y", "Interval", ".data", ":="))
}

#' Default initial values per method.
#' Used by `glicko_run()`, `glicko2_run()`, `bbt_run()`, `dbl_run()` whenever
#' the user does not provide explicit `init_r` / `init_rd` / `init_sigma`.
#' Each algorithm operates on its own natural rating scale, so the defaults
#' differ substantially across methods (e.g. Glicko's 1500 vs BBT's 25).
#' @noRd
.defaults <- list(
  glicko  = list(init_r = 1500,  init_rd = 350),
  glicko2 = list(init_r = 1500,  init_rd = 350, init_sigma = 0.05),
  bbt     = list(init_r = 25,    init_rd = 25 / 3),
  dbl     = list(init_r = 0,     init_rd = 1)
)
