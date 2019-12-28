#' @importFrom data.table rbindlist
#' @importFrom stats setNames terms update
NULL

#' DBL rating algorithm
#'
#' DBL rating algorithm
#' Wrapper arround `dbl` update algorithm. Wrapper allows user to simplify calculation providing only data and initial parameters assumptions
#' @param formula formula specifying model. DBL allows multiple variables in formula, also two-way interaction are available specified by `:`. `dbl` formula require first variable to be player name or identifier. LHS needs `rank|id`, to specify competitors order and event `id`.
#' @param data data.frame which contains columns specified in formula, and optionaly columns defined by `beta`, `weight` or `date`.
#' @param r named vector of initial estimates. If there is no assumption, initial ratings is set to be r=0.
#' @param rd named vector of initial variance of `r` estimates. In there is no assumption, initial is set to be rd=1.
#' @param beta The additional variance of performance. As beta increases, the performance is more uncertain and update change is smaller. By default `beta = 25/6`.
#' @param weight name of column in `data` containing weights. Weights increasing or decreasing update change. Higher weight increasing impact of corresponding event.
#' @param kappa parameter controlling `rd` to avoid quick decreasing to zero. Is the proportion of `rd` which is maximum change size.
#' @param init_r initial values for `r` if not provided. Default `r=0`
#' @param init_rd initial values for `rd` if not provided. Default `rd=1`
#' @return
#' A "rating" object is returned
#' \itemize{
#'   \item \code{final_r} named vector containing players ratings.
#'   \item \code{final_rd} named vector containing players ratings deviations.
#'   \item \code{r} data.frame with evolution of the ratings and ratings deviations estimated at each event.
#'   \item \code{pairs} pairwise combinations of players in analysed events with prior probability and result of a challange.
#'   \item \code{class} of the object
#'   \item \code{method} type of algorithm used
#'   \item \code{formula} modelled formula
#' }
#' @examples
#' data <- data.frame(
#'   name = c("A", "B", "C", "D"),
#'   rank = c(3, 4, 1, 2)
#' )
#' dbl <- dbl_run(rank ~ name, data)
#' @export
dbl_run <- function(formula,
                    data,
                    r,
                    rd,
                    beta,
                    weight,
                    kappa = 0.5,
                    init_r = 0,
                    init_rd = 1) {
  is_formula_missing(formula)
  is_data_provided(data)
  is_lhs_valid(formula)
  is_interactions_valid(formula)

  browser()
  
  lhs <- all.vars(update(formula, . ~ 0))
  rank <- lhs[1]
  rank_vec <- as.integer(data[[rank]])
  
  if (length(lhs) == 1) {
    id <- "id"
    id_vec <- rep(1L, nrow(data))
  } else {
    id <- lhs[2]
    id_vec <- as.integer(data[[lhs[2]]])
  }
  
  terms   <- get_terms(data, formula)
  X <- get_terms_matrix(data, terms)
  unique_params <- unname(unlist(apply(X, 2, unique)))
  
  if (missing(r)) {
    r <- setNames(rep(init_r, length(unique_params)), unique_params)
  }
  if (missing(rd)) {
    rd <- setNames(rep(init_rd, length(unique_params)), unique_params)
  }
  
  if (is.null(weight)) {
    weight_vec <- rep(1.0, nrow(data))
    weight <- "weight"
  }
  if (is.null(beta)) {
    beta_vec <- 1.0
    beta <- "beta"
  }
  
  if (is.null(kappa)) kappa <- 0.0001

  model <- dbl(
    unique_id = unique(id_vec),
    id = id_vec,
    rank_vec = rank_vec,
    team_vec = team_vec,
    X = as.matrix(terms),
    R = r,
    RD = rd,
    beta = beta_vec,
    weight = weight_vec,
    kappa = kappa
  )

  return(list())
}
