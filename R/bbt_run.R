#' @importFrom data.table rbindlist
#' @importFrom stats setNames terms update
NULL

#' BBT rating algorithm
#' 
#' BBT rating algorithm
#' Wrapper arround `bbt` update algorithm. Wrapper allows user to simplify calculation providing only data and initial parameters assumptions
#' @param formula formula specifying model. BBT algorithm allows only player ranking parameter and should be specified by following manner: 
#' `rank | id ~ name`. Names in formula are unrestricted, but model structure remains the same:
#' \itemize{
#'  \item rank player position in event.
#'  \item id event identifier in which pairwise comparison is assessed.
#'  \item name of player.
#' }
#' @param data data.frame which contains columns specified in formula, and optionaly columns defined by `sigma`, `weight` or `date`.
#' @param r named vector of initial rating estimates. In there is no assumption, initial ratings is set to be r=25 Names of vector should correspond with `name` in formula. 
#' @param rd named vector of initial rating deviation estimates. In there is no assumption, initial is set to be r=25/3 Names of vector should correspond with `name` in formula.
#' @param sigma name of column in `data` containing rating volatility. Rating volitality is a value which multiplies prior `rd`. If `sigma > 0` then prior `rd` increases, making estimate of `r` more uncertain.
#' @param weight name of column in `data` containing weights. Weights increasing or decreasing update change. Higher weight increasing impact of corresponding event.
#' @param idlab name of column in `data` containing date. Doesn't affect estimation process. If specified, charts displays estimates changes in time instead of by observation `id`
#' @param beta The additional variance of performance. As beta increases, the performance is more uncertain and update change is smaller. By default `beta = 25/6`.
#' @param kappa controls `rd` shrinkage not to be greater than `rd*(1-kappa)`. `kappa=1` means that `rd` will not be decreased.
#' @param gamma can help to control how fast the variance `rd` is reduced after updating. Lower `gamma` slows decreasing of `rd`, which tends to reach zero to quickly. The default value is `gamma = rd/c`.
#' @param init_r initial values for `r` if not provided. Default = 25
#' @param init_rd initial values for `r` if not provided. Default = 25/3
#' @param pb logical, if TRUE progress bar will appear in console. Default = FALSE
#' @return 
#' A "ratings" object is returned
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
#' # Example from Glickman
#' data <- data.frame( name = c( "A", "B", "C", "D" ), 
#'                     rank = c( 3, 4, 1, 2 ))
#' bbt <- bbt_run( rank ~ name, data )
#' @export
bbt_run <- function(formula, data, 
                    r = numeric(0), rd = numeric(0),
                    init_r = 25, init_rd = 25 / 3,
                    sigma = NULL, weight = NULL, 
                    beta = 25 / 6, kappa = 0.5, gamma = 999) {
  is_formula_missing(formula)
  is_data_provided(data)
  is_lhs_valid(formula)
  is_rhs_valid(formula, "bbt_run")

  lhs  <- all.vars(update(formula, . ~ 0))
  rhs  <- all.vars(update(formula, 0 ~ .))
  
  rank_var <- lhs[1]
  name_var <- rhs[1]
  
  id_vec <- if (length(lhs) == 1) rep(1.0, nrow(data)) else data[[lhs[2]]]
  rank_vec <- data[[rank_var]]
  names_vec <- as.character(data[[name_var]])
  weight_vec <- if (is.null(weight)) rep(1.0, nrow(data)) else data[[weight]]
  sigma_vec <- if (is.null(sigma)) rep(1.0, nrow(data)) else data[[sigma]]
  
  # default rating
  unique_names <- unique(unlist(names_vec))
  
  if (missing(r)) {
    r <- matrix(init_r, nrow = length(unique_names), ncol = 1, dimnames = list(unique_names, NULL)) 
  } else if (!is.matrix(r)) {
    r <- as.matrix(r, ncol = 1)
  } else if (nrow(r) != unique_names) {
    stop(sprintf("All elements in r should have a name which match %s argument in formula",
                 name_var))
  }
  
  if (missing(rd)) {
    rd <- matrix(init_rd, nrow = length(unique_names), ncol = 1, 
                 dimnames = list(unique_names, NULL))
  } else if (!is.matrix(r)) {
    rd <- as.matrix(rd, ncol = 1)
  } else if (nrow(rd) != unique_names) {
    stop(sprintf("All elements in rd should have a name which match %s argument in formula",
                 name_var))
  }

  if (kappa == 0) kappa = 0.0001
  
  ratings <- bbt( 
    id = id_vec, 
    rank = rank_vec, 
    name = names_vec,
    r =  r, 
    rd = rd,
    sigma = sigma_vec,
    weight  = weight_vec,
    kappa = kappa,
    beta = beta,
    gamma = gamma,
    init_r = init_r,
    init_rd = init_rd
  )
   
  return(ratings)
}