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
#' @param gamma can help to control how fast the variance `rd` is reduced after updating. Lower `gamma` slows decreasing of `rd`, which tends to reach zero to quickly. The default value is `gamma = rd/c`. In simple words `gamma` multiplies prediction error. 
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
#' data <- data.frame(name = c("A", "B", "C", "D"), 
#'                    rank = c(3, 4, 1, 2),
#'                    stringsAsFactors = FALSE)
#' bbt <- bbt_run( rank ~ name, data)
#' @export
bbt_run <- function(formula, data, 
                    r = numeric(0), rd = numeric(0),
                    init_r = 25, init_rd = 25 / 3,
                    lambda = NULL, weight = NULL, share = NULL,
                    beta = 25 / 6, kappa = 0.5) {
  check_single_argument(init_r, "init_r", min = 0)
  check_single_argument(init_rd, "init_rd", min = 0)
  check_single_argument(kappa, "kappa", min = 0.00000000001)
  check_single_argument(gamma, "gamma", min = 0)
  check_single_argument(beta, "beta", min = 0)
  is_formula_missing(formula)
  is_data_provided(data)
  is_lhs_valid(formula)
  is_rhs_valid(formula, "bbt_run")

  lhs  <- all.vars(update(formula, . ~ 0))
  rank <- lhs[1]
  rank_vec <- as.integer(data[[rank]])
  
  if (length(lhs) == 1) {
    id <- "id"
    id_vec <- rep(1L, nrow(data)) 
  } else {
    id <- lhs[2]
    id_vec <- data[[lhs[2]]]
  } 
  
  rhs_terms <- attr(terms(update(formula, 0 ~ .)), "term.labels")
  if (grepl("nest\\(", rhs_terms)) {
    player <- gsub("^nest\\(([^ |]+)[ ]*\\|.*$", "\\1", rhs_terms)
    team <- gsub("^nest\\(.+\\|[ ]*(.+)\\)$", "\\1", rhs_terms)    
    team_vec <- as.character(data[[team]])
    player_vec <- as.character(data[[player]])
  } else {
    player <- rhs_terms[1]
    team <- "team"
    player_vec <- as.character(data[[player]])
    team_vec <- player_vec
  }

  lambda_vec <- if (is.null(lambda)) rep(1.0, nrow(data)) else data[[lambda]]
  weight_vec <- if (is.null(weight)) rep(1.0, nrow(data)) else data[[weight]]
  share_vec <- if (is.null(share)) rep(1.0, nrow(data)) else data[[share]] # 1/n_it
  
  # default rating
  unique_names <- unique(unlist(player_vec))
  unique_id <- unique(id_vec)
  
  r  <- init_check_r(r, init_r, unique_names, player)
  rd <- init_check_r(rd, init_rd, unique_names, player)

  # check if lambda > 0
  check_integer_argument(id_vec, id)
  check_integer_argument(rank_vec, rank)
  check_string_argument(player_vec, player)
  check_string_argument(team_vec, team)
  check_numeric_argument(lambda_vec, lambda)
  check_numeric_argument(weight_vec, weight)
  check_numeric_argument(share_vec, share, min = 0, max = 1)
  
  
  g <- bbt( 
    unique_id = unique_id,
    id = id_vec, 
    rank = rank_vec, 
    team = team_vec,
    player = player_vec,
    r_val =  r, 
    rd_val = rd,
    lambda = lambda_vec,
    weight  = weight_vec,
    share  = share_vec,
    kappa = kappa,
    beta = beta,
    init_r = init_r,
    init_rd = init_rd
  )
   
  ratings <- rbindlist(g$r)
  pairs   <- rbindlist(g$p)
  
  out <- structure(
    list(final_r  = structure(g$final_r),
         final_rd = structure(g$final_rd),
         r        = ratings,
         pairs    = pairs
    ),
    class = "rating",
    method = "bbt",
    formula = formula,
    settings = list(
      init_r = init_r, 
      init_rd = init_rd, 
      weight = weight,
      sigma = sigma,
      kappa = kappa
    )
  )
  
  return(out)
}