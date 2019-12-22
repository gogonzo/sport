#' @importFrom data.table rbindlist
#' @importFrom stats setNames terms update
#' @importFrom utils setTxtProgressBar txtProgressBar
NULL

#' Glicko rating algorithm
#' 
#' Glicko rating algorithm
#' Wrapper arround `glicko` update algorithm. Wrapper allows user to simplify calculation providing only data and initial parameters assumptions
#' @param formula formula specifying model. Glicko algorithm allows only player ranking parameter and should be specified by following manner: 
#' `rank | id ~ name`. Names in formula are unrestricted, but model structure remains the same:
#' \itemize{
#'  \item {rank} player position in event.
#'  \item {id} event identifier in which pairwise comparison is assessed.
#'  \item {name} of player.
#' }
#' @param data data.frame which contains columns specified in formula, and optionaly columns defined by `sigma`, `weight` or `date`.
#' @param r named vector of initial players ratings estimates. In there is no assumption, initial ratings are set be r=1500. Names of vector should correspond with `name` in formula. 
#' @param rd named vector of initial rating deviation estimates. In there is no assumption, initial ratings are set be r=300 Names of vector should correspond with `name` in formula.
#' @param sigma name of column in `data` containing rating volatility. Rating volitality is a value which multiplies prior `rd`. If `sigma > 0` then prior `rd` increases, making estimate of `r` more uncertain.
#' @param weight name of column in `data` containing weights. Weights increasing or decreasing update change. Higher weight increasing impact of corresponding event.
#' @param kappa controls `rd` shrinkage not to be greater than `rd*(1-kappa)`. `kappa=1` means that `rd` will not be decreased.
#' @param idlab name of column in `data` containing date. Doesn't affect estimation process. If specified, charts displays estimates changes in time instead of by observation `id`.
#' @param init_r initial values for `r` if not provided. Default = 1500
#' @param init_rd initial values for `r` if not provided. Default = 350
#' @param pb logical, if TRUE progress bar will appear in console. Default = FALSE
#' @return 
#' A "rating" object is returned: \itemize{
#' \item \code{final_r} named vector containing players ratings.
#' \item \code{final_rd} named vector containing players ratings deviations.
#' \item \code{r} data.frame with evolution of the ratings and ratings deviations estimated at each event.
#' \item \code{pairs} pairwise combinations of players in analysed events with prior probability and result of a challange.
#' \item \code{class} of the object.
#' \item \code{method} type of algorithm used.
#' \item \code{formula} modelled formula.
#' }
#' @examples
#' # Example from Glickman
#' data <- data.frame(name = c( "A", "B", "C", "D"), 
#'                    rank = c( 3, 4, 1, 2 ))
#' glicko <- glicko_run(data, rank ~ name)
#' @export
glicko_run <- function(data, formula, 
                       r = numeric(0),
                       rd = numeric(0), 
                       init_r = 1500, 
                       init_rd = 350, 
                       share = numeric(0),
                       weight = numeric(0),
                       lambda = numeric(0), 
                       kappa = 0.5, 
                       gamma = 1.0) {
  check_single_argument(init_r, "init_r", min = 0)
  check_single_argument(init_rd, "init_rd", min = 0)
  check_single_argument(kappa, "kappa", min = 0.00000000001)
  check_single_argument(gamma, "gamma", min = 0)
  is_formula_missing(formula)
  is_data_provided(data)
  is_lhs_valid(formula)
  is_rhs_valid(formula, "glicko_run")
  
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
  
  share_vec <- if (length(share) == 0) rep(1.0, nrow(data)) else data[[share]] # 1/n_it
  weight_vec <- if (length(weight) == 0) rep(1.0, nrow(data)) else data[[weight]]
  lambda_vec <- if (length(lambda) == 0) rep(1.0, nrow(data)) else data[[lambda]]
  
  # default rating
  unique_names <- unique(unlist(player_vec))
  unique_id    <- unique(id_vec)
  
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
  
  g <- glicko(
    unique_id,
    id = id_vec, 
    rank = rank_vec, 
    team = team_vec, 
    player = player_vec, 
    r = r, 
    rd = rd,
    share = share_vec, 
    lambda = lambda_vec, 
    weight = weight_vec, 
    gamma = gamma,
    kappa = kappa, 
    init_r = init_r, 
    init_rd = init_rd)
  
  ratings <- rbindlist(g$r)
  pairs <- rbindlist(g$p)
  
  out <- structure(
    list(final_r  = structure(g$final_r),
         final_rd = structure(g$final_rd),
         r        = ratings,
         pairs    = pairs
    ),
    class = "rating",
    method = "glicko",
    formula = formula,
    settings = list(
      init_r = init_r, 
      init_rd = init_rd, 
      weight = weight,
      sigma = sigma,
      idlab = idlab,
      kappa = kappa, 
      idlab = idlab
    )
  )
  
  return(out)
}

#' Glicko2 rating algorithm
#' 
#' Glicko2 rating algorithm
#' Wrapper arround `glicko2` update algorithm. Wrapper allows user to simplify calculation providing only data and initial parameters assumptions
#' @param formula formula specifying model. Glicko2 algorithm allows only player ranking parameter and should be specified by following manner: 
#' `rank | id ~ name`. Names in formula are unrestricted, but model structure remains the same:
#' \itemize{
#'  \item rank player position in event.
#'  \item id event identifier in which pairwise comparison is assessed.
#'  \item name of player.
#' }
#' @param data data.frame which contains columns specified in formula, and optionaly columns defined by `weight` or `date`.
#' @param r named vector of initial rating estimates. In there is no assumption, initial ratings is set to be r=1500. Names of vector should correspond with team_name label. 
#' @param rd named vector of initial rating deviation estimates. In there is no assumption, initial ratings is set to be r=300 Names of vector should correspond with team_name label.
#' @param sigma named vector of rating volatile. In there is no assumption, initial ratings should be sigma=0.5. Names of vector should correspond with team_name label.
#' @param tau The system constant. Which constrains the change in volatility over time. Reasonable choices are between 0.3 and 1.2 (`default = 0.5`), though the system should be tested to decide which value results in greatest predictive accuracy. Smaller values of `tau` prevent the volatility measures from changing by largeamounts, which in turn prevent enormous changes in ratings based on very improbable results. If the application of Glicko-2 is expected to involve extremely improbable collections of game outcomes, then `tau` should be set to a small value, even as small as, say, `tau= 0`.2.
#' @param weight name of column in `data` containing weights. Weights increasing or decreasing update change. Higher weight increasing impact of corresponding event.
#' @param kappa controls `rd` shrinkage not to be greater than `rd*(1-kappa)`. `kappa=1` means that `rd` will not be decreased.
#' @param idlab name of column in `data` containing date. Doesn't affect estimation process. If specified, charts displays estimates changes in time instead of by event `id`
#' @param init_r initial values for `r` if not provided. Default = 1500
#' @param init_rd initial values for `r` if not provided. Default = 350
#' @param pb logical, if TRUE progress bar will appear in console. Default = FALSE
#' @return 
#' A "rating" object is returned
#' \itemize{
#'   \item \code{final_r} named vector containing players ratings.
#'   \item \code{final_rd} named vector containing players ratings deviations.
#'   \item \code{final_sigma} named vector containing players ratings volatiles.
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
#' glicko2 <- glicko2_run( rank ~ name, data )
#' @export
glicko2_run <- function(formula, data, 
                        r = numeric(0), rd = numeric(0), sigma = numeric(0), 
                        weight = NULL, idlab = NULL,
                        init_r = 1500, init_rd = 350, init_sigma = 0.05,
                        tau = 0.5, kappa = 0.5) {
  
  is_formula_missing(formula)
  is_data_provided(data)
  is_lhs_valid(formula)
  is_rhs_valid(formula, "glicko_run")
  is_vector_named(r, "r")
  is_vector_named(rd, "rd")
  is_vector_named(sigma, "sigma")
  
  # formula
  lhs  <- all.vars(update(formula, .~0));
  rhs  <- all.vars(update(formula, 0~.));
  
  rank_var <- lhs[1]
  name_var <- rhs[1]
  
  rank_vec <- data[[rank_var]]
  names_vec <- as.character(data[[name_var]])
  id_vec <- if (length(lhs) == 1) rep(1.0, nrow(data)) else data[[lhs[2]]]  
  weight_vec <- if (is.null(weight)) rep(1.0, nrow(data)) else data[[weight]]
  
  # default rating
  unique_names <- unique(names_vec)
  if (length(r) == 0) {
    r <- setNames(rep(init_r, length(unique_names)), unique_names)
  } else if (length(r) != length(unique_names)) {
    stop(sprintf("All elements in r should have a name which match %s argument in formula",
                 name_var))
  }
  
  if (length(rd) == 0) {
    rd <- setNames(rep(init_rd, length(unique_names)), unique_names)
  } else if (length(rd) != length(unique_names)) {
    stop(sprintf("All elements in rd should have a name which match %s argument in formula",
                 name_var))
  }
  
  if (length(sigma) == 0) {
    sigma <- setNames(rep(init_sigma, length(unique_names)), unique_names)
  } else if (length(sigma) != length(unique_names)) {
    stop(sprintf("All elements in sigma should have a name which match %s argument in formula",
                 name_var))
  }
  
  g <- glicko2(
    id = id_vec, 
    rank = rank_vec, 
    name = names_vec, 
    weight = weight_vec,     
    r = r, 
    rd = rd,
    sigma = sigma, 
    kappa = kappa, 
    tau = tau,
    init_r = init_r, 
    init_rd = init_rd)
  
  ratings <- rbindlist(g$r)
  pairs   <- rbindlist(g$p)
  
  out <- structure(
    list(final_r  = structure(g$final_r),
         final_rd = structure(g$final_rd),
         r        = ratings,
         pairs    = pairs
    ),
    class = "rating",
    method = "glicko2",
    formula = formula,
    settings = list(
      init_r = init_r, 
      init_rd = init_rd, 
      weight = weight,
      sigma = sigma,
      idlab = idlab,
      kappa = kappa, 
      idlab = idlab
    )
  )
  
  return(out)
}