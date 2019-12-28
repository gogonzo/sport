#' @importFrom data.table rbindlist
#' @importFrom stats setNames terms update
#' @importFrom utils setTxtProgressBar txtProgressBar
NULL


#' Apply rating algorithm
#'
#' Apply rating algorithm
#' @param formula formula specifying model. Allows only player ranking parameter and should be specified by following manner:
#' 
#' `rank | id ~ player(name)`. Names in formula are unrestricted, but model structure remains the same:
#' \itemize{
#'  \item {rank} player position in event.
#'  \item {id} event identifier in which pairwise comparison is assessed.
#'  \item {player(name)} name of the player. In this case \code{player(name)} 
#'  helps algorithm point name of the column where player names are stored.
#' }
#' Users can also specify formula in in different way:
#'  `rank | id ~ player(name|team)`. Which means that players are playing in teams, and results are obtained 
#'  for teams.
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
rating_run <- function(
  method,
  data,
  formula,
  r = numeric(0),
  rd = numeric(0),
  sigma = numeric(0),
  init_r = numeric(0),
  init_rd = numeric(0),
  init_sigma = numeric(0),
  share = numeric(0),
  weight = numeric(0),
  lambda = numeric(0),
  beta = numeric(0),
  gamma = numeric(0),
  kappa = numeric(0),
  tau = numeric(0)) {
  if (length(beta) == 0) beta <- 25 / 6
  if (length(gamma) == 0) gamma <- 1.0
  if (length(kappa) == 0) kappa <- 0.5
  if (length(tau) == 0) tau <- 0.5
  
  if (method == "glicko") {
    check_single_argument(gamma, "gamma", min = 0.00000000001)
  } else if (method == "bbt") {
    check_single_argument(beta, "beta", min = 0.00000000001)
  } else if (method == "glicko2") {
    check_single_argument(init_sigma, "init_sigma", min = 0.00000000001)
    check_single_argument(tau, "tau", min = 0.00000000001)
  }
  
  is_data_provided(data)
  check_single_argument(init_r, "init_r", min = 0)
  check_single_argument(init_rd, "init_rd", min = 0)
  is_formula_missing(formula)
  is_lhs_valid(formula)
  is_rhs_valid(formula, paste0(method, "_run"))
  
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
  
  
  rhs_terms <- attr(terms(update(formula, 0 ~ .)), "term.labels")
  if (grepl("player\\(", rhs_terms)) {
    player <- gsub("^player\\(([^ |]+)[ ]*\\|.*$", "\\1", rhs_terms)
    player_vec <- as.character(data[[player]])
    
    team <- gsub("^player\\(.+\\|[ ]*(.+)\\)$", "\\1", rhs_terms)
    team_vec <- as.character(data[[team]])
  } else {
    player <- rhs_terms[1]
    player_vec <- as.character(data[[player]])
    
    team <- "team"
    team_vec <- player_vec
  }
  
  check_integer_argument(id_vec, id)
  check_integer_argument(rank_vec, rank)
  check_string_argument(player_vec, player)
  check_string_argument(team_vec, team)
  
  if (length(share) == 0) {
    share_vec <- rep(1.0, nrow(data))
  } else {
    share_vec <- data[[share]] # 1/n_it
    check_numeric_argument(share_vec, share, min = 0, max = 1)
  }
  
  if (length(lambda) == 0) {
    lambda_vec <- rep(1.0, nrow(data))
  } else {
    lambda_vec <- data[[lambda]] # 1/n_it
    check_numeric_argument(lambda_vec, lambda, min = 0, max = 1)
  }
  
  if (length(weight) == 0) {
    weight_vec <- rep(1.0, nrow(data))
  } else {
    weight_vec <- data[[weight]] # 1/n_it
    check_numeric_argument(weight_vec, weight, min = 0, max = 1)
  }
  
  # default rating
  unique_names <- unique(unlist(player_vec))
  unique_id <- unique(id_vec)
  
  r <- init_check_r(r, init_r, unique_names, player)
  rd <- init_check_rd(rd, init_rd, unique_names, player)
  sigma <- init_check_sigma(sigma, init_sigma, unique_names, player, method)
  
  g <- if (method == "glicko") {
    glicko(
      unique_id = unique_id,
      id = id_vec,
      rank = rank_vec,
      team = team_vec,
      player = player_vec,
      r = r,
      rd = rd,
      sigma = numeric(0),
      init_r = init_r,
      init_rd = init_rd,
      init_sigma = 0.0,
      lambda = lambda_vec,
      share = share_vec,
      weight = weight_vec,
      beta = 0.0,
      gamma = gamma,
      kappa = kappa,
      tau = 0.0
    )
  } else if (method == "glicko2") {
    glicko2(
      unique_id,
      id = id_vec,
      rank = rank_vec,
      team = team_vec,
      player = player_vec,
      
      r = r,
      rd = rd,
      sigma = sigma,
      
      init_r = init_r,
      init_rd = init_rd,
      init_sigma = init_sigma,
      
      lambda = lambda_vec,
      share = share_vec,
      weight = weight_vec,
      
      beta = 0.0,
      gamma = 0.0,
      kappa = kappa,
      tau = tau
    )
  } else if (method == "bbt") {
    bbt(
      unique_id,
      id = id_vec,
      rank = rank_vec,
      team = team_vec,
      player = player_vec,
      
      r = r,
      rd = rd,
      sigma = numeric(0),
      
      init_r = init_r,
      init_rd = init_rd,
      init_sigma = 0.0,
      
      lambda = lambda_vec,
      share = share_vec,
      weight = weight_vec,
      
      beta = beta,
      gamma = 0.0,
      kappa = kappa,
      tau = 0.0
    )
  }
  return(g)
}


#' Glicko rating algorithm
#'
#' Glicko rating algorithm
#' Wrapper arround `glicko` update algorithm. Wrapper allows user to simplify calculation providing only data and initial parameters assumptions
#' @inheritParams rating_run
#' @examples
#' # Example from Glickman
#' data <- data.frame(
#'   name = c("A", "B", "C", "D"),
#'   rank = c(3, 4, 1, 2)
#' )
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
  g <- rating_run(
    method = "glicko",
    data = data,
    formula = formula,
    r = r,
    rd = rd,
    init_r = init_r,
    init_rd = init_rd,
    share = share,
    weight = weight,
    lambda = lambda,
    kappa = kappa,
    gamma = gamma
  )
  
  ratings <- data.table::rbindlist(g$r)[, -6]
  pairs <- data.table::rbindlist(g$p)
  
  out <- structure(
    list(
      final_r = g$final_r,
      final_rd = g$final_rd,
      r = ratings,
      pairs = pairs
    ),
    class = "rating",
    method = "glicko",
    formula = formula,
    settings = list(
      init_r = init_r,
      init_rd = init_rd,
      lambda = lambda,
      share = share,
      weight = weight,
      gamma = gamma,
      kappa = kappa
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
#' data <- data.frame(
#'   name = c("A", "B", "C", "D"),
#'   rank = c(3, 4, 1, 2)
#' )
#' glicko2 <- glicko2_run(rank ~ name, data)
#' @export
glicko2_run <- function(formula,
                        data,
                        r = numeric(0),
                        rd = numeric(0),
                        sigma = numeric(0),
                        lambda = NULL,
                        share = NULL,
                        weight = NULL,
                        init_r = 1500,
                        init_rd = 350,
                        init_sigma = 0.05,
                        kappa = 0.5,
                        tau = 0.5) {
  g <- rating_run(
    method = "glicko2",
    data = data,
    formula = formula,
    r = r,
    rd = rd,
    sigma = sigma,
    init_r = init_r,
    init_rd = init_rd,
    init_sigma = init_sigma,
    share = share,
    weight = weight,
    lambda = lambda,
    kappa = kappa,
    tau = tau
  )
  
  ratings <- data.table::rbindlist(g$r)
  pairs <- data.table::rbindlist(g$p)
  
  out <- structure(
    list(
      final_r = g$final_r,
      final_rd = g$final_rd,
      final_sigma = g$final_sigma,
      r = ratings,
      pairs = pairs
    ),
    class = "rating",
    method = "glicko2",
    formula = formula,
    settings = list(
      init_r = init_r,
      init_rd = init_rd,
      init_sigma = init_sigma,
      lambda = lambda,
      share = share,
      weight = weight,
      kappa = kappa,
      tau = tau
    )
  )
  
  return(out)
}

bbt_run <- function(formula,
                    data,
                    r = numeric(0),
                    rd = numeric(0),
                    init_r = 25,
                    init_rd = 25 / 3,
                    lambda = NULL,
                    weight = NULL,
                    share = NULL,
                    beta = 25 / 6,
                    kappa = 0.5) {
  g <- rating_run(
    method = "bbt",
    data = data,
    formula = formula,
    r = r,
    rd = rd,
    init_r = init_r,
    init_rd = init_rd,
    share = share,
    weight = weight,
    lambda = lambda,
    beta = beta,
    kappa = kappa
  )
  
  ratings <- rbindlist(g$r)[, -6]
  pairs <- rbindlist(g$p)
  
  out <- structure(
    list(
      final_r = g$final_r,
      final_rd = g$final_rd,
      r = ratings,
      pairs = pairs
    ),
    class = "rating",
    method = "bbt",
    formula = formula,
    settings = list(
      init_r = init_r,
      init_rd = init_rd,
      lambda = lambda,
      share = share,
      weight = weight,
      beta = beta,
      kappa = kappa
    )
  )
  return(out)
}


dbl_run <- function(formula,
                    data,
                    r = NULL,
                    rd = NULL,
                    beta = NULL,
                    lambda = NULL,
                    share = NULL,
                    weight = NULL,
                    kappa = 0.95,
                    init_r = 0,
                    init_rd = 1) {
  is_formula_missing(formula)
  is_data_provided(data)
  is_lhs_valid(formula)
  is_interactions_valid(formula)
  
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
  MAP <- get_terms_map(data, terms)
  X   <- get_terms_mat(data, terms)
  cls <- get_terms_cls(data, terms)
  unique_params <- unname(unlist(apply(MAP, 2, unique)))
  
  if (is.null(r)) {
    r <- setNames(rep(init_r, length(unique_params)), unique_params)
  }
  if (is.null(rd)) {
    rd <- setNames(rep(init_rd, length(unique_params)), unique_params)
  }
  
  lambda_vec <- if (is.null(lambda)) rep(1, nrow(data)) else data[["lambda"]]
  share_vec  <- if (is.null(share))  rep(1, nrow(data)) else data[["share"]]
  weight_vec <- if (is.null(weight)) rep(1, nrow(data)) else data[["weight"]]
  
  player_vec <- 1:nrow(data)
  
  if (is.null(kappa)) kappa <- 0.0001
  
  g <- dbl(
    unique_id = unique(id_vec),
    id = id_vec,
    rank_vec = rank_vec,
    player_vec = player_vec,
    MAP = as.matrix(MAP),
    X = as.matrix(X),
    cls = cls,
    R = r,
    RD = rd,
    lambda_vec = lambda_vec,
    share_vec = share_vec,
    weight_vec = weight_vec,
    kappa = kappa
  )
  
  ratings <- data.table::rbindlist(g$r)
  pairs <- data.table::rbindlist(g$p)
  
  out <- structure(
    list(
      final_r = g$final_r,
      final_rd = g$final_rd,
      r = ratings,
      pairs = pairs
    ),
    class = "rating",
    method = "dbl",
    formula = formula,
    settings = list(
      init_r = init_r,
      init_rd = init_rd,
      lambda = lambda,
      share = share,
      weight = weight,
      kappa = kappa
    )
  )  
  
  
  return(out)
}
