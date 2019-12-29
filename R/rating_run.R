#' @importFrom data.table rbindlist
#' @importFrom stats setNames terms update
NULL


#' Apply rating algorithm
#'
#' Apply rating algorithm
#' @param formula formula which specifies the model. RHS Allows only player 
#' rating parameter and it should be specified in following manner:
#' 
#' `rank | id ~ team(name)`.
#' \itemize{
#'   \item {rank} player position in event.
#'   \item {id} event identifier in which pairwise comparison is assessed.
#'   \item {team(name)} name of the contestant. In this case \code{team(name)} 
#'     helps algorithm point name of the column where player names are stored.
#' }
#' Users can also specify formula in in different way:
#'  `rank | id ~ team(name|team)`. Which means that players are playing in teams, 
#'  and results are observed for teams not for players. For more see vignette.
#'  
#' @param data data.frame which contains columns specified in formula, and
#'  optionaly columns defined by `lambda`, `weight`.
#'  
#' @param r named vector of initial players ratings estimates. If not specified 
#' then `r` will be created automatically for parameters specified in `formula`
#' with initial value `init_r`.
#' 
#' @param rd rd named vector of initial rating deviation estimates. If not specified 
#' then `rd` will be created automatically for parameters specified in `formula`
#' with initial value `init_rd`.
#' 
#' @param lambda name of the column in `data` containing lambda values or one 
#' constant value (eg. `lambda = colname` or `lambda = 0.5`).
#' Lambda impact prior variance, and uncertainty of the matchup result. The 
#' higher lambda, the higher prior variance and more uncertain result of the 
#' matchup. Higher lambda flattens chances of winning. 
#' 
#' @param share name of the column in `data` containing player share in team 
#' efforts. It's used to first calculate combined rating of the team and
#' then redistribute ratings update back to players level. Warning - it should
#' be used only if formula is specified with players nested within teams (`team(player|team)`).
#' 
#' 
#' @param weight name of the column in `data` containing weights values or
#' one constant (eg. `weight = colname` or `weight = 0.5`). 
#' Weights increasing (weight > 1) or decreasing (weight < 1) update change. 
#' Higher weight increasing impact of event result on rating estimate.
#' 
#' @param kappa controls `rd` shrinkage not to be greater than `rd*(1 - kappa)`.
#'  `kappa=1` means that `rd` will not be decreased.
#'  
#' @param idlab name of column in `data` containing date. Doesn't affect
#' estimation process. If specified, charts displays estimates changes in time
#'  instead of by observation `id`.
#'  
#' @param init_r initial values for `r` if not provided. 
#' Default (`glicko = 1500`, `glicko2 = 1500`, `bbt = 25`, `dbl = 0`)
#' 
#' @param init_rd initial values for `r` if not provided. 
#' Default (`glicko = 350`, `glicko2 = 350`, `bbt = 25/3`, `dbl = 1`)
#' 
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
  kappa = numeric(0),
  tau = numeric(0)) {
  if (length(kappa) == 0) kappa <- 0.5
  if (length(tau) == 0) tau <- 0.5
  
  if (method == "glicko2") {
    check_single_argument(init_sigma, "init_sigma", min = 0.00000000001)
    check_single_argument(tau, "tau", min = 0.00000000001)
  }
  
  is_data_provided(data)
  check_single_argument(init_r, "init_r", min = 0)
  check_single_argument(init_rd, "init_rd", min = 0)
  is_formula_missing(formula)
  is_lhs_valid(formula, data)
  is_rhs_valid(formula, data, paste0(method, "_run"))
  
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
  if (grepl("team\\(", rhs_terms)) {
    player <- gsub("^team\\(([^ |]+)[ ]*\\|.*$", "\\1", rhs_terms)
    player_vec <- as.character(data[[player]])
    
    team <- gsub("^team\\(.+\\|[ ]*(.+)\\)$", "\\1", rhs_terms)
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
  
  lambda_vec <- initialize_vec(var = lambda, data = data, argname = "lambda", min = 0)
  share_vec  <- initialize_vec(var = share, data = data, argname = "share", min = 0, max = 1)
  weight_vec <- initialize_vec(var = weight, data = data, argname = "weight", min = 0)
  
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
      
      kappa = kappa,
      tau = 0.0
    )
  }
  return(g)
}


#' Glicko rating algorithm
#'
#' Glicko rating algorithm
#' 
#' @inheritParams rating_run
#' 
#' @return
#' 
#' A "rating" object is returned: \itemize{
#' 
#' \item \code{final_r} named vector containing players ratings.
#' 
#' \item \code{final_rd} named vector containing players ratings deviations.
#' 
#' \item \code{r} data.frame with evolution of the ratings and ratings deviations
#'  estimated at each event.
#'  
#' \item \code{pairs} pairwise combinations of players in analysed events with 
#' prior probability and result of a challange.
#' 
#' \item \code{class} of the object.
#' 
#' \item \code{method} type of algorithm used.
#' 
#' \item \code{settings} arguments specified in function call.
#' }
#' 
#' @examples
#' # the simplest example
#' data <- data.frame(
#'   name = c("A", "B", "C", "D"),
#'   rank = c(3, 4, 1, 2)
#' )
#' glicko <- glicko_run(
#'   data = data, 
#'   formula = rank ~ name
#'  )
#' 
#' # Example from Glickman
#' glicko <- glicko_run(
#'   data = data, 
#'   formula = rank ~ name,
#'    r = setNames(c(1500.0, 1400.0, 1550.0, 1700.0), c("A", "B", "C", "D")),
#'    rd = setNames(c(200.0, 30.0, 100.0, 300.0), c("A", "B", "C", "D"))
#'   )
#' @export
glicko_run <- function(data, formula,
                       r = numeric(0),
                       rd = numeric(0),
                       init_r = 1500,
                       init_rd = 350,
                       share = numeric(0),
                       weight = numeric(0),
                       lambda = numeric(0),
                       kappa = 0.5) {
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
    kappa = kappa
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
      kappa = kappa
    )
  )
  
  return(out)
}

#' Glicko2 rating algorithm
#'
#' Glicko2 rating algorithm
#' @inheritParams rating_run
#' @param sigma (only for glicko2) named vector of initial players ratings 
#' estimates. If not specified then `sigma` will be created automatically for 
#' parameters specified in `formula` with initial value `init_sigma`.
#' 
#' @param tau The system constant. Which constrains the change in volatility over
#'  time. Reasonable choices are between 0.3 and 1.2 (`default = 0.5`), though 
#'  the system should be tested to decide which value results in greatest 
#'  predictive accuracy. Smaller values of `tau` prevent the volatility measures 
#'  from changing by largeamounts, which in turn prevent enormous changes in 
#'  ratings based on very improbable results. If the application of Glicko-2 is 
#'  expected to involve extremely improbable collections of game outcomes, then 
#'  `tau` should be set to a small value, even as small as, say, `tau= 0`.
#' A "rating" object is returned
#' @return
#' 
#' A "rating" object is returned: \itemize{
#' 
#' \item \code{final_r} named vector containing players ratings.
#' 
#' \item \code{final_rd} named vector containing players ratings deviations.
#' 
#' \item \code{final_sigma} named vector containing players ratings volatiles.
#' 
#' \item \code{r} data.frame with evolution of the ratings and ratings deviations
#'  estimated at each event.
#'  
#' \item \code{pairs} pairwise combinations of players in analysed events with 
#' prior probability and result of a challange.
#' 
#' \item \code{class} of the object.
#' 
#' \item \code{method} type of algorithm used.
#' 
#' \item \code{settings} arguments specified in function call.
#' }
#' 
#' @examples
#' # the simplest example
#' data <- data.frame(
#'   name = c("A", "B", "C", "D"),
#'   rank = c(3, 4, 1, 2)
#' )
#' glicko2 <- glicko_run(
#'   data = data, 
#'   formula = rank ~ name
#'  )
#' 
#' # Example from Glickman
#' glicko2 <- glicko2_run(
#'   data = data, 
#'   formula = rank ~ name,
#'    r = setNames(c(1500.0, 1400.0, 1550.0, 1700.0), c("A", "B", "C", "D")),
#'    rd = setNames(c(200.0, 30.0, 100.0, 300.0), c("A", "B", "C", "D"))
#'   )
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
#' Bayesian Bradley-Terry
#' 
#' Bayesian Bradley-Terry
#' @inheritParams rating_run
#' 
#' @return
#' 
#' A "rating" object is returned: \itemize{
#' 
#' \item \code{final_r} named vector containing players ratings.
#' 
#' \item \code{final_rd} named vector containing players ratings deviations.
#' 
#' \item \code{r} data.frame with evolution of the ratings and ratings deviations
#'  estimated at each event.
#'  
#' \item \code{pairs} pairwise combinations of players in analysed events with 
#' prior probability and result of a challange.
#' 
#' \item \code{class} of the object.
#' 
#' \item \code{method} type of algorithm used.
#' 
#' \item \code{settings} arguments specified in function call.
#' }
#' 
#' @examples
#' # the simplest example
#' data <- data.frame(
#'   name = c("A", "B", "C", "D"),
#'   rank = c(3, 4, 1, 2)
#' )
#' bbt <- bbt_run(
#'   data = data, 
#'   formula = rank ~ name
#'  )
#' 
#' bbt <- bbt_run(
#'   data = data, 
#'   formula = rank ~ name,
#'    r = setNames(c(25, 23.3, 25.83, 28.33), c("A", "B", "C", "D")),
#'    rd = setNames(c(4.76, 0.71, 2.38, 7.14), c("A", "B", "C", "D"))
#'   )
#' @export
bbt_run <- function(formula,
                    data,
                    r = numeric(0),
                    rd = numeric(0),
                    init_r = 25,
                    init_rd = 25 / 3,
                    lambda = NULL,
                    weight = NULL,
                    share = NULL,
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
      kappa = kappa
    )
  )
  return(out)
}


#' Dynamic Bayesian Logit
#' 
#' Dynamic Bayesian Logit
#' 
#' @inheritParams rating_run
#' @param formula formula which specifies the model. Unlike other algorithms
#' in the packages (glicko_run, glicko2_run, bbt_run), this doesn't allow
#' players nested in teams with `team(player | team)` but instead let user
#' specify multiple parameters also in interaction with others.
#' 
#' @return
#' 
#' A "rating" object is returned: \itemize{
#' 
#' \item \code{final_r} named vector containing players ratings.
#' 
#' \item \code{final_rd} named vector containing players ratings deviations.
#' 
#' \item \code{r} data.frame with evolution of the ratings and ratings deviations
#'  estimated at each event.
#'  
#' \item \code{pairs} pairwise combinations of players in analysed events with 
#' prior probability and result of a challange.
#' 
#' \item \code{class} of the object.
#' 
#' \item \code{method} type of algorithm used.
#' 
#' \item \code{settings} arguments specified in function call.
#' }
#' 
#' @examples
#' # the simplest example
#' data <- data.frame(
#'   name = c("A", "B", "C", "D"),
#'   rank = c(3, 4, 1, 2),
#'   gate = c(1, 2, 3, 4),
#'   factor1 = c("a", "a", "b", "b"),
#'   factor2 = c("a", "b", "a", "b")
#' )
#' 
#' dbl <- dbl_run(
#'   data = data, 
#'   formula = rank ~ name
#'  )
#' 
#' dbl <- dbl_run(
#'   data = data, 
#'   formula = rank ~ name + gate * factor1)
#' @export
dbl_run <- function(formula,
                    data,
                    r = NULL,
                    rd = NULL,
                    lambda = NULL,
                    weight = NULL,
                    kappa = 0.95,
                    init_r = 0,
                    init_rd = 1) {
  is_formula_missing(formula)
  is_data_provided(data)
  is_lhs_valid(formula, data)
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
  
  lambda_vec <- initialize_vec(var = lambda, data = data, argname = "lambda", min = 0)
  weight_vec <- initialize_vec(var = weight, data = data, argname = "weight", min = 0)
  
  team_vec <- 1:nrow(data)
  
  if (is.null(kappa)) kappa <- 0.0001
  
  g <- dbl(
    unique_id = unique(id_vec),
    id = id_vec,
    rank_vec = rank_vec,
    team_vec = team_vec,
    MAP = as.matrix(MAP),
    X = as.matrix(X),
    cls = cls,
    R = r,
    RD = rd,
    lambda_vec = lambda_vec,
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
      weight = weight,
      kappa = kappa
    )
  )  
  
  
  return(out)
}