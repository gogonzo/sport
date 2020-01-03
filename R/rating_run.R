#' @importFrom data.table rbindlist
#' @importFrom stats setNames terms update
NULL


#' Apply rating algorithm
#'
#' Apply rating algorithm
#' @param formula formula which specifies the model. RHS Allows only player 
#' rating parameter and it should be specified in following manner:
#' 
#' \code{rank | id ~ player(name)}.
#' \itemize{
#'   \item {rank} player position in event.
#'   \item {id} event identifier in which pairwise comparison is assessed.
#'   \item {player(name)} name of the contestant. In this case \code{player(name)} 
#'     helps algorithm point name of the column where player names are stored.
#' }
#' Users can also specify formula in in different way:
#'  \code{rank | id ~ player(name|team)}. Which means that players are playing in teams, 
#'  and results are observed for teams not for players. For more see vignette.
#'
#' @param method one of \code{c("glicko", "glicko2", "bbt", "dbl")} 
#'  
#' @param data data.frame which contains columns specified in formula, and
#'  optional columns defined by \code{lambda}, \code{weight}.
#'  
#' @param r named vector of initial players ratings estimates. If not specified 
#' then \code{r} will be created automatically for parameters specified in \code{formula}
#' with initial value \code{init_r}.
#' 
#' @param rd rd named vector of initial rating deviation estimates. If not specified 
#' then \code{rd} will be created automatically for parameters specified in \code{formula}
#' with initial value \code{init_rd}.
#' 
#' @param sigma (only for glicko2) named vector of initial players ratings 
#' estimates. If not specified then \code{sigma} will be created automatically for 
#' parameters specified in \code{formula} with initial value \code{init_sigma}.
#' 
#' @param lambda name of the column in `data` containing lambda values or one 
#' constant value (eg. \code{lambda = colname} or \code{lambda = 0.5}).
#' Lambda impact prior variance, and uncertainty of the matchup result. The 
#' higher lambda, the higher prior variance and more uncertain result of the 
#' matchup. Higher lambda flattens chances of winning. 
#' 
#' @param share name of the column in `data` containing player share in team 
#' efforts. It's used to first calculate combined rating of the team and
#' then redistribute ratings update back to players level. Warning - it should
#' be used only if formula is specified with players nested within teams (`player(player|team)`).
#' 
#' 
#' @param weight name of the column in `data` containing weights values or
#' one constant (eg. \code{weight = colname} or \code{weight = 0.5}). 
#' Weights increasing (weight > 1) or decreasing (weight < 1) update change. 
#' Higher weight increasing impact of event result on rating estimate.
#' 
#' @param kappa controls \code{rd} shrinkage not to be greater than \code{rd*(1 - kappa)}.
#'  `kappa=1` means that  \code{rd} will not be decreased.
#' @param tau The system constant. Which constrains the change in volatility over
#'  time. Reasonable choices are between 0.3 and 1.2 (\code{default = 0.5}), though 
#'  the system should be tested to decide which value results in greatest 
#'  predictive accuracy. Smaller values of \code{tau} prevent the volatility measures 
#'  from changing by large amounts, which in turn prevent enormous changes in 
#'  ratings based on very improbable results. If the application of Glicko-2 is 
#'  expected to involve extremely improbable collections of game outcomes, then 
#'  `tau` should be set to a small value, even as small as, say, \code{tau= 0}.
#'  
#' @param init_r initial values for \code{r} if not provided. 
#' Default (\code{glicko = 1500}, \code{glicko2 = 1500}, \code{bbt = 25}, 
#' \code{dbl = 0})
#' 
#' @param init_rd initial values for \code{rd} if not provided. 
#' Default (\code{glicko = 350}, \code{glicko2 = 350}, \code{bbt = 25/3}, \code{dbl = 1})
#' 
#' @param init_sigma initial values for \code{sigma} if not provided. 
#' Default = 0.5
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
  lambda = numeric(0),
  share = numeric(0),
  weight = numeric(0),
  kappa = numeric(0),
  tau = numeric(0)) {
  if (length(kappa) == 0) kappa <- 0.5
  if (length(tau) == 0) tau <- 0.5
  
  if (method == "glicko2") {
    check_single_argument(init_sigma, "init_sigma", min = 0.00000000001)
    check_single_argument(tau, "tau", min = 0.00000000001)
  }
  
  is_data_provided(data)
  is_formula_missing(formula)
  is_lhs_valid(formula, data)
  is_rhs_valid(formula, data, only_team_term = TRUE, single = FALSE)
  check_single_argument(init_r, "init_r", min = 0)
  check_single_argument(init_rd, "init_rd", min = 0)
 
  rank <- get_rank_name(formula)
  rank_vec <- as.integer(data[[rank]])
 
  id <- get_id_name(formula)
  id_vec <- if (length(id) == 0) rep(1L, nrow(data)) else  as.integer(data[[id]])
  
  team <- get_team_name(formula)
  player <- get_player_name(formula)
  are_variables_in_dataset(c(rank, id, team, player), data)
  
  team_vec <- as.character(data[[team]])
  player_vec <- if (length(player) == 0) team_vec else as.character(data[[player]])
  
  
  check_integer_argument(id_vec, id)
  check_integer_argument(rank_vec, rank)
  check_string_argument(player_vec, player)
  check_string_argument(team_vec, team)
  
  lambda_vec <- initialize_vec(var = lambda, data = data, argname = "lambda", min = 0)
  share_vec  <- initialize_vec(var = share, data = data, argname = "share", min = 0, max = 1)
  weight_vec <- initialize_vec(var = weight, data = data, argname = "weight", min = 0)
  
  # default rating
  unique_names <- unique(c(unlist(player_vec), names(r), names(rd), names(sigma)))
  unique_id <- unique(id_vec)
  
  r <- init_check_r(r, init_r, unique_names, player)
  rd <- init_check_rd(rd, init_rd, unique_names, player)
  sigma <- init_check_sigma(sigma, init_sigma, unique_names, player, method)
  check_equal_names(r, rd)
   
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
  
  ratings <- data.table::rbindlist(g$r)
  pairs <- data.table::rbindlist(g$p)
  
  rhs_terms <- extract_team_terms(formula)
  if (length(rhs_terms) == 1) {
    ratings <- ratings[,"player" := NULL]
    names(ratings)[names(ratings) == "team"] <- rhs_terms[1]
    names(pairs)[names(pairs) == "team"] <- rhs_terms[1]
    
  } else {
    names(ratings)[names(ratings) == "player"] <- rhs_terms[1]
    names(ratings)[names(ratings) == "team"] <- rhs_terms[2]
    names(pairs)[names(pairs) == "team"] <- rhs_terms[2]
  }
  
  g$r <- if (method == "glicko2") ratings else ratings[,"sigma" := NULL]
  g$p <- pairs
  
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
#' prior probability and result of a challenge.
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
#'   id = c(1, 1, 1, 1),
#'   team = c("A", "A", "B", "B"),
#'   player = c("a", "b", "c", "d"),
#'   rank_team = c(1, 1, 2, 2),
#'   rank_player = c(3, 4, 1, 2)
#' )
#' 
#' # Example from Glickman
#' glicko <- glicko_run(
#'   data = data, 
#'   formula = rank_player | id ~ player(player),
#'    r = setNames(c(1500.0, 1400.0, 1550.0, 1700.0), c("a", "b", "c", "d")),
#'    rd = setNames(c(200.0, 30.0, 100.0, 300.0), c("a", "b", "c", "d"))
#'   )
#'   
#' # nested matchup
#' glicko <- glicko_run(
#'   data = data, 
#'   formula = rank_team | id ~ player(player | team)
#'  )
#' @export
glicko_run <- function(data, formula,
                       r = numeric(0),
                       rd = numeric(0),
                       init_r = 1500,
                       init_rd = 350,
                       lambda = numeric(0),
                       share = numeric(0),
                       weight = numeric(0),
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

  out <- structure(
    list(
      final_r = g$final_r,
      final_rd = g$final_rd,
      r = g$r,
      pairs = g$p
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
#'
#' @return
#' 
#' A "rating" object is returned: \itemize{
#' 
#' \item \code{final_r} named vector containing players ratings.
#' 
#' \item \code{final_rd} named vector containing players ratings deviations.
#' 
#' \item \code{final_sigma} named vector containing players ratings volatile.
#' 
#' \item \code{r} data.frame with evolution of the ratings and ratings deviations
#'  estimated at each event.
#'  
#' \item \code{pairs} pairwise combinations of players in analysed events with 
#' prior probability and result of a challenge.
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
#'   id = c(1, 1, 1, 1),
#'   team = c("A", "A", "B", "B"),
#'   player = c("a", "b", "c", "d"),
#'   rank_team = c(1, 1, 2, 2),
#'   rank_player = c(3, 4, 1, 2)
#' )
#' 
#' # Example from Glickman
#' glicko2 <- glicko2_run(
#'   data = data, 
#'   formula = rank_player | id ~ player(player),
#'    r = setNames(c(1500.0, 1400.0, 1550.0, 1700.0), c("a", "b", "c", "d")),
#'    rd = setNames(c(200.0, 30.0, 100.0, 300.0), c("a", "b", "c", "d"))
#'   )
#'   
#' # nested matchup
#' glicko2 <- glicko2_run(
#'   data = data, 
#'   formula = rank_team | id ~ player(player | team)
#'  )
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
    lambda = lambda,
    share = share,
    weight = weight,
    kappa = kappa,
    tau = tau
  )
  
  out <- structure(
    list(
      final_r = g$final_r,
      final_rd = g$final_rd,
      final_sigma = g$final_sigma,
      r = g$r,
      pairs = g$p
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
#' prior probability and result of a challenge.
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
#'   id = c(1, 1, 1, 1),
#'   team = c("A", "A", "B", "B"),
#'   player = c("a", "b", "c", "d"),
#'   rank_team = c(1, 1, 2, 2),
#'   rank_player = c(3, 4, 1, 2)
#' )
#' 
#' bbt <- bbt_run(
#'   data = data, 
#'   formula = rank_player | id ~ player(player),
#'    r = setNames(c(25, 23.3, 25.83, 28.33), c("a", "b", "c", "d")),
#'    rd = setNames(c(4.76, 0.71, 2.38, 7.14), c("a", "b", "c", "d"))
#'   )
#'   
#' # nested matchup
#' bbt <- bbt_run(
#'   data = data, 
#'   formula = rank_team | id ~ player(player | team)
#'  )
#' 
#' @export
bbt_run <- function(formula,
                    data,
                    r = numeric(0),
                    rd = numeric(0),
                    init_r = 25,
                    init_rd = 25 / 3,
                    lambda = NULL,
                    share = NULL,
                    weight = NULL,
                    kappa = 0.5) {
  g <- rating_run(
    method = "bbt",
    data = data,
    formula = formula,
    r = r,
    rd = rd,
    init_r = init_r,
    init_rd = init_rd,
    lambda = lambda,
    share = share,
    weight = weight,
    kappa = kappa
  )
  
  out <- structure(
    list(
      final_r = g$final_r,
      final_rd = g$final_rd,
      r = g$r,
      pairs = g$p
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
#' in the packages (glicko_run, glicko2_run, bbt_run), this method doesn't allow
#' players nested in teams with `player(player | team)` and user should matchup
#' in formula using `player(player)`. DBL allows user specify multiple parameters 
#' also in interaction with others.
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
#' prior probability and result of a challenge.
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
#' 
#' data <- data.frame(
#'   id = c(1, 1, 1, 1),
#'   name = c("A", "B", "C", "D"),
#'   rank = c(3, 4, 1, 2),
#'   gate = c(1, 2, 3, 4),
#'   factor1 = c("a", "a", "b", "b"),
#'   factor2 = c("a", "b", "a", "b")
#' )
#' 
#' dbl <- dbl_run(
#'   data = data, 
#'   formula = rank | id ~ player(name)
#'  )
#' 
#' dbl <- dbl_run(
#'   data = data, 
#'   formula = rank | id ~ player(name) + gate * factor1)
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
  is_rhs_valid(formula, data, only_team_term = FALSE, single = TRUE)
  
  rank <- get_rank_name(formula)
  rank_vec <- as.integer(data[[rank]])
  
  id <- get_id_name(formula)
  id_vec <- if (length(id) == 0) rep(1L, nrow(data)) else  as.integer(data[[id]])
  
  
  terms <- get_terms(data, formula)
  MAP   <- get_terms_map(data, terms)
  X     <- get_terms_mat(data, terms)
  cls   <- get_terms_cls(data, terms)
  unique_params <- unname(unlist(apply(MAP, 2, unique)))
  
  team <- get_team_name(formula)
  team_vec <- as.character(data[[team]])
  
  r <- init_check_r(r, init_r, unique_params, "term columns")
  rd <- init_check_rd(rd, init_rd, unique_params, "term columns")
  check_equal_names(r, rd)
  
  lambda_vec <- initialize_vec(var = lambda, data = data, argname = "lambda", min = 0)
  weight_vec <- initialize_vec(var = weight, data = data, argname = "weight", min = 0)
  
  if (is.null(kappa)) kappa <- 0.0001
  
  
  
  g <- dbl(
    unique_id = unique(id_vec),
    id_vec = id_vec,
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
  
  rhs_terms <- extract_team_terms(formula)
  names(ratings)[names(ratings) == "team"] <- rhs_terms[1]
  names(pairs)[names(pairs) == "team"] <- rhs_terms[1]

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
