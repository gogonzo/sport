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
#' data <- data.frame( name = c("A", "B", "C", "D"), 
#'                     rank = c( 3, 4, 1, 2 ))
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
  
  all_params <- allLevelsList(formula, data)
  lhs  <- all.vars(update(formula, .~0))
  rhs  <- all.vars(update(formula, 0~.))
  id   <- ifelse(length(lhs) == 1 , "id", lhs[2])
  data[[rhs[1]]] <- as.character(data[[rhs[1]]])
  
  if (length(lhs) == 1) data$id <- 1
  
  if (missing(r)) 
    r  <- setNames(rep(init_r, length(all_params)), all_params)
  if (missing(rd)) 
    rd <- setNames(rep(init_rd, length(all_params)), all_params)
  if (missing(weight)) {
    data$weight <- 1.0
    weight <- "weight" 
  } 
  if (missing(beta)) {
    data$beta <- 1.0
    beta <- "beta" 
  } 
  if (kappa == 0) 
    kappa <- 0.0001

  if (is.data.frame(data)) {
    data_list <- split(data[c(rhs, beta, weight)],
                       data[[id]])
  } 

  rank_list  <- split(as.integer(data[[lhs[1]]]), data[[id]])
  rider_list <- split(data[[rhs[1]]], data[[id]])

  models <- list()
  for (i in names(data_list)) {
    terms <- createTermMatrix(formula, data_list[[i]][rhs]) 
    model <- dbl(
      name    = rider_list[[i]],
      rank    = rank_list[[i]],
      X       = as.matrix(terms),
      R       = r[colnames(terms)], 
      RD      = rd[colnames(terms)],
      beta    = data_list[[i]][[beta]],
      weight  = data_list[[i]][[weight]],
      kappa   = kappa
    )
    
    if (any(!is.finite(model$rd) | !is.finite(model$r) | model$rd < 0))
      stop(paste0("Parameters error after evaluating ", id,"=",i),call. = F)

    r[names(model$r)]   <- model$r
    rd[names(model$rd)] <- model$rd
    
    models[[ i ]] <- model
  }
  
  model_r <- suppressWarnings( 
    data.table::rbindlist( 
      lapply(models, function(x) x[["r_df"]]), 
      use.names = TRUE, 
      idcol = "id" 
    ) 
  )
  model_P <- suppressWarnings(
    data.table::rbindlist( 
      lapply(models, function(x) x[["pairs"]]), 
      use.names = TRUE, 
      idcol = "id"
    ) 
  )
  
  # Output, class and attributes ----
  class(model_r[[id]]) <- class(model_P[[id]]) <- class(data[[id]]) 
  
  # add winning probability to data    
  p <- model_P[,.(p_win = prod(P)), by = c("id","name")][,
                    p_win := p_win / sum(p_win), by = "id"]
  model_r <- merge(model_r, 
                   p, 
                   all.x = TRUE, 
                   by = c("id","name"),
                   sort = FALSE)
  
  out <- structure(
    list(final_r  = r,
         final_rd = rd,
         r        = model_r,
         pairs    = model_P
    ),
    class = "rating",
    method = "dbl",
    formula = formula,
    settings = list(init_r = init_r,
                    init_rd = init_rd, 
                    beta = beta,
                    weight = weight,
                    kappa = kappa)
  )
  
  return(out)
}