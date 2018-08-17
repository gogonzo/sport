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
#' @param idlab name of column in `data` containing date. Doesn't affect estimation process. If specified, charts displays estimates changes in time in
#' @param tau parameter controlling `rd` to avoid quick decreasing to zero. Is the proportion of `rd` which is maximum change size.
#' @param init_r initial values for `r` if not provided. Default `r=0`
#' @param init_rd initial values for `rd` if not provided. Default `rd=1`
#' @return 
#' A "sport" object is returned
#' \itemize{
#'   \item \code{final_r} named vector containing players ratings.
#'   \item \code{final_rd} named vector containing players ratings deviations.
#'   \item \code{r} data.frame with evolution of the ratings and ratings deviations estimated at each event.
#'   \item \code{pairs} pairwise combinations of players in analysed events with prior probability and result of a challange.
#'   \item \code{class} of the object
#'   \item \code{method} type of algorithm used
#'   \item \code{formula} modelled formula
#' }
#' @export
dbl_run <- function(formula, data, r, rd, beta, weight, idlab, tau=0.05, init_r=0, init_rd=1){
  is_formula_missing(formula)
  is_data_provided(formula)
  is_lhs_valid(formula)
  
  all_params <- allLevelsList(formula, data)
  lhs  <- all.vars(update(formula, .~0))
  rhs  <- all.vars(update(formula, 0~.))
  y    <- lhs[1]
  id <- ifelse( length(lhs)==1 , "id", lhs[2])
  if( length(lhs) == 1) data$id <- 1
  
  if(missing(r)) 
    r  <- setNames(rep(init_r, length(all_params)), all_params )
  if(missing(rd)) 
    rd <- setNames(rep(init_rd, length(all_params)), all_params )
  if(missing(idlab)) 
    idlab <- id
  if( missing(weight) ){
    data$weight <- 1; weight      <- "weight" } 
  if( missing(beta) ){
    data$beta <- 1; beta      <- "beta" } 
  
  if(any(class(data)=="data.frame")) 
    data_list <- split( data[ c(rhs, beta, weight,idlab) ], data[[ lhs[2] ]] )   
  unique_id  <- unique(data[[ lhs[2] ]]) 
  rank_list  <- split( data[[ lhs[1] ]] , data[[ lhs[2] ]])
  rider_list <- split( data[[ rhs[1] ]] , data[[ lhs[2] ]])
  
  j <- 0
  n <- length(data_list)
  pb <- txtProgressBar(min=0, max=n, width=20, initial=0, style=3)
  models <- list()
  for(i in names(data_list)){
    j <- j + 1
    terms <- createTermMatrix( formula, data_list[[ i ]][ rhs ] ) 
    model <- dbl(
      rider_list[[ i ]],
      rank    = rank_list[[ i ]],
      X       = as.matrix(terms),
      R       = r[ colnames(terms) ], 
      RD      = rd[ colnames(terms) ],
      beta     = data_list[[ i ]][[ beta ]],
      weight  = data_list[[ i ]][[ weight ]],
      identifier = as.character( data_list[[ i ]][[ idlab ]] ),
      tau = tau
    )
    
    r[names(model$r)]  <- model$r
    rd[names(model$rd)] <- model$rd
    
    models[[ i ]] <- model
    setTxtProgressBar(pb,j)
  }
  
  model_r <- suppressWarnings( data.table::rbindlist( lapply(models,function(x) x[["r_df"]] ) , use.names=T, idcol="id" ) )
  model_P <- suppressWarnings( data.table::rbindlist( lapply(models,function(x) x[["pairs"]] ) , use.names=T, idcol="id" ) )
  identifierp <- unlist( lapply(models,`[[`,"identifierp"), FALSE, FALSE )
  identifier  <- unlist( lapply(models,`[[`,"identifier"), FALSE, FALSE )
  
  # Output, class and attributes ----
  class( model_r[[ id ]] ) <- class( model_P[[ id ]] )  <- class( data[[ id ]] ) 
  out <- structure(
    list(final_r  = r,
         final_rd = rd,
         r        = structure( model_r, identifier = identifier),
         pairs    = structure( model_P, identifier = identifierp)),
    class="sport",
    method = "dbl",
    formula = formula
  )
  
  return( out )
}