#' @importFrom data.table rbindlist
#' @importFrom stats setNames terms update
NULL

#' DBL rating algorithm
#' 
#' DBL rating algorithm
#' Wrapper arround `dbl` update algorithm. Wrapper allows user to simplify calculation providing only data and initial parameters assumptions
#' @param formula formula specifying model. DBL allows multiple variables in formula, also two-way interaction are available. LHS needs `rank|id`, to specify competitors order and event `id`.
#' @param data data.frame which contains columns specified in formula, and optionaly columns defined by `sig`, `weight` or `date`.
#' @param r named vector of initial estimates. If there is no assumption, initial ratings is set to be r=0. 
#' @param rd named vector of initial deviation estimates. In there is no assumption, initial is set to be rd=3.
#' @param sig named vector of rating volatile. In there is no assumption, initial ratings should be sig=0.5. Names of vector should correspond with team_name label.
#' @param weight name of column in `data` containing weights. Weights increasing or decreasing update change. Higher weight increasing impact of corresponding event.
#' @param idlab name of column in `data` containing date. Doesn't affect estimation process. If specified, charts displays estimates changes in time in
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
dbl_run <- function(formula, data, r, rd, sig, weight, idlab){
  if(missing(formula)) stop("Formula is not specified")
  if(missing(data)) stop("Data is not provided")
  if( !length(all.vars(update(formula, .~0)) ) %in% c(1,2) ) stop("Left hand side formula must contain one or two variables")
  if( length(all.vars(update(formula, .~0)) ) == 1) data$id <- 1
  

  if( missing(weight) ){
    data$weight <- 1
    weight      <- "weight"
  } 
  if( missing(sig) ){
    data$sig <- 1
    sig      <- "sig"
  } 
  
  all_params <- allLevelsList(formula, data)
  lhs  <- all.vars(update(formula, .~0))
  rhs  <- all.vars(update(formula, 0~.))
  y    <- lhs[1]
  id <- ifelse( length(lhs)==1 , "id", lhs[2])
  
  if(missing(r)) r  <- setNames(rep(0, length(all_params)), all_params )
  if(missing(rd)) rd <- setNames(rep(10, length(all_params)), all_params )
  if(missing(idlab)) idlab <- id
  
  if(any(class(data)=="data.frame")) 
    data_list <- split( data[ c(rhs, sig, weight,idlab) ], data[[ lhs[2] ]] )   
  unique_id  <- unique(data[[ lhs[2] ]]) 
  rank_list  <- split( data[[ lhs[1] ]] , data[[ lhs[2] ]])
  rider_list <- split( data[[ rhs[1] ]] , data[[ lhs[2] ]])
  
  models <- list()
  for(i in names(data_list)){
    terms <- createTermMatrix( formula, data_list[[ i ]][ rhs ] ) 
    model <- dbl(
      rider_list[[ i ]],
      rank    = rank_list[[ i ]],
      X       = as.matrix(terms),
      R       = r[ colnames(terms) ], 
      RD      = rd[ colnames(terms) ],
      sig     = data_list[[ i ]][[ sig ]],
      weight  = data_list[[ i ]][[ weight ]],
      identifier = as.character( data_list[[ i ]][[ idlab ]] )
    )
    
    r[names(model$r)]  <- model$r
    rd[names(model$rd)] <- model$rd
    
    models[[ i ]] <- model
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
         r        = structure( model_r, class="data.frame", identifier = identifier),
         pairs    = structure( model_P, class="data.frame", identifier = identifierp)),
    class="sport",
    method = "dbl",
    formula = formula
  )
  
  return( out )
}