#' @importFrom dplyr bind_rows
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
#' @param data data.frame which contains columns specified in formula, and optionaly columns defined by `sig`, `weight` or `date`.
#' @param r named vector of initial rating estimates. In there is no assumption, initial ratings is set to be r=25 Names of vector should correspond with `name` in formula. 
#' @param rd named vector of initial rating deviation estimates. In there is no assumption, initial is set to be r=25/3 Names of vector should correspond with `name` in formula.
#' @param sig name of column in `data` containing rating volatility. Rating volitality is a value which multiplies prior `rd`. If `sig > 1` then prior `rd` increases, making estimate of `r` more uncertain.
#' @param weight name of column in `data` containing weights. Weights multiplies step update increasing/decreasing step impact on ratings estimates.
#' @param date name of column in `data` containing date. Doesn't affect estimation process. If specified, charts displays estimates changes in time instead of by observation `id`
#' @param kappa small positive value to ensure rd positive after update. Higher value of `kappa` limits `rd` change size, and lower value of `kappa` allows `rd` update to be bigger. By default `kappa=0.0001`
#' @param beta The additional variance of performance. By default `beta = 25/6`.
#' @param gamma can help to control how fast the variance `rd` is reduced after updating. Lower `gamma` slow down decreasing of `rd`, which tends to reach zero to quickly. The default value is `gamma = rd/c`.
#' @param init_r initial rating for new competitors (contains NA). Default = 25
#' @param init_rd initial rating deviations for new competitors. Default = 25/3
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

bbt_run <- function(formula, data, r,rd, sig, weight, kappa, beta, gamma, date, init_r = 25, init_rd=25/3){
  if(missing(formula)) stop("Formula is not specified")
  if( length(all.vars(update(formula, .~0)) ) != 2) stop("Left hand side formula must contain two variables")
  if( missing(sig) ){
    data$sig <- 1
    sig <- "sig"
  } 
  if( missing(weight) ){
    data$weight <- 1
    weight <- "weight"
  } 

  y  <- all.vars(formula)[1]
  id <- all.vars(formula)[2]
  x  <- all.vars(formula)[-(1:2)]
  
  if(missing(r)){
    player_names <- unique( data[[ x ]] )
    r <- as.matrix( setNames( rep(init_r, length(player_names)), player_names ) )
  }
  if(missing(rd)){
    player_names <- unique( data[[ x ]] )
    rd<- as.matrix( setNames( rep(init_rd,  length(player_names)), player_names ) )
  }
  if(class(r)!="matrix"){
    r <- as.matrix(r)
  }
  if(class(rd)!="matrix"){
    rd <- as.matrix(rd)
  }
  
  if(any(class(data)=="data.frame")) 
    data_list <- split(data[ c(y,id,x, sig, weight)], data[[ id ]] ) 
  
  model_r <- list()
  model_P <- list()
  for(i in names(data_list)){
    
    team_name <- data_list[[ i ]][[ x ]]
    model   <- bbt( 
      team_name, 
      rank    = data_list[[ i ]][[ y ]], 
      r       = r[ team_name,,drop=FALSE], 
      rd      = rd[ team_name,,drop=FALSE],
      sig     = data_list[[ i ]][[ sig ]],
      weight  = data_list[[ i ]][[ weight ]],
      init_r = init_r,
      init_rd = init_rd
    )
    
    r [ team_name, ] <- model$r[  team_name, ]
    rd[ team_name, ] <- model$rd[ team_name, ]
    
    model_P[[ i ]] <- model$pairs
    model_r[[ i ]] <- data.frame(names=team_name, r = model$r, rd = model$rd)
  }
  
  model_r <- dplyr::bind_rows( model_r , .id = id )
  model_P <- dplyr::bind_rows( model_P , .id = id )
  
  # Output, class and attributes ----
  class( model_r[[ id ]] ) <- class( model_P[[ id ]] )  <- class( data[[ id ]] )
  if(!missing(date)){
    dates <- unique( data[ colnames(data) %in% c(id,date) ] )
    model_r_date <- dates[[ date ]][ match( model_r[[ id ]], dates[[ id ]] ) ] 
    model_P_date <- dates[[ date ]][ match( model_P[[ id ]], dates[[ id ]] ) ] 
  } else {
    model_r_date <- model_r[[ id ]]
    model_P_date <- model_P[[ id ]]
  }
  
  out <- structure(
    list(final_r  = r,
         final_rd = rd,
         r        = structure( model_r, class="data.frame", identifier = model_r_date),
         pairs    = structure( model_P, class="data.frame"  , identifier = model_P_date)),
    class="sport",
    method = "bbt",
    formula = formula
  )
  
  return( out )
}