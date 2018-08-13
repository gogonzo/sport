#' @importFrom dplyr bind_rows
#' @importFrom stats setNames terms update 
NULL

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
#' @param sig named vector of rating volatile. In there is no assumption, initial ratings should be sig=0.5. Names of vector should correspond with team_name label.
#' @param tau The system constant. Which constrains the change in volatility over time. Reasonable choices are between 0.3 and 1.2 (`default = 0.5`), though the system should be tested to decide which value results in greatest predictive accuracy. Smaller values of `tau` prevent the volatility measures from changing by largeamounts, which in turn prevent enormous changes in ratings based on very improbable results. If the application of Glicko-2 is expected to involve extremely improbable collections of game outcomes, then `tau` should be set to a small value, even as small as, say, `tau= 0`.2.
#' @param weight name of column in `data` containing weights. Weights increasing or decreasing update change. Higher weight increasing impact of corresponding event.
#' @param date name of column in `data` containing date. Doesn't affect estimation process. If specified, charts displays estimates changes in time instead of by event `id`
#' @param init_r initial rating for new competitors (contains NA). Default = 1500
#' @param init_rd initial rating deviations for new competitors. Default = 350
#' @return 
#' A "sport" object is returned
#' \itemize{
#'   \item \code{final_r} named vector containing players ratings.
#'   \item \code{final_rd} named vector containing players ratings deviations.
#'   \item \code{final_sig} named vector containing players ratings volatiles.
#'   \item \code{r} data.frame with evolution of the ratings and ratings deviations estimated at each event.
#'   \item \code{pairs} pairwise combinations of players in analysed events with prior probability and result of a challange.
#'   \item \code{class} of the object
#'   \item \code{method} type of algorithm used
#'   \item \code{formula} modelled formula
#' }
#' @export

glicko2_run <- function(formula, data, r, rd,sig, tau, weight, date, init_r = 1500, init_rd=350){
  if(missing(formula)) stop("Formula is not specified")
  if( !length(all.vars(update(formula, .~0)) )  %in% c(1,2)) stop("Left hand side formula must contain two variables")
  if( length(all.vars(update(formula, 0~.)) ) != 1) stop("Glicko expects only one variable which is ~ 1|pid_variable")
  
  y  <- all.vars(formula)[1]
  id <- all.vars(formula)[2]
  x  <- gsub( "^1[ ]*|[ ]*","", all.vars(formula)[-(1:2)] )
  
  if( missing(r) ){
    player_names <- unique(data[[x]])
    r   <- setNames( rep( init_r, length( player_names) ), player_names )
  }
  if( missing(rd) ){
    player_names <- unique(data[[x]])
    rd  <- setNames( rep( init_rd , length( player_names) ), player_names )
  }
  if( missing(sig) ){
    player_names <- unique(data[[x]])
    sig <- setNames( rep( 0.05, length( player_names) ), player_names )
  }
  if( missing(weight) ){
    data$weight <- 1
    weight <- "weight"
  } 
  
  if(any(class(data)=="data.frame")) 
    data_list <- split( data[ c(y,id,x, weight) ], data[[ id ]] ) 
  
  model_r <- list()
  model_P <- list()
  for(i in names(data_list)){
    player_names <- data_list[[ i ]][[ x ]]
    
    model <- glicko2( 
      player_names , 
      rank   = data_list[[ i ]][[ y ]], 
      r      = r[ player_names ] ,  
      rd     = rd[ player_names ] , 
      sig    = sig[ player_names ] , 
      weight = data_list[[ i ]][[ weight ]],
      init_r = init_r,
      init_rd = init_rd
    )  
    r [ player_names ] <- model$r[  player_names ]
    rd[ player_names ] <- model$rd[ player_names ]
    sig[ player_names ] <- model$sig[ player_names ]
    
    model_r[[ i ]] <- data.frame(names=player_names, r = model$r, rd = model$rd, sig = model$sigma)
    model_P[[ i ]] <- model$pairs
    
  }
  
  model_r <- suppressWarnings( dplyr::bind_rows( model_r , .id = id ) )
  model_P <- suppressWarnings( dplyr::bind_rows( model_P , .id = id ) )
  
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
         final_sig=sig,
         r        = structure( model_r, class="data.frame", identifier = model_r_date),
         pairs    = structure( model_P, class="data.frame", identifier = model_P_date)),
    class="sport",
    method = "glicko2",
    formula = formula
  )
  
  return( out )
}