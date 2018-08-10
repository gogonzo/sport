#' @importFrom dplyr bind_rows
NULL


#' Glicko rating algorithm
#' 
#' Glicko rating algorithm
#' Wrapper arround `glicko` update algorithm. Wrapper allows user to simplify calculation providing only data and initial parameters assumptions
#' @param formula formula specifying model. Glicko algorithm allows only player ranking parameter and should be specified by following manner: 
#' `rank | id ~ name`. Names in formula are unrestricted, but model structure remains the same where:
#' \enumerate{
#'  \item rank player position in event.
#'  \item id event identifier in which pairwise comparison is assessed.
#'  \item name of player.
#' }
#' @param data data.frame which contains columns specified in formula, and optionaly columns defined by `sig`, `weight` or `date`.
#' @param r named vector of initial players ratings estimates. In there is no assumption, initial ratings are set be r=1500. Names of vector should correspond with `name` in formula. 
#' @param rd named vector of initial rating deviation estimates. In there is no assumption, initial ratings are set be r=300 Names of vector should correspond with `name` in formula.
#' @param sig name of column in `data` containing rating volatility. Rating volitality is a value which multiplies prior `rd`. If `sig > 1` then prior `rd` increases, making estimate of `r` more uncertain.
#' @param weight name of column in `data` containing weights. Weights multiplies step update increasing/decreasing step impact on ratings estimates.
#' @param date name of column in `data` containing date. Doesn't affect estimation process. If specified, charts displays estimates changes in time instead of by observation `id`.
#' @export

glicko_run <- function(formula,data, r, rd, sig, weight,date){
  if(missing(formula)) stop("Formula is not specified")
  y  <- all.vars(formula)[1]
  id <- all.vars(formula)[2]
  x  <- gsub( "^1[ ]*|[ ]*","", all.vars(formula)[-(1:2)] )
  
  if( length(all.vars(update(formula, .~0)) ) != 2) stop("Left hand side formula must contain two variables")
  if( length(all.vars(update(formula, 0~.)) ) != 1) stop("Glicko expects only one variable which is ~ 1|pid_variable")
  if( missing(r) ){
    team_names <- unique(data[[x]])
    r <- setNames( rep(1500, length(team_names)), team_names )
    rd<- setNames( rep(300,  length(team_names)), team_names )
  }
  if( missing(sig) ){
    data$sig <- 1
    sig      <- "sig"
  } 
  if( missing(weight) ){
    data$weight <- 1
    weight      <- "weight"
  } 
  
  
  if(!class(data[[x]]) %in% c("character","factor")) warning(paste("variable",x,"is of class",class(x)))
  if(any(class(data)=="data.frame")) 
    data <- split(data[ c(y,id,x, sig, weight)], data[[ id ]] )   
  
  
  
  model_P <- list()
  model_r <- list()
  for(i in names(data)){
    team_names <- data[[ i ]][[ x ]]
    model      <- glicko( 
      team_names , 
      rank = data[[ i ]][[ y ]], 
      r    =  r[team_names ], 
      rd   = rd[team_names ], 
      sig = data[[ i ]][[ sig ]] ,
      weight = data[[ i ]][[ weight ]] 
    )    
    r [ team_names ] <- model$r[  team_names ]
    rd[ team_names ] <- model$rd[ team_names ]
    
    model_r[[ i ]] <- data.frame(names=team_names, r = model$r, rd = model$rd)
    model_P[[ i ]] <- model$pairs
    
  }
  model_r <- bind_rows(model_r, .id = id )
  model_P <- bind_rows(model_P, .id = id )
  
  return(
    list(
      r = model_r,
      pairs = model_P,
      final_r = r,
      final_rd = rd
    )
  )
}
