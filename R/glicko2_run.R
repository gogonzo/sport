#' @importFrom dplyr bind_rows
NULL

#' Glicko2 rating algorithm
#' 
#' Glicko2 rating algorithm
#' Wrapper arround `glicko2` update algorithm. Wrapper allows user to simplify calculation providing only data and initial parameters assumptions
#' @param formula formula specifying model. Glicko2 algorithm allows only player ranking parameter and should be specified by following manner: 
#' `rank | id ~ name`. Names in formula are unrestricted, but model structure remains the same where:
#' \enumerate{
#'  \item rank alternative/participant position in experiment/event. Order of comparison
#'  \item id event identifier
#'  \item name of alternative/participant
#' }
#' @param data data.frame which contains columns specified in formula, and optionaly columns defined by `weight` or `date`.
#' @param r named vector of initial rating estimates. In there is no assumption, initial ratings should be r=1500. Names of vector should correspond with team_name label. 
#' @param rd named vector of initial rating deviation estimates. In there is no assumption, initial ratings should be r=300 Names of vector should correspond with team_name label.
#' @param sig named vector of rating volatile. In there is no assumption, initial ratings should be sig=0.5. Names of vector should correspond with team_name label.
#' @param weight name of column in `data` containing weights. Weights multiplies step update increasing/decreasing step impact on parameters estimates
#' @param date name of column in `data` containing date. Doesn't affect estimation process. If specified, charts displays estimates changes in time instead of by observation `id`
#' @export

glicko2_run <- function(formula, data, r, rd,sig, weight){
  if(missing(formula)) stop("Formula is not specified")
  if( length(all.vars(update(formula, .~0)) ) != 2) stop("Left hand side formula must contain two variables")
  if( length(all.vars(update(formula, 0~.)) ) != 1) stop("Glicko expects only one variable which is ~ 1|pid_variable")
  
  y  <- all.vars(formula)[1]
  id <- all.vars(formula)[2]
  x  <- gsub( "^1[ ]*|[ ]*","", all.vars(formula)[-(1:2)] )
  
  if( missing(r) ){
    team_names <- unique(data[[x]])
    r   <- setNames( rep( 1500, length( team_names) ), team_names )
    rd  <- setNames( rep( 300 , length( team_names) ), team_names )
    sig <- setNames( rep( 0.05, length( team_names) ), team_names )
  }
  if( missing(weight) ){
    data$weight <- 1
    weight <- "weight"
  } 
  
  if(any(class(data)=="data.frame")) 
    data <- split( data[ c(y,id,x, weight) ], data[[ id ]] ) 
  
  model_r <- list()
  model_P <- list()
  for(i in names(data)){
    team_names <- data[[ i ]][[ x ]]
    
    model <- glicko2( 
      team_names , 
      rank   = data[[ i ]][[ y ]], 
      r      = r[ team_names ] ,  
      rd     = rd[ team_names ] , 
      sig    = sig[ team_names ] , 
      weight = data[[ i ]][[ weight ]]
    )  
    r [ team_names ] <- model$r[  team_names ]
    rd[ team_names ] <- model$rd[ team_names ]
    sig[ team_names ] <- model$sig[ team_names ]
    
    model_r[[ i ]] <- data.frame(names=team_names, r = model$r, rd = model$rd)
    model_P[[ i ]] <- model$pairs
    
  }
  
  model_r <- bind_rows(model_r, .id = id)
  model_P <- bind_rows(model_P, .id = id)
  
  return(
    list(
      r = model_r,
      pairs = model_P,
      final_r = r,
      final_rd = rd
    )
  )
}