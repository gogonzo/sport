#' @importFrom dplyr bind_rows
NULL

#' Glicko rating algorithm
#' 
#' Glicko rating algorithm
#' Wrapper arround `glicko` update algorithm. Wrapper allows user to simplify calculation providing only data and initial parameters assumptions
#' @param formula formula specifying model. Glicko algorithm allows only player ranking parameter.
#' @param data data.frame which contains three columns: #' \enumerate{
#'   \item id event identifier
#'   \item team_name label of event participant -  name corresponding with parameters names
#'   \item rank position of team_name in event
#'   \item time additional parameter increasing variance of participant rating related with time. 
#' }  
#' @param r named vector of initial rating estimates. In there is no assumption, initial ratings should be r=1500. Names of vector should correspond with team_name label. 
#' @param  rd named vector of initial rating deviation estimates. In there is no assumption, initial ratings should be r=300 Names of vector should correspond with team_name label.
#' @export
glicko_run <- function(formula, data, r, rd, time){
  y  <- all.vars(formula)[1]
  id <- all.vars(formula)[2]
  x  <- all.vars(formula)[-(1:2)]
  
  if( missing(r) ){
    team_names <- unique(data[[x]])
    r <- setNames( rep(1500, length(team_names)), team_names )
    rd<- setNames( rep(300,  length(team_names)), team_names )
  }
  if( missing(time) ){
    data$time <- 0
    time <- "time"
  } 
  
  if(any(class(data)=="data.frame"))
    data <- split( data, data[[ id ]] )  
  
  
  
  model_P <- list()
  model_r <- list()
  for(i in names(data)){
    team_names <- data[[ i ]][[ x ]]
    model      <- glicko( 
      team_names , 
      rank = data[[ i ]][[ y ]], 
      r    =  r[team_names ], 
      rd   = rd[team_names ], 
      time = data[[ i ]][[ time ]] 
    )    
    r [ team_names ] <- model$r[  team_names ]
    rd[ team_names ] <- model$rd[ team_names ]
    
    model_r[[ i ]] <- data.frame(names=team_names, r = model$r, rd = model$rd)
    model_P[[ i ]] <- model$pairs
    
  }
  model_r <- bind_rows(model_r, .id = "id")
  model_P <- bind_rows(model_P, .id = "id")
  
  return(
    list(
      r = model_r,
      pairs = model_P,
      final_r = r,
      final_rd = rd
    )
  )
}
