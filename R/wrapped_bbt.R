#' @importFrom dplyr bind_rows
NULL

#' BBT rating algorithm
#' 
#' BBT rating algorithm
#' Wrapper arround `bbt` update algorithm. Wrapper allows user to simplify calculation providing only data and initial parameters assumptions
#' @param formula formula specifying model. BBT algorithm allows only player ranking parameter.
#' @param data data.frame which contains three columns: #' \enumerate{
#'   \item id event identifier
#'   \item team_name label of event participant -  name corresponding with parameters names
#'   \item rank position of team_name in event
#' }  
#' @param r named vector of initial rating estimates. In there is no assumption, initial ratings should be r=25 Names of vector should correspond with team_name label. 
#' @param  rd named vector of initial rating deviation estimates. In there is no assumption, initial ratings should be r=25/3 Names of vector should correspond with team_name label.
#' @export
bbt_run <- function(formula, data, r,rd){
  if(missing(formula)) stop("Formula is not specified")
  if( length(all.vars(update(formula, .~0)) ) != 2) stop("Left hand side formula must contain two variables")
  
  y  <- all.vars(formula)[1]
  id <- all.vars(formula)[2]
  x  <- all.vars(formula)[-(1:2)]
  
  if(missing(r)){
    team_names <- unique( data[[ x ]] )
    r <- as.matrix( setNames( rep(25, length(team_names)), team_names ) )
  }
  if(missing(rd)){
    team_names <- unique( data[[ x ]] )
    rd<- as.matrix( setNames( rep(25/3,  length(team_names)), team_names ) )
  }
  if(class(r)!="matrix"){
    r <- as.matrix(r)
  }
  if(class(rd)!="matrix"){
    rd <- as.matrix(rd)
  }
  
  if(any(class(data)=="data.frame")) data <- split(data, data[[ id ]] ) 
  
  model_r <- list()
  model_P <- list()
  for(i in names(data)){
    
    team_name <- data[[ i ]][[ x ]]
    model   <- bbt( 
      team_name, 
      rank = data[[ i ]][[ y ]], 
      r  = r[team_name,,drop=FALSE], 
      rd = rd[team_name,,drop=FALSE]
    )
    
    r [ team_name, ] <- model$r[  team_name, ]
    rd[ team_name, ] <- model$rd[ team_name, ]
    
    model_P[[ i ]] <- model$pairs
    model_r[[ i ]] <- data.frame(names=team_name, r = model$r, rd = model$rd)
  }
  
  model_r <- bind_rows( model_r, .id = "id")
  model_P <- bind_rows( model_P, .id = "id")
  
  return(
    list(
      r = model_r,
      pairs = model_P,
      final_r = r,
      final_rd = rd
    )
  )
}