#' @importFrom dplyr bind_rows
NULL

#' BBT rating algorithm
#' 
#' BBT rating algorithm
#' Wrapper arround `bbt` update algorithm. Wrapper allows user to simplify calculation providing only data and initial parameters assumptions
#' @param formula formula specifying model. BBT algorithm allows only player ranking parameter and should be specified by following manner: 
#' `rank | id ~ name`. Names in formula are unrestricted, but model structure remains the same where:
#' \enumerate{
#'  \item rank player position in event.
#'  \item id event identifier in which pairwise comparison is assessed.
#'  \item name of player.
#' }
#' @param data data.frame which contains columns specified in formula, and optionaly columns defined by `sig`, `weight` or `date`.
#' @param r named vector of initial rating estimates. In there is no assumption, initial ratings is set to be r=25 Names of vector should correspond with `name` in formula. 
#' @param rd named vector of initial rating deviation estimates. In there is no assumption, initial is set recomended to be r=25/3 Names of vector should correspond with `name` in formula.
#' @param sig name of column in `data` containing rating volatility. Rating volitality is a value which multiplies prior `rd`. If `sig > 1` then prior `rd` increases, making estimate of `r` more uncertain.
#' @param weight name of column in `data` containing weights. Weights multiplies step update increasing/decreasing step impact on ratings estimates.
#' @param date name of column in `data` containing date. Doesn't affect estimation process. If specified, charts displays estimates changes in time instead of by observation `id`
#' @export

bbt_run <- function(formula, data, r,rd, sig, weight){
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
  
  if(any(class(data)=="data.frame")) 
    data <- split(data[ c(y,id,x, sig, weight)], data[[ id ]] ) 
  
  model_r <- list()
  model_P <- list()
  for(i in names(data)){
    
    team_name <- data[[ i ]][[ x ]]
    model   <- bbt( 
      team_name, 
      rank    = data[[ i ]][[ y ]], 
      r       = r[ team_name,,drop=FALSE], 
      rd      = rd[ team_name,,drop=FALSE],
      sig     = data[[ i ]][[ sig ]],
      weight  = data[[ i ]][[ weight ]]
    )
    
    r [ team_name, ] <- model$r[  team_name, ]
    rd[ team_name, ] <- model$rd[ team_name, ]
    
    model_P[[ i ]] <- model$pairs
    model_r[[ i ]] <- data.frame(names=team_name, r = model$r, rd = model$rd)
  }
  
  model_r <- bind_rows( model_r, .id = id)
  model_P <- bind_rows( model_P, .id = id)
  
  return(
    list(
      r = model_r,
      pairs = model_P,
      final_r = r,
      final_rd = rd
    )
  )
}