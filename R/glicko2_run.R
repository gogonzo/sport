#' @importFrom data.table rbindlist
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
#' @param sigma named vector of rating volatile. In there is no assumption, initial ratings should be sigma=0.5. Names of vector should correspond with team_name label.
#' @param tau The system constant. Which constrains the change in volatility over time. Reasonable choices are between 0.3 and 1.2 (`default = 0.5`), though the system should be tested to decide which value results in greatest predictive accuracy. Smaller values of `tau` prevent the volatility measures from changing by largeamounts, which in turn prevent enormous changes in ratings based on very improbable results. If the application of Glicko-2 is expected to involve extremely improbable collections of game outcomes, then `tau` should be set to a small value, even as small as, say, `tau= 0`.2.
#' @param weight name of column in `data` containing weights. Weights increasing or decreasing update change. Higher weight increasing impact of corresponding event.
#' @param kappa controls `rd` shrinkage not to be greater than `rd*(1-kappa)`. `kappa=1` means that `rd` will not be decreased.
#' @param idlab name of column in `data` containing date. Doesn't affect estimation process. If specified, charts displays estimates changes in time instead of by event `id`
#' @param init_r initial values for `r` if not provided. Default = 1500
#' @param init_rd initial values for `r` if not provided. Default = 350
#' @param pb logical, if TRUE progress bar will appear in console. Default = FALSE
#' @return 
#' A "rating" object is returned
#' \itemize{
#'   \item \code{final_r} named vector containing players ratings.
#'   \item \code{final_rd} named vector containing players ratings deviations.
#'   \item \code{final_sigma} named vector containing players ratings volatiles.
#'   \item \code{r} data.frame with evolution of the ratings and ratings deviations estimated at each event.
#'   \item \code{pairs} pairwise combinations of players in analysed events with prior probability and result of a challange.
#'   \item \code{class} of the object
#'   \item \code{method} type of algorithm used
#'   \item \code{formula} modelled formula
#' }
#' @examples
#' # Example from Glickman
#' data <- data.frame( name = c( "A", "B", "C", "D" ), 
#'                     rank = c( 3, 4, 1, 2 ))
#' glicko2 <- glicko2_run( rank ~ name, data )
#' @export
glicko2_run <- function(formula, data, r, rd,sigma, tau=0.5, weight,kappa=0.5, idlab, init_r = 1500, init_rd=350, pb=FALSE){
  is_formula_missing(formula)
  is_data_provided(data)
  is_lhs_valid(formula)
  is_rhs_valid(formula, "glicko2_run")
  
  lhs  <- all.vars(update(formula, .~0))
  rhs  <- all.vars(update(formula, 0~.))
  y    <- lhs[1]
  x    <- rhs[1]
  id <- ifelse( length(lhs)==1 , "id", lhs[2])
  data[[x]] <- as.character(data[[x]])
  
  if( length(lhs) == 1) data$id <- 1
  
  if( missing(r) ){
    message(paste("r is missing and will set to default="), init_r)
    player_names <- unique(data[[x]]);
    r   <- setNames( rep( init_r, length( player_names) ), player_names ) }
  if( missing(rd) ){
    message(paste("rd is missing and will set to default="), init_rd)
    player_names <- unique(data[[x]])
    rd  <- setNames( rep( init_rd , length( player_names) ), player_names ) }
  if( missing(sigma) ){
    message("sigma is missing and will set to default=0.05")
    player_names <- unique(data[[x]])
    sigma <- setNames( rep( 0.05, length( player_names) ), player_names )
  }
  if( missing(weight) ){ 
    data$weight <- 1; weight="weight"}
  if( missing(idlab) )   
    idlab <- id
  if(kappa==0) kappa=0.0001
  if(!is.character(data[[x]])) {
    message(paste0("\nvariable '",x,"' is of class ",class(data[[x]])[1]," and will be converted to character"))
    data[[x]] <- as.character(data[[x]])
  }
  if(is.data.frame(data)) 
    data_list <- split(data[ unique(c(y,id,x, weight,idlab))], data[[ id ]] )
  
  
  n <- length(data_list)
  if(pb){  j <- 0; pb <- txtProgressBar(min=0, max=n, width=20, initial=0, style=3) }
  models <- list()
  for(i in names(data_list)){
    player_names <- data_list[[ i ]][[ x ]]
    
    model <- glicko2( 
      player_names , 
      rank   = data_list[[ i ]][[ y ]], 
      r      = r[ player_names ] ,  
      rd     = rd[ player_names ] , 
      sigma    = sigma[ player_names ] , 
      tau    = tau,
      weight = data_list[[ i ]][[ weight ]],
      kappa = kappa,
      identifier = as.character( data_list[[ i ]][[ idlab ]] ), 
      init_r = init_r,
      init_rd = init_rd
    )  
    
    if(any(!is.finite(model$rd) | !is.finite(model$r) | !is.finite(model$sigma) | model$rd < 0))
      stop(paste0("Parameters error after evaluating ", id,"=",i),call. = F)
    
    r [ player_names ] <- model$r[  player_names ]
    rd[ player_names ] <- model$rd[ player_names ]
    sigma[ player_names ] <- model$sigma[ player_names ]
    
    models[[ i ]] <- model
    if(pb){ j <- j + 1; setTxtProgressBar(pb,j) }
  }
  
  model_r <- suppressWarnings( data.table::rbindlist( lapply(models,function(x) x[["r_df"]] ) , use.names=T, idcol="id" ) )
  model_P <- suppressWarnings( data.table::rbindlist( lapply(models,function(x) x[["pairs"]] ) , use.names=T, idcol="id" ) )
  identifierp <- unlist( lapply(models,`[[`,"identifierp"), FALSE, FALSE )
  identifier  <- unlist( lapply(models,`[[`,"identifier"), FALSE, FALSE )
  
  # Output, class and attributes ----
  class( model_r[[ id ]] ) <- class( model_P[[ id ]] )  <- class( data[[ id ]] )
  
  # add winning probability to data    
  p <- model_P[,.(p_win = prod(P)), by=c("id","name")][,
                  p_win := p_win/sum(p_win), by = "id"]
  model_r <- merge(model_r, p, all.x=T, by=c("id","name"),sort=F)
  
  out <- structure(
    list(final_r  = r,
         final_rd = rd,
         final_sigma = sigma,
         r        = structure( model_r, identifier = identifier),
         pairs    = structure( model_P, identifier = identifierp)),
    class="rating",
    method = "glicko2",
    formula = formula,
    settings = list(init_r=init_r, init_rd=init_rd, tau=tau, weight=weight, kappa=kappa, idlab=idlab)
  )
  
  return( out )
}