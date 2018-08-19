#' @importFrom data.table rbindlist
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
#' @param weight name of column in `data` containing weights. Weights increasing or decreasing update change. Higher weight increasing impact of corresponding event.
#' @param idlab name of column in `data` containing date. Doesn't affect estimation process. If specified, charts displays estimates changes in time instead of by observation `id`
#' @param beta The additional variance of performance. As beta increases, the performance is more uncertain and update change is smaller. By default `beta = 25/6`.
#' @param gamma can help to control how fast the variance `rd` is reduced after updating. Lower `gamma` slows decreasing of `rd`, which tends to reach zero to quickly. The default value is `gamma = rd/c`.
#' @param init_r initial values for `r` if not provided. Default = 25
#' @param init_rd initial values for `r` if not provided. Default = 25/3
#' @return 
#' A "ratings" object is returned
#' \itemize{
#'   \item \code{final_r} named vector containing players ratings.
#'   \item \code{final_rd} named vector containing players ratings deviations.
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
#' bbt <- bbt_run( rank ~ name, data )
#' @export
bbt_run <- function(formula, data, r,rd, sig, weight,beta=25/6, gamma, idlab, init_r = 25, init_rd=25/3){
  is_formula_missing(formula)
  is_data_provided(data)
  is_lhs_valid(formula)
  is_rhs_valid(formula, "bbt_run")

  lhs  <- all.vars(update(formula, .~0))
  rhs  <- all.vars(update(formula, 0~.))
  y    <- lhs[1]
  x    <- rhs[1]
  id <- ifelse( length(lhs)==1 , "id", lhs[2])
  if( length(lhs) == 1) data$id <- 1
  
  if(missing(r)){
    message(paste("r is missing and will set to default="), init_r)
    names <- unique( data[[ x ]] )
    r <- as.matrix( setNames( rep(init_r, length(names)), names ) ) }
  if(missing(rd)){
    message(paste("rd is missing and will set to default="), init_rd)
    names <- unique( data[[ x ]] )
    rd<- as.matrix( setNames( rep(init_rd, length(names)), names ) ) }
  if( missing(sig) ){
    data$sig <- 1; sig <- "sig" } 
  if( missing(weight) ){
    data$weight <- 1; weight <- "weight" } 
  if(missing(gamma)) 
    gamma <- 999
  if(missing(idlab)) 
    idlab <- id 
  if(class(r)!="matrix")   r <- as.matrix(r)
  if(class(rd)!="matrix") rd <- as.matrix(rd)
  
  if(any(class(data)=="data.frame")) 
    data_list <- split(data[ unique(c(y,id,x, sig, weight, idlab))], data[[ id ]] ) 
  
  j <- 0
  n <- length(data_list)
  pb <- txtProgressBar(min=0, max=n, width=20, initial=0, style=3)
  models <- list()
  for(i in names(data_list)){
    j <- j + 1
    team_name <- data_list[[ i ]][[ x ]]
    model     <- bbt( 
      team_name, 
      rank    = data_list[[ i ]][[ y ]], 
      r       = r[ team_name,,drop=FALSE], 
      rd      = rd[ team_name,,drop=FALSE],
      sig     = data_list[[ i ]][[ sig ]],
      weight  = data_list[[ i ]][[ weight ]],
      beta = beta,
      gamma = gamma,
      identifier = as.character( data_list[[ i ]][[ idlab ]] ),
      init_r = init_r,
      init_rd = init_rd
    )
    
    r [ team_name, ] <- model$r[  team_name, ]
    rd[ team_name, ] <- model$rd[ team_name, ]
    
    models[[ i ]] <- model
    setTxtProgressBar(pb,j)
  }
  
  model_r <- suppressWarnings( data.table::rbindlist( lapply(models,function(x) x[["r_df"]] ) , use.names=T, idcol="id" ) )
  model_P <- suppressWarnings( data.table::rbindlist( lapply(models,function(x) x[["pairs"]] ) , use.names=T, idcol="id" ) )
  identifierp <- unlist( lapply(models,`[[`,"identifierp"), FALSE, FALSE )
  identifier  <- unlist( lapply(models,`[[`,"identifier"), FALSE, FALSE )
  
  # Output, class and attributes ----
  class( model_r[[ id ]] ) <- class( model_P[[ id ]] ) <- class( data[[ id ]] )
  
  out <- structure(
    list(final_r  = setNames(as.vector(r), rownames(r)),
         final_rd = setNames(as.vector(rd), rownames(rd)),
         r        = structure( model_r, identifier = identifier),
         pairs    = structure( model_P, identifier = identifierp)),
    class="rating",
    method = "bbt",
    formula = formula,
    settings = list(sig=sig, weight=weight,beta=beta, gamma=gamma, idlab=idlab, init_r=init_r, init_rd=init_rd)
  )
  
  return( out )
}