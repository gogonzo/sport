#' @importFrom data.table rbindlist
#' @importFrom stats setNames terms update
#' @importFrom utils setTxtProgressBar txtProgressBar
NULL

#' Glicko rating algorithm
#' 
#' Glicko rating algorithm
#' Wrapper arround `glicko` update algorithm. Wrapper allows user to simplify calculation providing only data and initial parameters assumptions
#' @param formula formula specifying model. Glicko algorithm allows only player ranking parameter and should be specified by following manner: 
#' `rank | id ~ name`. Names in formula are unrestricted, but model structure remains the same:
#' \itemize{
#'  \item {rank} player position in event.
#'  \item {id} event identifier in which pairwise comparison is assessed.
#'  \item {name} of player.
#' }
#' @param data data.frame which contains columns specified in formula, and optionaly columns defined by `sigma`, `weight` or `date`.
#' @param r named vector of initial players ratings estimates. In there is no assumption, initial ratings are set be r=1500. Names of vector should correspond with `name` in formula. 
#' @param rd named vector of initial rating deviation estimates. In there is no assumption, initial ratings are set be r=300 Names of vector should correspond with `name` in formula.
#' @param sigma name of column in `data` containing rating volatility. Rating volitality is a value which multiplies prior `rd`. If `sigma > 0` then prior `rd` increases, making estimate of `r` more uncertain.
#' @param weight name of column in `data` containing weights. Weights increasing or decreasing update change. Higher weight increasing impact of corresponding event.
#' @param kappa controls `rd` shrinkage not to be greater than `rd*(1-kappa)`. `kappa=1` means that `rd` will not be decreased.
#' @param idlab name of column in `data` containing date. Doesn't affect estimation process. If specified, charts displays estimates changes in time instead of by observation `id`.
#' @param init_r initial values for `r` if not provided. Default = 1500
#' @param init_rd initial values for `r` if not provided. Default = 350
#' @param pb logical, if TRUE progress bar will appear in console. Default = FALSE
#' @return 
#' A "rating" object is returned: \itemize{
#' \item \code{final_r} named vector containing players ratings.
#' \item \code{final_rd} named vector containing players ratings deviations.
#' \item \code{r} data.frame with evolution of the ratings and ratings deviations estimated at each event.
#' \item \code{pairs} pairwise combinations of players in analysed events with prior probability and result of a challange.
#' \item \code{class} of the object.
#' \item \code{method} type of algorithm used.
#' \item \code{formula} modelled formula.
#' }
#' @examples
#' # Example from Glickman
#' data <- data.frame( name = c( "A", "B", "C", "D" ), 
#'                     rank = c( 3, 4, 1, 2 ))
#' glicko <- glicko_run( rank ~ name, data )
#' @export
glicko_run <- function(formula, data, r, rd, sigma, weight, kappa=0.5, idlab, init_r=1500, init_rd=350, pb=FALSE){
  is_formula_missing(formula)
  is_data_provided(data)
  is_lhs_valid(formula)
  is_rhs_valid(formula, "glicko_run")

  lhs  <- all.vars(update(formula, .~0))
  rhs  <- all.vars(update(formula, 0~.))
  y    <- lhs[1]
  x    <- rhs[1]
  id <- ifelse( length(lhs)==1 , "id", lhs[2])
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
    data$sigma <- 0; sigma <- "sigma" } 
  if( missing(weight) ){ 
    data$weight <- 1; weight="weight"}
  if( missing(idlab) )   
    idlab <- id
  if( !class(data[[x]]) %in% c("character","factor")) {
    message(paste0("\nvariable '",x,"' is of class ",class(data[[x]])," and will be converted to character"))
    data[[x]] <- as.character(data[[x]])
  }
  if(kappa==0) kappa=0.0001
  if(any(class(data)=="data.frame")) 
    data_list <- split(data[ unique(c(y,id,x, sigma, weight,idlab))], data[[ id ]] )
  
  j <- 0
  n <- length(data_list)
  if(pb) pb <- txtProgressBar(min=0, max=n, width=20, initial=0, style=3)
  models <- list()
  for(i in names(data_list)){
    player_names <- data_list[[ i ]][[ x ]]
    model      <- glicko( 
      name   = player_names , 
      rank   = data_list[[ i ]][[ y ]], 
      r      = r[player_names ], 
      rd     = rd[player_names ], 
      sigma  = data_list[[ i ]][[ sigma ]] ,
      weight = data_list[[ i ]][[ weight ]],
      kappa  = kappa,
      identifier = as.character( data_list[[ i ]][[ idlab ]] ),
      init_r = init_r,
      init_rd = init_rd
    )    
    
    if(any(!is.finite(model$rd) | !is.finite(model$r) | model$rd < 0))
      stop(paste0("Parameters error after evaluating ", id,"=",i),call. = F)
    
    r [ player_names ] <- model$r[  player_names ]
    rd[ player_names ] <- model$rd[ player_names ]

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
         r        = structure( model_r, identifier = identifier),
         pairs    = structure( model_P, identifier = identifierp)),
    class    = "rating",
    method   = "glicko",
    formula  = formula,
    settings = list(init_r = init_r, init_rd = init_rd, sigma=sigma, weight=weight, kappa=kappa, idlab=idlab)
  )
  
  return( out )
}

