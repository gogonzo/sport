#' @importFrom data.table rbindlist
#' @importFrom stats setNames terms update
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
#' @param data data.frame which contains columns specified in formula, and optionaly columns defined by `sig`, `weight` or `date`.
#' @param r named vector of initial players ratings estimates. In there is no assumption, initial ratings are set be r=1500. Names of vector should correspond with `name` in formula. 
#' @param rd named vector of initial rating deviation estimates. In there is no assumption, initial ratings are set be r=300 Names of vector should correspond with `name` in formula.
#' @param sig name of column in `data` containing rating volatility. Rating volitality is a value which multiplies prior `rd`. If `sig > 1` then prior `rd` increases, making estimate of `r` more uncertain.
#' @param weight name of column in `data` containing weights. Weights increasing or decreasing update change. Higher weight increasing impact of corresponding event.
#' @param idlab name of column in `data` containing date. Doesn't affect estimation process. If specified, charts displays estimates changes in time instead of by observation `id`.
#' @param init_r initial values for `r` if not provided. Default = 1500
#' @param init_rd initial values for `r` if not provided. Default = 350
#' @return 
#' A "sport" object is returned: \itemize{
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
glicko_run <- function(formula, data, r, rd, sig, weight, idlab, init_r=1500, init_rd=350){
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
  if( missing(sig) ){ 
    data$sig <- 1; sig <- "sig" } 
  if( missing(weight) ){ 
    data$weight <- 1; weight="weight"}
  if( missing(idlab) )   
    idlab <- id
  if( !class(data[[x]]) %in% c("character","factor")) {
    message(paste0("\nvariable '",x,"' is of class ",class(data[[x]])," and will be converted to character"))
    data[[x]] <- as.character(data[[x]])
  }
  if(any(class(data)=="data.frame")) 
    data_list <- split(data[ unique(c(y,id,x, sig, weight,idlab))], data[[ id ]] )
  
  j <- 0
  n <- length(data_list)
  pb <- txtProgressBar(min=0, max=n, width=20, initial=0, style=3)
  models <- list()
  for(i in names(data_list)){
    j <- j + 1;
    player_names <- data_list[[ i ]][[ x ]]
    model      <- glicko( 
      name   = player_names , 
      rank   = data_list[[ i ]][[ y ]], 
      r      = r[player_names ], 
      rd     = rd[player_names ], 
      sig    = data_list[[ i ]][[ sig ]] ,
      weight = data_list[[ i ]][[ weight ]],
      identifier = as.character( data_list[[ i ]][[ idlab ]] ),
      init_r = init_r,
      init_rd = init_rd
    )    
    r [ player_names ] <- model$r[  player_names ]
    rd[ player_names ] <- model$rd[ player_names ]

    models[[ i ]] <- model
    setTxtProgressBar(pb,j)
  }
  
  model_r <- suppressWarnings( data.table::rbindlist( lapply(models,function(x) x[["r_df"]] ) , use.names=T, idcol="id" ) )
  model_P <- suppressWarnings( data.table::rbindlist( lapply(models,function(x) x[["pairs"]] ) , use.names=T, idcol="id" ) )
  identifierp <- unlist( lapply(models,`[[`,"identifierp"), FALSE, FALSE )
  identifier  <- unlist( lapply(models,`[[`,"identifier"), FALSE, FALSE )
  
  # Output, class and attributes ----
  class( model_r[[ id ]] ) <- class( model_P[[ id ]] )  <- class( data[[ id ]] )
  
  
  out <- structure(
    list(final_r  = r,
         final_rd = rd,
         r        = structure( model_r, identifier = identifier),
         pairs    = structure( model_P, identifier = identifierp)),
    class="sport",
    method = "glicko",
    formula = formula
  )
  
  return( out )
}

