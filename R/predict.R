#' Predict sport model
#' 
#' Predict sport model
#' @param object of class sport
#' @param new_data data.frame with data to predict
#' @return probabilities of winning challange by player over his opponent in all provided events.
#' @examples
#'
#' @export
predict.sport <- function(object,new_data,...){
  method   <- attr(object,"method")
  formula <- attr(object,"formula")
  
  lhs  <- all.vars(update(formula, .~0))
  rhs  <- all.vars(update(formula, 0~.))
  y    <- lhs[1]
  x    <- rhs[1]
  id <- ifelse( length(lhs)==1 , "id", lhs[2])
  if( length(lhs) == 1) new_data$id <- 1
  
  r       <- object$final_r
  rd      <- object$final_rd
  
  if(method == "glicko"){
    weight <- attr(object,"settings")$weight
    sig    <- attr(object,"settings")$sig
    idlab  <- attr(object,"settings")$idlab
    init_r <- attr(object,"settings")$init_r
    init_rd<- attr(object,"settings")$init_rd
    
    if( !weight %in% colnames(new_data) ) new_data[[weight]] <- 1
    if( !sig    %in% colnames(new_data) ) new_data[[sig]]    <- 1
    
    data_list <- split(new_data[ unique(c(y,id,x, sig, weight,idlab))], new_data[[ id ]] )
    
    models <- list()
    for(i in names(data_list)){
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
      models[[ i ]] <- model   
    }
  }
  
  if(method == "glicko2"){
    sig    <- object$final_sig
    weight <- attr(object,"settings")$weight
    tau    <- attr(object,"settings")$tau
    idlab  <- attr(object,"settings")$idlab
    init_r <- attr(object,"settings")$init_r
    init_rd<- attr(object,"settings")$init_rd
    
    if( !weight %in% colnames(new_data) ) new_data[[weight]] <- 1
    data_list <- split(new_data[ unique(c(y,id,x, weight,idlab))], new_data[[ id ]] )
    
    models <- list()
    for(i in names(data_list)){
      player_names <- data_list[[ i ]][[ x ]]
      model <- glicko2( 
        player_names , 
        rank   = data_list[[ i ]][[ y ]], 
        r      = r[ player_names ] ,  
        rd     = rd[ player_names ] , 
        sig    = sig[ player_names ] , 
        tau    = tau,
        weight = data_list[[ i ]][[ weight ]],
        identifier = as.character( data_list[[ i ]][[ idlab ]] ), 
        init_r = init_r,
        init_rd = init_rd
      )  
      models[[ i ]] <- model   
    }
  }
  
  if(method == "bbt"){
    r      <- as.matrix(r)
    rd     <- as.matrix(rd)
    weight <- attr(object,"settings")$weight
    sig    <- attr(object,"settings")$sig
    beta   <- attr(object,"settings")$beta
    gamma  <- attr(object,"settings")$gamma
    idlab  <- attr(object,"settings")$idlab
    init_r <- attr(object,"settings")$init_r
    init_rd<- attr(object,"settings")$init_rd
    
    if( !weight %in% colnames(new_data) ) new_data[[weight]] <- 1
    if( !sig    %in% colnames(new_data) ) new_data[[sig]]    <- 1
    
    data_list <- split(new_data[ unique(c(y,id,x, sig, weight,idlab))], new_data[[ id ]] )
    
    models <- list()
    for(i in names(data_list)){
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
      models[[ i ]] <- model
    }
  }
  
  if(method == "dbl"){
    all_params <- allLevelsList(formula, new_data)
    weight <- attr(object,"settings")$weight
    beta   <- attr(object,"settings")$beta
    tau    <- attr(object, "settings")$tau
    idlab  <- attr(object,"settings")$idlab
    init_r <- attr(object,"settings")$init_r
    init_rd<- attr(object,"settings")$init_rd
    
    if( !weight %in% colnames(new_data) ) new_data[[weight]] <- 1
    if( !beta    %in% colnames(new_data) ) new_data[[beta]]  <- 1
    
    data_list <- split( new_data[ c(rhs, beta, weight,idlab) ], new_data[[ id ]] )   
    unique_id  <- unique(new_data[[ id ]]) 
    rank_list  <- split( new_data[[ lhs[1] ]] , new_data[[ id ]])
    rider_list <- split( new_data[[ rhs[1] ]] , new_data[[ id ]])
    
    models <- list()
    for(i in names(data_list)){
      terms <- createTermMatrix( formula, data_list[[ i ]][ rhs ] ) 
      model <- dbl(
        rider_list[[ i ]],
        rank    = rank_list[[ i ]],
        X       = as.matrix(terms),
        R       = r[ colnames(terms) ], 
        RD      = rd[ colnames(terms) ],
        beta     = data_list[[ i ]][[ beta ]],
        weight  = data_list[[ i ]][[ weight ]],
        identifier = as.character( data_list[[ i ]][[ idlab ]] ),
        tau = tau
      )
  
      models[[ i ]] <- model
    }
    
    
  }
  
  model_P <- suppressWarnings( data.table::rbindlist( lapply(models,function(x) x[["pairs"]] ) , use.names=T, idcol="id" ) )
  class( model_P[[ id ]] ) <- class( new_data[[ id ]] ) 
  
  return(model_P[,1:4])
}
