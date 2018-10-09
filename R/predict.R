#' Predict rating model
#' 
#' Predict rating model
#' @param object of class rating
#' @param newdata data.frame with data to predict
#' @param ... optional arguments
#' @return probabilities of winning challange by player over his opponent in all provided events.
#' @examples
#' glicko <- glicko_run(rank|id~rider, gpheats[1:16,])
#' predict(glicko,gpheats[17:20,])
#' @export
predict.rating <- function(object,newdata,...){
  if(missing(newdata)) stop("newdata is requested to predict", call.=F)
  method   <- attr(object,"method")
  formula <- attr(object,"formula")
  
  lhs  <- all.vars(update(formula, .~0))
  rhs  <- all.vars(update(formula, 0~.))
  y    <- lhs[1]
  x    <- rhs[1]
  id <- ifelse( length(lhs)==1 , "id", lhs[2])
  if( length(lhs) == 1) newdata$id <- 1
  
  r       <- object$final_r
  rd      <- object$final_rd
  
  is_newdata_consistent( c(lhs,rhs), colnames(newdata) )
  
  if(method == "glicko"){
    weight <- attr(object,"settings")$weight
    sigma    <- attr(object,"settings")$sigma
    idlab  <- attr(object,"settings")$idlab
    init_r <- attr(object,"settings")$init_r
    init_rd<- attr(object,"settings")$init_rd
    kappa  <- attr(object,"settings")$kappa
    
    if( !weight %in% colnames(newdata) ) newdata[[weight]]  <- 1
    if( !sigma    %in% colnames(newdata) ) newdata[[sigma]] <- 0
    
    data_list <- split(newdata[ unique(c(y,id,x, sigma, weight,idlab))], newdata[[ id ]] )
    
    models <- list()
    for(i in names(data_list)){
      player_names <- data_list[[ i ]][[ x ]]
      model        <- glicko( 
        name   = player_names , 
        rank   = data_list[[ i ]][[ y ]], 
        r      = r[player_names ], 
        rd     = rd[player_names ], 
        sigma  = data_list[[ i ]][[ sigma ]] ,
        weight = data_list[[ i ]][[ weight ]],
        kappa  = kappa,
        identifier = as.character( data_list[[ i ]][[ idlab ]] ),
        init_r  = init_r,
        init_rd = init_rd
      )    
      models[[ i ]] <- model   
    }
  }
  
  if(method == "glicko2"){
    sigma    <- object$final_sigma
    init_r <- attr(object,"settings")$init_r
    init_rd<- attr(object,"settings")$init_rd
    weight <- attr(object,"settings")$weight
    tau    <- attr(object,"settings")$tau
    kappa  <- attr(object,"settings")$kappa
    idlab  <- attr(object,"settings")$idlab
    
    if( !weight %in% colnames(newdata) ) newdata[[weight]] <- 1
    data_list <- split(newdata[ unique(c(y,id,x, weight,idlab))], newdata[[ id ]] )
    
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
        kappa    = kappa,
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
    sigma    <- attr(object,"settings")$sigma
    beta   <- attr(object,"settings")$beta
    gamma  <- attr(object,"settings")$gamma
    kappa  <- attr(object,"settings")$kappa
    idlab  <- attr(object,"settings")$idlab
    init_r <- attr(object,"settings")$init_r
    init_rd<- attr(object,"settings")$init_rd
    
    if( !weight %in% colnames(newdata) ) newdata[[weight]] <- 1
    if( !sigma    %in% colnames(newdata) ) newdata[[sigma]]<- 0
    
    data_list <- split(newdata[ unique(c(y,id,x, sigma, weight,idlab))], newdata[[ id ]] )
    
    models <- list()
    for(i in names(data_list)){
      team_name <- data_list[[ i ]][[ x ]]
      model     <- bbt( 
        team_name, 
        rank    = data_list[[ i ]][[ y ]], 
        r       = r[ team_name,,drop=FALSE], 
        rd      = rd[ team_name,,drop=FALSE],
        sigma     = data_list[[ i ]][[ sigma ]],
        weight  = data_list[[ i ]][[ weight ]],
        beta = beta,
        gamma = gamma,
        kappa    = kappa,
        identifier = as.character( data_list[[ i ]][[ idlab ]] ),
        init_r = init_r,
        init_rd = init_rd
      )
      models[[ i ]] <- model
    }
  }
  
  if(method == "dbl"){
    all_params <- allLevelsList(formula, newdata)
    lhs  <- all.vars(update(formula, .~0))
    rhs  <- all.vars(update(formula, 0~.))
    y    <- lhs[1]
    
    weight <- attr(object,"settings")$weight
    beta   <- attr(object,"settings")$beta
    kappa  <- attr(object,"settings")$kappa
    idlab  <- attr(object,"settings")$idlab
    init_r <- attr(object,"settings")$init_r
    init_rd<- attr(object,"settings")$init_rd
    
    if( !weight %in% colnames(newdata) ) newdata[[weight]] <- 1
    if( !beta    %in% colnames(newdata) )newdata[[beta]]  <- 1
    
    data_list <- split( newdata[ c(rhs, beta, weight,idlab) ], newdata[[ id ]] )   
    unique_id  <- unique(newdata[[ id ]]) 
    rank_list  <- split( newdata[[ lhs[1] ]] , newdata[[ id ]])
    rider_list <- split( newdata[[ rhs[1] ]] , newdata[[ id ]])
    
    models <- list()
    for(i in names(data_list)){
      terms <- createTermMatrix( formula, data_list[[ i ]][ rhs ] ) 
      model <- dbl(
        rider_list[[ i ]],
        rank    = rank_list[[ i ]],
        X       = as.matrix(terms),
        R       = r[ colnames(terms) ], 
        RD      = rd[ colnames(terms) ],
        beta    = data_list[[ i ]][[ beta ]],
        weight  = data_list[[ i ]][[ weight ]],
        kappa    = kappa,
        identifier = as.character( data_list[[ i ]][[ idlab ]] )
      )
  
      models[[ i ]] <- model
    }
    
    
  }
  
  model_P <- suppressWarnings( data.table::rbindlist( lapply(models,function(x) x[["pairs"]] ) , use.names=T, idcol="id" ) )
  class( model_P[[ id ]] ) <- class( newdata[[ id ]] ) 
  
  return(model_P[,1:4])
}
