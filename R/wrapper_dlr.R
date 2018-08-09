#' @importFrom dplyr bind_rows
NULL

#' BDL rating algorithm
#' 
#' BDL rating algorithm
#' Wrapper arround `bdl` update algorithm. Wrapper allows user to simplify calculation providing only data and initial parameters assumptions
#' @param formula formula specifying model.
#' @param data data.frame which contains three columns: #' \enumerate{
#'   \item id event identifier
#'   \item team_name label of event participant -  name corresponding with parameters names
#'   \item rank position of team_name in event
#' }  
#' @param r named vector of initial rating estimates. In there is no assumption, initial ratings should be r=25 Names of vector should correspond with team_name label. 
#' @param  rd named vector of initial rating deviation estimates. In there is no assumption, initial ratings should be r=25/3 Names of vector should correspond with team_name label.
#' @export

dlr_run <- function(formula, data,r,rd){
  if(missing(formula)) stop("Formula is not specified")
  if( length(all.vars(update(formula, .~0)) ) != 2) stop("Left hand side formula must contain two variables")
  
  y  <- all.vars(formula)[1]
  id <- all.vars(formula)[2]
  x  <- all.vars(formula)[-(1:2)]
  X  <- data[x]  
  
  model_P <- list()
  model_r <- list()
  if( length(x)==1 ){
    # if(! class(X[,1]) %in% c("factor","character")) error_msg <- "first parameter has to be factor or character"
    if(missing(r)){
      names <- unique(X[[ 1 ]])
      r <- matrix( 0, nrow = length(names), dimnames = list(names, NULL) )  
    }
    if( missing(rd) ){
      names <- unique(X[[ 1 ]])
      rd <- matrix( 1, nrow = length(names), dimnames = list(names, NULL) ) 
    }
    if( class(r)!="matrix" ){
      r <- as.matrix(r, ncol=1)
    }
    if( class(rd)!="matrix" ){
      rd <- as.matrix(rd, ncol=1)
    }
    
    ranks_list  <- split( data[[ y ]]   , data[[ id ]])
    X_list      <- split( X[[ 1 ]] , data[[ id ]])
    
    for(i in names(ranks_list)){
      
      names  <- X_list[[ i ]]
      ranks  <- ranks_list[[ i ]]
      
      model <- dlr1( 
        names, 
        rank = ranks,  
        R  = r[ names,,drop=FALSE ], 
        X  = matrix(1 , nrow=length(names)),
        RD = rd[ names,,drop=FALSE ]
      )
      
      r[ names, ] <- model$R
      rd[ names, ] <- model$RD
      
      model_P[[ i ]] <- model$pairs
      model_r[[ i ]] <- data.frame( names = names, r = model$R, rd=model$RD)
    }
  } else {
    map <- mapParams( X )  
    if( missing(r) ){
      names <- unique( c(map) )
      r <- setNames( rep(0, length(names)), names)  
    }
    if(missing(rd)){
      names <- unique( c(map) )
      rd <- setNames( rep(1, length(names)), names) 
    }
    
    
    idx_list   <- split(1:nrow(data), data[[ id ]])
    map_list   <- lapply(idx_list, function(i) map[i,,drop=FALSE])
    X_list     <- lapply(idx_list, function(i) X[i,,drop=FALSE])
    ranks_list <- lapply(idx_list, function(i) data[[y]][i])
    names_list <-lapply(idx_list, function(i) X[[ x[1] ]][i])
    
    for( i in names(X_list) ){
      
      map_i   <- map_list[[ i ]]
      X_i  <- getX( X_list[[ i ]] )
      R_i  <- apply(map_i,2, function(x) r[x])
      RD_i <- apply(map_i,2, function(x) rd[x])
      R_i[is.na(R_i)] <- 0
      RD_i[is.na(RD_i)] <- 1
      
      model   <- dlr( 
        names_list[[i]], 
        rank = ranks_list[[ i ]], 
        R  = R_i, 
        X = X_i,
        RD = RD_i,
        map = map_i
      )
      
      model$r  <- model$r[ names(model$r) != "" ]
      model$rd <- model$rd[ names(model$rd) != "" ]
      
      model$r  <- model$r[ !is.na(model$r) ]
      model$rd <- model$rd[ !is.na(model$rd) ]
      
      r[ names(model$r) ]   <- model$r
      rd[ names(model$rd) ] <- model$rd
      
      model_P[[ i ]] <- model$pairs
      model_r[[ i ]] <- data.frame(names=  names(model$r), r = model$r, rd=model$rd)
    }
  }
  
  model_P <- bind_rows(model_P, .id="id")
  model_r <- bind_rows(model_r, .id="id")
  
  return(
    list(
      pairs = model_P, 
      r = model_r,
      final_r = r,
      final_rd = rd
    )
  )
}


#' DBL rating algorithm
#' 
#' DBL rating algorithm
#' Wrapper arround `dbl` update algorithm. Wrapper allows user to simplify calculation providing only data and initial parameters assumptions
#' @param formula formula specifying model.
#' @param data data.frame which contains three columns: #' \enumerate{
#'   \item id event identifier
#'   \item team_name label of event participant -  name corresponding with parameters names
#'   \item rank position of team_name in event
#' }  
#' @param init_r named vector of initial rating estimates. In there is no assumption, initial ratings should be r=25 Names of vector should correspond with team_name label. 
#' @param init_rd named vector of initial rating deviation estimates. In there is no assumption, initial ratings should be r=25/3 Names of vector should correspond with team_name label.
#' @export
dbl_run <- function(formula, data, r, rd){
  if(missing(formula)) stop("Formula is not specified")
  if(missing(data)) stop("Data is not provided")
  
  all_terms  <- sport::createTermMatrix(formula,data)
  all_params <- sport::allLevelsList(formula, data)
  
  lhs  <- all.vars(update(formula, .~0))
  rhs  <- all.vars(update(formula, 0~.))
  
  id         <- data[[ lhs[2] ]]
  unique_id  <- unique(id) 
  data_list  <- split(data, id)
  rank_list  <- split( data[[ lhs[1] ]] , id)
  rider_list <- split( data[[ rhs[1] ]] , id)
  
  names(data_list)  <- unique_id
  names(rank_list)  <- unique_id
  names(rider_list) <- unique_id
  
  if(missing(r))
    r  <- setNames(rep(0, length(all_params)), all_params )
  if(missing(rd))
    rd <- setNames(rep(1, length(all_params)), all_params )
  
  model_r <- list()
  model_P  <- list()
  
  for(i in id){
    terms <- createTermMatrix( formula, data_list[[ i ]] ) 
    model <- dbl(
      rider_list[[ i ]],
      rank = rank_list[[ i ]],
      X = terms,
      R =   r[ colnames(terms) ], 
      RD = rd[ colnames(terms) ]  
    )
    
    model_r[[ i ]] <- data.frame(
      param  = colnames(terms), 
      init_r = r[ colnames(terms) ],
      init_rd= rd[ colnames(terms) ],
      r      = model$r,
      rd     = model$rd,
      row.names = NULL
    )
    
    model_P[[ i ]] <- model$pairs
    
    r[names(model$r)]  <- model$r
    rd[names(model$rd)] <- model$rd
    
  }
  
  list(
    r       = bind_rows( model_r , .id = "id" ),
    pairs   = bind_rows( model_P  , .id = "id" ),
    final_r = r,
    final_rd = rd
  )
}




getX <- function(X){
  col_classes <- unlist( lapply(X,class) )
  factor_idx <- col_classes %in% c("character","factor")
  X[,factor_idx] <- (!is.na(X[,factor_idx]))*1
  as.matrix(X)
}

mapParams <- function(X){
  H <- matrix("",nrow(X), ncol(X))
  for(j in 1:ncol(X)){
    if(class(X[[j]])=="character"){
      H[,j] <- paste0(colnames(X)[j],": ",X[[j]] )
    } else {
      H[,j] <- colnames(X)[j]
    }
  }
  return(H)
}