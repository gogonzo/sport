#' DBL rating algorithm
#' 
#' DBL rating algorithm
#' Wrapper arround `dbl` update algorithm. Wrapper allows user to simplify calculation providing only data and initial parameters assumptions
#' @param formula formula specifying model. DBL allows multiple variables in formula, also two-way interaction are available. LHS needs `rank|id`, to specify competitors order and event `id`.
#' @param data data.frame which contains columns specified in formula, and optionaly columns defined by `sig`, `weight` or `date`.
#' @param init_r named vector of initial rating estimates. In there is no assumption, initial ratings should be r=25 Names of vector should correspond with team_name label. 
#' @param init_rd named vector of initial rating deviation estimates. In there is no assumption, initial ratings should be r=25/3 Names of vector should correspond with team_name label.
#' @param sig named vector of rating volatile. In there is no assumption, initial ratings should be sig=0.5. Names of vector should correspond with team_name label.
#' @param weight name of column in `data` containing weights. Weights multiplies step update increasing/decreasing step impact on parameters estimates
#' @param date name of column in `data` containing date. Doesn't affect estimation process. If specified, charts displays estimates changes in time in
#' @export

dbl_run <- function(formula, data, r, rd, sig, weight, date){
  if(missing(formula)) stop("Formula is not specified")
  if(missing(data)) stop("Data is not provided")
  if( missing(weight) ){
    data$weight <- 1
    weight      <- "weight"
  } 
  if( missing(sig) ){
    data$sig <- 1
    sig      <- "sig"
  } 
  
  all_params <- sport::allLevelsList(formula, data)
  
  lhs  <- all.vars(update(formula, .~0))
  rhs  <- all.vars(update(formula, 0~.))
  
  if(any(class(data)=="data.frame")) 
    data_list <- split( data[ c(rhs, sig, weight) ], data[[ lhs[2] ]] )   
  unique_id  <- unique(data[[ lhs[2] ]]) 
  rank_list  <- split( data[[ lhs[1] ]] , data[[ lhs[2] ]])
  rider_list <- split( data[[ rhs[1] ]] , data[[ lhs[2] ]])
  
  if(missing(r))
    r  <- setNames(rep(0, length(all_params)), all_params )
  if(missing(rd))
    rd <- setNames(rep(10, length(all_params)), all_params )
  
  model_r <- list()
  model_P  <- list()
  
  for(i in names(data_list)){
    terms <- createTermMatrix( formula, data_list[[ i ]][ rhs ] ) 
    model <- dbl(
      rider_list[[ i ]],
      rank   = rank_list[[ i ]],
      X      = as.matrix(terms),
      R      = r[ colnames(terms) ], 
      RD     = rd[ colnames(terms) ],
      sig     = data_list[[ i ]][[ sig ]],
      weight  = data_list[[ i ]][[ weight ]]
    )
    
    model_r[[ i ]] <- data.frame(
      names  = colnames(terms), 
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
    r       = bind_rows( model_r  , .id = id ),
    pairs   = bind_rows( model_P  , .id = id ),
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