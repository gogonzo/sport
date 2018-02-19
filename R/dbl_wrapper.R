#' Glicko rating algorithm
#' 
#' Glicko rating algorithm
#' Wrapper arround `glicko` update algorithm. Wrapper allows user to simplify calculation providing only data and initial parameters assumptions
#' @param data data.frame which contains three columns: #' \enumerate{
#'   \item id event identifier
#'   \item team_name label of event participant -  name corresponding with parameters names
#'   \item rank position of team_name in event
#'   \item time additional parameter increasing variance of participant rating related with time. 
#' }  
#' @param r named vector of initial rating estimates. In there is no assumption, initial ratings should be r=1500. Names of vector should correspond with team_name label. 
#' @param  rd named vector of initial rating deviation estimates. In there is no assumption, initial ratings should be r=300 Names of vector should correspond with team_name label.
#' @export
glicko_run <- function(formula, data, r, rd, time){
  y  <- all.vars(formula)[1]
  id <- all.vars(formula)[2]
  x  <- all.vars(formula)[-(1:2)]
  
  if( missing(r) ){
    team_names <- unique(data[[x]])
    r <- setNames( rep(1500, length(team_names)), team_names )
    rd<- setNames( rep(300,  length(team_names)), team_names )
  }
  if( missing(time) ){
    data$time <- 0
    time <- "time"
  } 
  
  if(any(class(data)=="data.frame"))
    data <- split( data, data[[ id ]] )  
  
  
  
  glicko_list <- list()
  for(i in 1:length(data)){
    team_names <- data[[ i ]][[ x ]]
    glicko <- glicko( 
      team_names , 
      rank = data[[ i ]][[ y ]], 
      r = r[team_names ], 
      rd = rd[team_names ], 
      time = data[[ i ]][[ time ]] 
    )    
    r [ team_names ] <- glicko$r[  team_names ]
    rd[ team_names ] <- glicko$rd[ team_names ]
    
    glicko_list[[ i ]] <- glicko
    
  }
  return(glicko_list)
}


#' Glicko2 rating algorithm
#' 
#' Glicko2 rating algorithm
#' Wrapper arround `glicko2` update algorithm. Wrapper allows user to simplify calculation providing only data and initial parameters assumptions
#' @param data data.frame which contains three columns: #' \enumerate{
#'   \item id event identifier
#'   \item team_name label of event participant -  name corresponding with parameters names
#'   \item rank position of team_name in event
#'   \item time additional parameter increasing variance of participant rating related with time. 
#' }  
#' @param r named vector of initial rating estimates. In there is no assumption, initial ratings should be r=1500. Names of vector should correspond with team_name label. 
#' @param  rd named vector of initial rating deviation estimates. In there is no assumption, initial ratings should be r=300. Names of vector should correspond with team_name label.
#' @param  sig named vector of rating volatile. In there is no assumption, initial ratings should be sig=0.5. Names of vector should correspond with team_name label.
#' @export

glicko2_run <- function(formula, data, r, rd,sig, time){
  y  <- all.vars(formula)[1]
  id <- all.vars(formula)[2]
  x  <- all.vars(formula)[3]
  
  if( missing(r) ){
    team_names <- unique(data[[x]])
    r   <- setNames( rep(1500, length(team_names)), team_names )
    rd   <- setNames( rep(300,  length(team_names)), team_names )
    sig  <- rep(0.05, length(team_names)) %>% setNames(team_names)
  }
  if( missing(time) ){
    data$time <- 0
    time <- "time"
  } 
  
  if(any(class(data)=="data.frame"))
    data <- split( data, data[[ id ]] )  
  
  glicko_list <- list()
  for(i in 1:length(data)){
    team_names <- data[[ i ]][[ x ]]
    
    glicko <- glicko2( 
      team_names , 
      rank = data[[ i ]][[ y ]], 
      r    = r[ team_names ] ,  
      rd   = rd[ team_names ] , 
      sig  = sig[ team_names ] , 
      time = data[[ i ]][[ time ]]
    )  
    r [ team_names ] <- glicko$r[  team_names ]
    rd[ team_names ] <- glicko$rd[ team_names ]
    sig[ team_names ] <- glicko$sig[ team_names ]
    
    glicko_list[[ i ]] <- glicko
    
  }
  return(glicko_list)
}
#' BBT rating algorithm
#' 
#' BBT rating algorithm
#' Wrapper arround `bbt` update algorithm. Wrapper allows user to simplify calculation providing only data and initial parameters assumptions
#' @param data data.frame which contains three columns: #' \enumerate{
#'   \item id event identifier
#'   \item team_name label of event participant -  name corresponding with parameters names
#'   \item rank position of team_name in event
#' }  
#' @param r named vector of initial rating estimates. In there is no assumption, initial ratings should be r=25 Names of vector should correspond with team_name label. 
#' @param  rd named vector of initial rating deviation estimates. In there is no assumption, initial ratings should be r=25/3 Names of vector should correspond with team_name label.
#' @export


bbt_run <- function(data, r,rd){
  if(missing(r)){
    team_names <- unique(data$team_name)
    r <- setNames( rep(25, length(team_names)), team_names )
    rd<- setNames( rep(25/3,  length(team_names)), team_names )
  }
  if(any(class(data)=="data.frame")) data <- split(data, data$id) 
  
  bbt_list <- list()
  for(i in 1:length(data)){
    
    team_name <- data[[ i ]][["team_name"]]
    ranks  <- data[[ i ]][["rank"]]
    
    bbt   <- bbt( 
      team_name, 
      rank = ranks, 
      r  = r[team_name,,drop=FALSE], 
      rd = rd[team_name,,drop=FALSE]
    )
    
    r [ team_name, ] <- bbt$r[  team_name, ]
    rd[ team_name, ] <- bbt$rd[ team_name, ]
    
    bbt_list[[ i ]] <- bbt
  }
  return(bbt_list)
}

#' BDL rating algorithm
#' 
#' BDL rating algorithm
#' Wrapper arround `bdl` update algorithm. Wrapper allows user to simplify calculation providing only data and initial parameters assumptions
#' @param data data.frame which contains three columns: #' \enumerate{
#'   \item id event identifier
#'   \item team_name label of event participant -  name corresponding with parameters names
#'   \item rank position of team_name in event
#' }  
#' @param R named vector of initial rating estimates. In there is no assumption, initial ratings should be r=25 Names of vector should correspond with team_name label. 
#' @param  RD named vector of initial rating deviation estimates. In there is no assumption, initial ratings should be r=25/3 Names of vector should correspond with team_name label.
#' @export

bdl_run <- function(data,R,RD){
  
  X   <- data[,!colnames(data) %in% c("rank","id"), drop=FALSE]  
  
  bdl_list <- list()
  if(all(colnames(X)=="team_name")){
    ranks_list  <- split(data$rank,as.factor(data$id))
    riders_list <- split(data$team_name,as.factor(data$id))
    ddl_list <- list()
    for(i in 1:length(riders_list)){
      
      names  <- riders_list[[ i ]]
      ranks  <- ranks_list[[ i ]]
      
      ddl <- ddl( 
        names, 
        rank = ranks,  
        R  = R[names,,drop=FALSE], 
        X  = matrix(1 , nrow=length(names)),
        RD = RD[names,,drop=FALSE]
      )
      
      R[ names, ] <- ddl$R
      RD[ names, ] <- ddl$RD
      
      ddl_list[[ i ]] <- ddl
    }
    
    
  } else {
    map <- mapParams( X )  
    idx_list <- split(1:nrow(data), data$id)
    
    map_list <- lapply(idx_list, function(x) map[x,,drop=FALSE])
    X_list   <- lapply(idx_list, function(x) X[x,,drop=FALSE])
    ranks_list <- lapply(idx_list, function(x) data$rank[x])
    names_list <-lapply(idx_list, function(x) data$team_name[x])
    
    for(i in 1:length(map_list)){
      
      map_i   <- map_list[[ i ]]
      X_i  <- getX( X_list[[ i ]] )
      R_i  <- apply(map_i,2, function(x) R[x])
      RD_i <- apply(map_i,2, function(x) RD[x])
      R_i[is.na(R_i)] <- 0
      RD_i[is.na(RD_i)] <- 0
      
      bdl   <- bdl( 
        names_list[[i]], 
        rank = ranks_list[[ i ]], 
        R  = R_i, 
        X = X_i,
        RD = RD_i,
        map = map_i
      )
      
      bdl$r_update <- bdl$r_update[ names(bdl$r_update) != "" ]
      bdl$rd_update <- bdl$rd_update[ names(bdl$rd_update) != "" ]
      
      R[ names(bdl$r_update) ]   <- bdl$r_update
      RD[ names(bdl$rd_update) ] <- bdl$rd_update
      
      bdl_list[[ i ]] <- bdl
    }
    
    
  }
  
  return(bdl_list)
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