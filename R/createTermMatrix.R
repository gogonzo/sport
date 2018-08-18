createTermMatrix <- function(formula, data){
  vars <- extractTermNames(formula, lapply(data, class))
  terms <- NULL
  
  for(i in 1:length(vars)){
    class_i <- vars[[ i ]]
    names_i <- names( vars[[ i ]] )
    if(any( !class_i %in% c("character","factor","numeric","integer")) )
      stop("Variables can be only of following classes: factor, character, numeric, integer.")
    
    
    if( length(class_i) == 1 )  {
      
      if(class_i == 'factor'){
        term_i <- factor2dummy( factor = as.character(data[[ names_i ]] ) )
        colnames(term_i) <- paste(names_i,colnames(term_i),sep=".")
        
      } else if(class_i %in% c("character")){
        term_i <- factor2dummy( factor = data[[ names_i ]] )
        colnames(term_i) <- paste(names_i,colnames(term_i),sep=".")
        
      } else if(class_i %in% c("numeric","integer") ) {
        term_i <- data[ names_i ]
      }
      
    } else if( length(class_i) == 2 ){
      
      if( class_i[1] %in% c("factor","character") & class_i[2] %in% c("numeric","integer") ){
        factor <- paste(names_i[1], data[[names_i[1]]], sep=".")        
        term_i <- factor2dummy( factor )
        term_i <- term_i * data[[ names_i[[2]] ]] 
        colnames(term_i) <- paste0( colnames(term_i), ":" , names_i[[2]])
      } else if( all(class_i %in% c("factor","character") ) ){
        factor <- paste( paste(names_i[1], data[[names_i[1]]], sep=".") , 
                         paste(names_i[2], data[[ names_i[2] ]], sep="."), sep=":")
        term_i <- factor2dummy( factor )
      } else if( all(class_i %in% c("numeric","integer") ) ){
        term_i <- matrix(data[[ names_i[1] ]] * data[[ names_i[2] ]], ncol=1)
        colnames(term_i) <- paste0( names_i[1],":",names_i[2])
      } 
    }
    terms <- cbind(terms,term_i)
  }
  return( terms )
}


allLevelsList <- function(formula, data){
  vars <- extractTermNames(formula, lapply(data, class))
  params <- NULL
  
  for(i in 1:length(vars)){
    class_i <- vars[[ i ]]
    names_i <- names( vars[[ i ]] )
    if(any( !class_i %in% c("character","factor","numeric","integer")) )
      stop("Variables can be only of following classes: factor, character, numeric, integer.")
    
      
    if( length(class_i) == 1 )  {
      
      if(class_i == 'factor'){
        param_i <- unique(as.character(data[[ names_i ]] ) )
        param_i <- paste( names_i , param_i,sep=".")
        
      } else if(class_i %in% c("character")){
        param_i <- unique( data[[ names_i ]] )
        param_i <- paste( names_i, param_i,sep=".")
        
      } else if(class_i %in% c("numeric","integer") ) {
        param_i <- names_i
      } 
      
    } else if( length(class_i) == 2 ){
      
      if( class_i[1] %in% c("factor","character") & class_i[2] %in% c("numeric","integer") ){
        param_i <- paste(names_i[1], unique( data[[ names_i[1]]] ), sep=".")
        param_i <- paste0( param_i, ":" , names_i[[2]])
        
      } else if( all(class_i %in% c("factor","character") ) ){
        factor <- paste( paste(names_i[1], data[[ names_i[1] ]], sep=".") , 
                         paste(names_i[2], data[[ names_i[2] ]] , sep="."), sep=":")
        param_i <- unique(factor)
        
      } else if( all(class_i %in% c("numeric","integer") ) ){
        param_i <- paste( names_i[1] , names_i[2], sep=":")
        
      }
      
    }
    params <- c(params,param_i)
  }
  return(params)
}

extractTermNames <- function(formula, classes){
  trm <- unlist( strsplit( attr(terms(formula),"term.labels"),"[ ][+][ ]+" ) )
  trm <- strsplit(trm,":")

  vars <- lapply( trm , function(x) unlist(classes[x] ) )
  vars <- lapply( vars, function(x) x[order(!x %in% c('character','factor'))] )
  
  return(vars)
}