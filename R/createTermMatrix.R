createTermMatrix <- function(formula, data){
  vars <- extractTermNames(formula, lapply(data, class))
  terms <- NULL
  
  if(length(vars)==1 & length(vars[[1]])==1) {
    if(is.factor(vars[[1]])){
      return( factor2dummy( factor = as.character(data[[ names(vars[[1]]) ]] ) ) )
      
    } else if(is.character(vars[[1]])){
      return( factor2dummy( factor = data[[ names(vars[[1]]) ]] ) )
      
    } else if(is.numeric(vars[[1]])) {
      return( data[ names(vars[[1]]) ] )
    }
  }
  
  
  for(i in 1:length(vars)){
    class_i <- vars[[ i ]]
    names_i <- names( vars[[ i ]] )
    if(is.factor( vars[[ i ]])){
      term_i <- factor2dummy( factor = as.character(data[[ names_i ]] ) )
      colnames(term_i) <- paste(names_i,colnames(term_i),sep=".")
      
    } else if(is.character( vars[[ i ]])){
      term_i <- factor2dummy( factor = data[[ names_i ]] )
      colnames(term_i) <- paste(names_i,colnames(term_i),sep=".")
      
    } else if(is.numeric( vars[[ i ]])) {
      term_i <- data[ names_i ]
    } else {
      stop("Variables can be only of following classes: factor, character, numeric, integer.")
    }
  }
  
  terms <- cbind(terms,term_i)

  return( terms )
}


allLevelsList <- function(formula, data){
  vars <- extractTermNames(formula, lapply(data, class))
  params <- NULL
  
  if(length(vars)==1 & length(vars[[1]])==1)
    if(is.factor(vars[[1]])) {
      return( unique( as.character(data[[ names(vars[[1]]) ]]) ) )
      
    } else if(is.character(vars[[1]])){
      return( unique( as.character(data[[ names(vars[[1]]) ]]) ) )
      
    } else if(is.numeric(vars[[1]])) {
      return( names(vars[[1]]))
    } 
    

  
  for(i in 1:length(vars)){
    class_i <- vars[[ i ]]
    names_i <- names( vars[[ i ]] )

    if(is.factor(class_i)){
      param_i <- unique(as.character(data[[ names_i ]] ) )
      param_i <- paste( names_i , param_i,sep=".")
      
    } else if(is.character(class_i)){
      param_i <- unique( data[[ names_i ]] )
      param_i <- paste( names_i, param_i,sep=".")
      
    } else if(is.numeric(class_i)) {
      param_i <- names_i
    } 
      
    params <- c(params,param_i)
  }
  return(params)
}

extractTermNames <- function(formula, classes) {
  trm <- unlist( strsplit( attr(terms(formula),"term.labels"),"[ ][+][ ]+" ) )
  trm <- strsplit(trm,":")

  vars <- lapply( trm , function(x) unlist(classes[x] ) )
  vars <- lapply( vars, function(x) x[order(!x %in% c('character','factor'))] )
  
  return(vars)
}