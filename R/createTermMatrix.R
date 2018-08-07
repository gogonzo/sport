createTermMatrix <- function(formula, data){
  # formula <- Sepal.Length ~ Species + Petal.Length + Petal.Length : Species; data <- iris
  if(missing(formula)) stop("Formula is not specified")
  if(missing(data)) warning("No data provided")
  rhs <- all.vars(update(formula, 0~.)) 
  lhs <- all.vars(update(formula, .~0)) 
  if( length(lhs) != 2) stop("Left hand side formula must contain two variables. Observed and partitioning (id) variable")
  
  vars <- extractTermNames(formula, lapply(data, class))
  
  for(i in vars){
    class_i <- vars[[i]]
    names_i <- names(vars[[i]])
    
    if( length(vars_i) == 1 )  {
      
      if(vars_i %in% c("factor","character")){
        term_i <- factor2Dummy( factor = data[[ names_i ]] )
      } else if(vars %in% c("numeric","integer") ) {
        term_i <- data[[names_i]]
      } else {
        stop("Variables can be only of following classes: factor, character, numeric, integer.")
      }
      
    } else if(length(vars_i) == 2){
      
      
    } else {
      stop("Only two vars interactions are permitted. You can still create column combining values of desired variables.")
    
    
  }
  
  # f_var1 | f_var2 - all possible combination (existing) - names = name(f_var1).f_var1_lvl | name(f_var2).f_var2_lvl
  # n_var  | f_var1 - all factor levels - name - name(n_var) | name(f_var1).f_var1_lvl
  
  var     <- all.vars( formula )[3] %>% gsub("^1[ ]*|[]*")
  levels <- data[[ var ]] %>% unique
  
  
  head( model.matrix(~Species + Sepal.Length,data = iris) )
  }
}
  
extractTermNames <- function(formula, classes){
  trm <- unlist(strsplit( attr(terms(formula),"term.labels"),"[ ][+][ ]+" ))
  trm <- strsplit(trm,":")

  vars <- lapply( trm , function(x) unlist(classes[x] ) )
  vars <- lapply( vars, function(x) x[order(!x %in% c('character','factor'))])
  
  return(vars)
}
  
factor2Dummy <- function(factor){
  levels <- unique( factor )  
  dummy <- matrix( 0 , nrow=length(factor) , ncol=length(levels) )
  colnames(dummy) <- levels
  for(i in 1:length(factor))
    dummy[ i, levels==factor[i] ] <- 1

  return(dummy)
}