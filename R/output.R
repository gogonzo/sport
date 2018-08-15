summary.sport <- function(x){
  if(!"sport" %in% class(x) ) stop("Function summary() needs object of class sport")
  if(!any(c("glicko","glicko2","bbt","dbl") %in% attr(x,"method") )) stop("Function summary() needs object of class sport")
  
  print(x$final_r[1:6])
  
}