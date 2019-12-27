#' @useDynLib sport
#' @importFrom methods is
NULL

if(getRversion() >= "2.15.1")  
  utils::globalVariables(c(".","P", "Y","r","rd","Interval","name","p_win"))