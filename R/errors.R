is_data_provided <- function(data)
  if(missing(data)) stop("Data is not provided", call. = F)

is_formula_missing <- function(formula)
  if(missing(formula)) stop("Formula is not specified",call. = F)  

is_lhs_valid <- function(formula)
  if( !length(all.vars(update(formula, .~0)) )  %in% c(1,2)) 
    stop("Left hand side formula must contain two variables", call. = F)

is_rhs_valid1 <- function(formula, model)
  if( length(all.vars(update(formula, 0~.)) ) != 1) 
    stop(paste(model,"expects only one variable which is ~ name"), call. = F ) 

is_player_class_valid <- function()
  if(class(data[[x]]) != c("character")) 
    warning(paste0("variable ",x," is of class ",class(x),". Will be converted to character"))