is_data_provided <- function(data)
  if(missing(data)) stop("Data is not provided", call. = F)

is_formula_missing <- function(formula)
  if(missing(formula)) 
    stop("Formula is not specified",call. = F) else if(class(formula)!="formula")
    stop("Formula is not specified",call. = F)

is_lhs_valid <- function(formula)
  if( length(all.vars(update(formula, .~0)) )  == 1 ) {
    warning("LHS of formula doesn't contain `| id` element. It will be assummed that all belongs to the same event id", call. = F) 
  } else if( length(all.vars(update(formula, .~0)) ) == 2 & 
             !grepl("[|]",format(update(formula,.~0)) ) ){
    
    stop("LHS of formula must be seperated by `|` operator eg. `rank | id ~ .`",call.=F) 
  } else if( length(all.vars(update(formula, .~0))) > 2 )
    stop("LHS must contain 1 or 2 variables", call.=F)

is_interactions_valid <- function(formula)
  if( grepl("[-*|]",format(update(formula,0~.)) ) )
    stop("RHS formula can be composed only using names and `+` or `:` operator eg. ~ name + field:name")

is_rhs_valid <- function(formula, model)
  if( length(all.vars(update(formula, 0~.)) ) != 1) 
    stop(paste(model,"expects only one variable which is `~ name`"), call. = F ) 

is_newdata_consistent <- function(vars, newnames)
  if( !all( vars %in% newnames) )
    stop(paste0(
      "Variables (", 
      paste(setdiff(vars,newnames),collapse=","),
      ") are not present in newdata. Provide colnames identical to specified in model formula"), call.=F)