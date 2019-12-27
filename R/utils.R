is_data_provided <- function(data) {
  if(missing(data)) stop("Data is not provided", call. = FALSE)  
}

is_formula_missing <- function(formula) {
  if (length(formula) == 0) {
    stop("Formula is not specified", call. = FALSE) 
  } else if (!inherits(formula, "formula")) {
    stop("Formula incorrectly specified", call. = FALSE)
  }
}

is_lhs_valid <- function(formula) {
  if ( length(all.vars(update(formula, .~0)) )  == 1 ) {
    warning("LHS of formula doesn't contain `| id` element. It will be assummed that all belongs to the same event id", call. = F) 
  } else if (length(all.vars(update(formula, .~0)) ) == 2 & 
             !grepl("[|]",format(update(formula,.~0)))) {
    
    stop("LHS of formula must be seperated by `|` operator eg. `rank | id ~ .`", call. = FALSE) 
  } else if (length(all.vars(update(formula, . ~ 0))) > 2)
    stop("LHS must contain 1 or 2 variables", call. = FALSE)  
}

is_interactions_valid <- function(formula) {
  if (grepl("[-*|]", format(update(formula, 0 ~ .))))
    stop("RHS formula can be composed only using names and `+` or `:` operator eg. ~ name + field:name")
  
}

is_rhs_valid <- function(formula, model) {
  rhs_terms <- attr(terms(update(formula, 0 ~ .)), "term.labels")
  
  if (length(rhs_terms) != 1) {
    stop(sprintf("%s expects only one variable which is `~ pname` or `nest(player | team)`", 
                 model), 
         call. = FALSE)       
  } else if (grepl("team\\(.+\\)", rhs_terms)) {
    nested <- unlist(strsplit(gsub("team\\((.+)\\)", "\\1", rhs_terms), "\\|"))
    if (length(nested) != 2) {
      stop(sprintf("term %s must contain two variables nest(player | team)", rhs_terms))
    } 
  }
}

is_newdata_consistent <- function(vars, newnames) {
  if (!all( vars %in% newnames)) {
    stop(
      paste0(
        "Variables (", 
        paste(setdiff(vars, newnames), collapse = ","),
        ") are not present in newdata. Provide colnames identical to specified in model formula"), call. = FALSE)      
  }
}

check_numeric_argument <- function(x, var_name, min = -Inf, max = Inf) {
  if (!is.numeric(x)) {
    stop(sprintf("Variable %s should be of type numeric.", var_name),
         call. = FALSE)
  } else if (any(!is.finite(x))) {
    stop(sprintf("Variable %s contains non-finite values. All elements should be finite.", var_name),
         call. = FALSE)
  } else if (any(x < min | x > max)) {
    stop(sprintf("All values in variable %s should be in range [%s, %s]", var_name, min, max),
         call. = FALSE)
  }
}

check_integer_argument <- function(x, var_name, min = -Inf, max = Inf) {
  if (!is.integer(x)) {
    stop(sprintf("Variable %s should be of type integer.", var_name),
         call. = FALSE)
  } else if (any(!is.finite(x))) {
    stop(sprintf("Variable %s contains non-finite values. All elements should be finite.", var_name),
         call. = FALSE)
  } else if (any(x < min | x > max)) {
    stop(sprintf("All values in variable %s should be in range [%s, %s]", var_name, min, max),
         call. = FALSE)
  }
}

check_string_argument <- function(x, var_name) {
  if (!is.character(x)) {
    stop(sprintf("Variable %s should be of type character.", var_name),
         call. = FALSE)
  } else if (any(is.na(x))) {
    stop(sprintf("Variable %s contains non-finite values. All elements should be finite.", var_name),
         call. = FALSE)
  }
}

check_single_argument <- function(x, var_name, min = -Inf, max = Inf) {
  if (length(x) > 1) {
    stop(sprintf("variable %s should be a single value", var_name),
         call. = FALSE)
  } else if (is.numeric(x) && x < min) {
    stop(sprintf("variable %s should be greater than %s", var_name, min),
         call. = FALSE)
  }  else if (is.numeric(x) && x > max) {
    stop(sprintf("variable %s should be lower than %s", var_name, max),
         call. = FALSE)
  }
}

init_check_r <- function(r, init_r, unique_names, player) {
  if (length(r) == 0) {
    if (!is.finite(init_r)) {
      stop("init_r should be a finite number", call. = FALSE)
    }
    r <- setNames(rep(init_r, length(unique_names)), unique_names)
  } else if (!setequal(sort(names(r)), sort(unique_names))) {
    stop(sprintf("All names in r should have a name which match %s argument in formula", player),
         call. = FALSE)
  } else if (any(duplicated(names(r)))) {
    stop("All names in r should be unique. Duplicated names not allowed.", call. = FALSE)    
  } else if (any(is.na(r))) {
    stop("All values in r should be a finite number. NA's not allowed.",
         call. = FALSE)
  } else {
    r
  }
}

init_check_rd <- function(rd, init_rd, unique_names, player) {
  if (length(rd) == 0) {
    if (!is.finite(init_rd) || init_rd <= 0) {
      stop("init_rd value should be positive", call. = FALSE)
    }
    rd <- setNames(rep(init_rd, length(unique_names)), unique_names)
  } else if (!setequal(sort(names(rd)), sort(unique_names))) {
    stop(sprintf("All names in rd should have a name which match %s argument in formula", player),
         call. = FALSE)
  } else if (any(duplicated(names(rd)))) {
    stop("All names in rd should be unique. Duplicated names not allowed.", call. = FALSE)    
  } else if (any(is.na(rd))) {
    stop("All values in rd should be a finite number. NA's not allowed.",
         call. = FALSE)    
  } else if (any(rd < 0)) {
    stop("All values in rd should be positive",
         call. = FALSE)
  } else {
    rd
  }
}

init_check_sigma <- function(sigma, init_sigma, unique_names, player, method) {
  if (method != "glicko2") {
    return(numeric(0))
  } else if (length(sigma) == 0) {
    if (!is.finite(init_sigma) || init_sigma <= 0) {
      stop("init_sigma value should be positive", call. = FALSE)
    }
    sigma <- setNames(rep(init_sigma, length(unique_names)), unique_names)
  } else if (!setequal(sort(names(sigma)), sort(unique_names))) {
    stop(sprintf("All names in sigma should have a name which match %s argument in formula", player),
         call. = FALSE)
  } else if (any(duplicated(names(sigma)))) {
    stop("All names in sigma should be unique. Duplicated names not allowed.", call. = FALSE)    
  } else if (any(is.na(sigma))) {
    stop("All values in sigma should be a finite number. NA's not allowed.",
         call. = FALSE)    
  } else if (any(sigma < 0)) {
    stop("All values in sigma should be positive",
         call. = FALSE)
  } else {
    sigma
  }
}
