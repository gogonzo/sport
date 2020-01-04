#' @importFrom stats setNames terms update 

is_data_provided <- function(data) {
  if (missing(data)) stop("Data is not provided", call. = FALSE)
}

is_newdata_consistent <- function(vars, newnames) {
  if (!all(vars %in% newnames)) {
    stop(
      paste0(
        "Variables (",
        paste(setdiff(vars, newnames), collapse = ","),
        ") are not present in newdata. Provide colnames identical to specified in model formula"
      ),
      call. = FALSE
    )
  }
}

are_variables_in_dataset <- function(vars, data) {
  missing_vars <- setdiff(vars, colnames(data))
  
  if (length(missing_vars) > 0) {
    stop(sprintf(
      "Variable(s) %s specified in formula not present in data",
      paste(missing_vars, collapse = ", ")),
      call. = FALSE)
    
  }
  
  return(invisible(TRUE))
}
check_numeric_argument <- function(x, var_name, min = -Inf, max = Inf) {
  if (!is.numeric(x)) {
    stop(sprintf("Variable %s should be of type numeric.", var_name),
         call. = FALSE
    )
  } else if (any(!is.finite(x))) {
    stop(sprintf("Variable %s contains non-finite values. All elements should be finite.", var_name),
         call. = FALSE
    )
  } else if (any(x < min | x > max)) {
    stop(sprintf("All values in variable %s should be in range [%s, %s]", var_name, min, max),
         call. = FALSE
    )
  }
}

check_integer_argument <- function(x, var_name, min = -Inf, max = Inf) {
  if (!is.integer(x)) {
    stop(sprintf("Variable %s should be of type integer.", var_name),
         call. = FALSE
    )
  } else if (any(!is.finite(x))) {
    stop(sprintf("Variable %s contains non-finite values. All elements should be finite.", var_name),
         call. = FALSE
    )
  } else if (any(x < min | x > max)) {
    stop(sprintf("All values in variable %s should be in range [%s, %s]", var_name, min, max),
         call. = FALSE
    )
  }
}

check_string_argument <- function(x, var_name) {
  if (!is.character(x)) {
    stop(sprintf("Variable %s should be of type character.", var_name),
         call. = FALSE
    )
  } else if (any(is.na(x))) {
    stop(sprintf("Variable %s contains non-finite values. All elements should be finite.", var_name),
         call. = FALSE
    )
  }
}

check_single_argument <- function(x, var_name, min = -Inf, max = Inf) {
  if (length(x) > 1) {
    stop(sprintf("variable %s should be a single value", var_name),
         call. = FALSE
    )
  } else if (is.numeric(x) && x < min) {
    stop(sprintf("variable %s should be greater than %s", var_name, min),
         call. = FALSE
    )
  } else if (is.numeric(x) && x > max) {
    stop(sprintf("variable %s should be lower than %s", var_name, max),
         call. = FALSE
    )
  }
}

init_check_r <- function(r, init_r, unique_names, player) {
  if (length(r) == 0) {
    if (!is.finite(init_r)) {
      stop("init_r should be a finite number", call. = FALSE)
    }
    r <- setNames(rep(init_r, length(unique_names)), unique_names)
  } else if (any(duplicated(names(r)))) {
    stop("All names in r should be unique. Duplicated names not allowed.", call. = FALSE)
    
  } else if (any(!unique_names %in% names(r))) {
    warning(
      sprintf("Some names in %s doesn't have a match in r. 
              Missing parameters will be added with init_r = %s", 
              player, init_r
      ),
      call. = FALSE
    )
    
    r2 <- setNames(rep(init_r, length(unique_names)), unique_names)
    if (any(names(r) %in% unique_names)) {
      r2[names(r)] <- r      
    }

    r <- r2
    
  } else if (any(is.na(r))) {
    stop("All values in r should be a finite number. NA's not allowed.",
         call. = FALSE
    )
  }
  
  return(r)
}

init_check_rd <- function(rd, init_rd, unique_names, player) {
  if (length(rd) == 0) {
    if (!is.finite(init_rd) || init_rd <= 0) {
      stop("init_rd value should be positive", call. = FALSE)
    }
    rd <- setNames(rep(init_rd, length(unique_names)), unique_names)

  } else if (any(duplicated(names(rd)))) {
    stop("All names in rd should be unique. Duplicated names not allowed.", call. = FALSE)
    
  } else if (any(!unique_names %in% names(rd))) {
    warning(
      sprintf("Some names in %s doesn't have a match in rd. 
              Missing parameters will be added with init_rd = %s", 
              player, init_rd
      ),
      call. = FALSE
    )
    
    rd2 <- setNames(rep(init_rd, length(unique_names)), unique_names)
    rd2[names(rd)] <- rd
    rd <- rd2
    
  } else if (any(is.na(rd))) {
    stop("All values in rd should be a finite number. NA's not allowed.",
         call. = FALSE
    )
  } else if (any(rd < 0)) {
    stop("All values in rd should be positive",
         call. = FALSE
    )
  }
  
  return(rd)
}

init_check_sigma <- function(sigma, init_sigma, unique_names, player, method) {
  if (method != "glicko2") {
    return(numeric(0))
  } else   if (length(sigma) == 0) {
    if (!is.finite(init_sigma) || init_sigma <= 0) {
      stop("init_sigma value should be positive", call. = FALSE)
    }
    sigma <- setNames(rep(init_sigma, length(unique_names)), unique_names)
    
  } else if (any(duplicated(names(sigma)))) {
    stop("All names in sigma should be unique. Duplicated names not allowed.", call. = FALSE)
    
  } else if (any(!unique_names %in% names(sigma))) {
    warning(
      sprintf("Some names in %s doesn't have a match in sigma. 
              Missing parameters will be added with init_sigma = %s", 
              player, init_sigma
      ),
      call. = FALSE
    )
    
    sigma2 <- setNames(rep(init_sigma, length(unique_names)), unique_names)
    sigma2[names(sigma)] <- sigma
    sigma <- sigma2
    
  } else if (any(is.na(sigma))) {
    stop("All values in sigma should be a finite number. NA's not allowed.",
         call. = FALSE
    )
  } else if (any(sigma < 0)) {
    stop("All values in sigma should be positive",
         call. = FALSE
    )
  }
  
  return(sigma)
}

check_equal_names <- function(x, y) {
  xname <- deparse(substitute(x))
  yname <- deparse(substitute(y))
  
  diff1 <- setdiff(names(x), names(y))
  if (length(diff1) > 0) {
    stop(sprintf("Names of %s doesn't match %s names.", xname, yname),
         call. = FALSE)
  }
  
  diff2 <- setdiff(names(y), names(x))
  if (length(diff2) > 0) {
    stop(sprintf("Names of %s doesn't match %s names.", yname, xname),
         call. = FALSE)
  }
  
  return(invisible(TRUE))
}

initialize_vec <- function(var, data, argname, min = -Inf, max = Inf) {
  if (length(var) == 0) {
    var_vec <- rep(1.0, nrow(data))
    
  } else if (is.character(var) && length(var) == 1) {
    if (!var %in% colnames(data)) {
      stop(sprintf("variable %s specified in %s is not present in data",
                   var, argname))
    }
    var_vec <- data[[var]] # 1/n_it
    check_numeric_argument(var_vec, var, min = min, max = max)
    
  } else if (is.numeric(var) && length(var) == 1) {
    check_numeric_argument(var, argname, min = min, max = max)
    var_vec <- rep(var, nrow(data))
    
  } else {
    stop(sprintf("%s should be variable name from `data` argument or 
                 single numeric value.", argname))
  }
  
  return(var_vec)
}

