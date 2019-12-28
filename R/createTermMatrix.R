get_type <- function(x) {
  if (is.character(x) || is.factor(x)) {
    "character"
  } else if (is.numeric(x)) {
    "numeric"
  }
}

get_terms <- function(data, formula) {
  classes <- lapply(data, get_type)
  trm <- unlist(strsplit(attr(terms(formula), "term.labels"), "[ ][+][ ]+"))
  trm <- strsplit(trm, ":")
  trm <- lapply(trm, gsub, pattern = "([a-zA-Z]+\\()|(\\))", replacement = "")
  
  vars <- lapply(trm, function(x) unlist(classes[x]))
  vars <- lapply(vars, function(x) x[order(!x %in% c("character"))])
  idx_null <- which(vapply(vars, is.null, logical(1)))
  if (length(idx_null) > 0) {
    missing_vars <- unlist(trm[idx_null])
    stop(sprintf("Variable(s) %s specified in formula are missing in data",
                 paste(missing_vars, collapse = ", ")))
  }

  return(vars)
}

get_terms_map <- function(data, terms) {
  df <- as.data.frame(
    lapply(terms, function(term) {
      if (length(term) == 1) {
        if (term == "character") {
          sprintf("%s=%s", names(term), data[[names(term)]])          
        } else {
          rep(names(term), times = nrow(data))
        }

      } else if (length(term) == 2) {
        if (all(term == "character")) {
          sprintf(
            "%s:%s",
            sprintf("%s=%s", names(term)[1], data[[names(term)[1]]]),
            sprintf("%s=%s", names(term)[2], data[[names(term)[2]]])
          )
        } else if (term[1] == "character" && term[2] == "numeric") {
          sprintf(
            "%s:%s",
            sprintf("%s=%s", names(term)[1], data[[names(term)[1]]]),
            names(term)[2]
          )
        } else if (all(term == "numeric")) {
          sprintf("%s*%s", names(term)[1], names(term)[2])
        }
      } else {
        stop("Only two-variable interactions are possible. 
             Please combine variables manualy and include them in formula.")
      }            
    })
  )

  names <- vapply(terms, function(term) {
    if (length(term) == 1) {
      names(term)
    } else {
      if (any(term == "character")) {
        paste(names(term), collapse = "|")
      } else if (all(term == "numeric")) {
        paste(names(term), collapse = "*")
      }
    }            
  }, FUN.VALUE = character(1))
  
  names(df) <- names
  df <- as.matrix(df)
  
  return(df)
}
get_terms_mat <- function(data, terms) {
  df <- as.data.frame(
    lapply(terms, function(term) {
      if (length(term) == 1) {
        if (term == "character") {
          rep(1, times = nrow(data))       
        } else {
          data[[names(term)]]
        }
        
      } else if (length(term) == 2) {
        if (all(term == "character")) {
          rep(1, times = nrow(data))
        } else if (term[1] == "character" && term[2] == "numeric") {
          data[names(term)[2]]
        } else if (all(term == "numeric")) {
          data[names(term)[1]] * data[names(term)[2]]
        }
      } else {
        stop("Only two-variable interactions are possible. 
             Please combine variables manualy and include them in formula.")
      }            
    })
  )
  
  names <- vapply(terms, function(term) {
    if (length(term) == 1) {
      names(term)
    } else {
      if (any(term == "character")) {
        paste(names(term), collapse = "|")
      } else if (all(term == "numeric")) {
        paste(names(term), collapse = "*")
      }
    }            
  }, FUN.VALUE = character(1))
  
  names(df) <- names
  df <- as.matrix(df)
  
  return(df)
}
get_terms_cls <- function(data, terms) {
  out <- vapply(terms, function(term) {
    if (length(term) == 1) {
      if (term == "character") {
        "character"       
      } else {
        "numeric"
      }
      
    } else if (length(term) == 2) {
      if (all(term == "character")) {
        "character"
      } else if (term[1] == "character" && term[2] == "numeric") {
        "character"
      } else if (all(term == "numeric")) {
        "numeric"
      }
    } else {
      stop("Only two-variable interactions are possible. 
           Please combine variables manualy and include them in formula.")
    }            
  }, FUN.VALUE = character(1))
  
  return(out)
}
