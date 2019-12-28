createTermMatrix <- function(formula, data) {
  vars <- extractTermNames(formula, lapply(data, class))
  terms <- NULL

  if (length(vars) == 1 & length(vars[[1]]) == 1) {
    if (vars[[1]] == "factor") {
      return(factor2dummy(factor = as.character(data[[ names(vars[[1]]) ]])))
    } else if (vars[[1]] %in% c("character")) {
      return(factor2dummy(factor = data[[ names(vars[[1]]) ]]))
    } else if (vars[[1]] %in% c("numeric", "integer")) {
      return(data[ names(vars[[1]]) ])
    }
  }

  for (i in 1:length(vars)) {
    class_i <- vars[[ i ]]
    names_i <- names(vars[[ i ]])
    if (any(!class_i %in% c("character", "factor", "numeric", "integer"))) {
      stop("Variables can be only of following classes: factor, character, numeric, integer.")
    }

    if (length(class_i) == 1) {
      if (class_i == "factor") {
        term_i <- factor2dummy(factor = as.character(data[[ names_i ]]))
        colnames(term_i) <- paste(names_i, colnames(term_i), sep = ".")
      } else if (class_i %in% c("character")) {
        term_i <- factor2dummy(factor = data[[ names_i ]])
        colnames(term_i) <- paste(names_i, colnames(term_i), sep = ".")
      } else if (class_i %in% c("numeric", "integer")) {
        term_i <- data[ names_i ]
      }
    } else if (length(class_i) == 2) {
      if (class_i[1] %in% c("factor", "character") & class_i[2] %in% c("numeric", "integer")) {
        factor <- paste(names_i[1], data[[names_i[1]]], sep = ".")
        term_i <- factor2dummy(factor)
        term_i <- term_i * data[[ names_i[[2]] ]]
        colnames(term_i) <- paste0(colnames(term_i), ":", names_i[[2]])
      } else if (all(class_i %in% c("factor", "character"))) {
        factor <- paste(paste(names_i[1], data[[names_i[1]]], sep = "."),
          paste(names_i[2], data[[ names_i[2] ]], sep = "."),
          sep = ":"
        )
        term_i <- factor2dummy(factor)
      } else if (all(class_i %in% c("numeric", "integer"))) {
        term_i <- matrix(data[[ names_i[1] ]] * data[[ names_i[2] ]], ncol = 1)
        colnames(term_i) <- paste0(names_i[1], ":", names_i[2])
      }
    }
    terms <- cbind(terms, term_i)
  }
  return(terms)
}


allLevelsList <- function(formula, data) {
  get_type <- function(x) {
    if (is.character(x) || is.factor(x)) {
      "character"
    } else if (is.numeric(x)) {
      "numeric"
    }
  }
  
  vars <- extractTermNames(formula, lapply(data, get_type))
  params <- NULL

  if (length(vars) == 1 & length(vars[[1]]) == 1) {
    if (vars[[1]] == "factor") {
      return(unique(as.character(data[[ names(vars[[1]]) ]])))
    } else if (vars[[1]] %in% c("character")) {
      return(unique(as.character(data[[ names(vars[[1]]) ]])))
    } else if (vars[[1]] %in% c("numeric", "integer")) {
      return(names(vars[[1]]))
    }
  }



  for (i in 1:length(vars)) {
    class_i <- vars[[ i ]]
    names_i <- names(vars[[ i ]])
    if (any(!class_i %in% c("character", "factor", "numeric", "integer"))) {
      stop("Variables can be only of following classes: factor, character, numeric, integer.")
    }


    if (length(class_i) == 1) {
      if (class_i == "factor") {
        param_i <- unique(as.character(data[[ names_i ]]))
        param_i <- paste(names_i, param_i, sep = ".")
      } else if (class_i %in% c("character")) {
        param_i <- unique(data[[ names_i ]])
        param_i <- paste(names_i, param_i, sep = ".")
      } else if (class_i %in% c("numeric", "integer")) {
        param_i <- names_i
      }
    } else if (length(class_i) == 2) {
      if (class_i[1] %in% c("factor", "character") & class_i[2] %in% c("numeric", "integer")) {
        param_i <- paste(names_i[1], unique(data[[ names_i[1]]]), sep = ".")
        param_i <- paste0(param_i, ":", names_i[[2]])
      } else if (all(class_i %in% c("factor", "character"))) {
        factor <- paste(paste(names_i[1], data[[ names_i[1] ]], sep = "."),
          paste(names_i[2], data[[ names_i[2] ]], sep = "."),
          sep = ":"
        )
        param_i <- unique(factor)
      } else if (all(class_i %in% c("numeric", "integer"))) {
        param_i <- paste(names_i[1], names_i[2], sep = ":")
      }
    }
    params <- c(params, param_i)
  }
  return(params)
}

extractTermNames <- function(formula, classes) {
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


get_terms <- function(data, formula) {
  get_type <- function(x) {
    if (is.character(x) || is.factor(x)) {
      "character"
    } else if (is.numeric(x)) {
      "numeric"
    }
  }
  
  vars <- extractTermNames(formula, lapply(data, get_type))
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
