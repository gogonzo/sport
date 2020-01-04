#' @importFrom stats terms

is_formula_missing <- function(formula) {
  if (length(formula) == 0) {
    stop("Formula is not specified", call. = FALSE)
  } else if (!inherits(formula, "formula")) {
    stop("Formula incorrectly specified", call. = FALSE)
  }
  
  invisible(TRUE)
}

is_lhs_valid <- function(formula, data) {
  if (length(all.vars(update(formula, . ~ 0))) == 1) {
    warning("LHS of formula doesn't contain `| id` element. 
            It will be assummed that all belongs to the same event id",
            call. = F)
    
  } else if (length(all.vars(update(formula, . ~ 0))) == 2 &
             !grepl("[|]", format(update(formula, . ~ 0)))) {
    stop("LHS of formula must be seperated by `|` operator eg. `rank | id ~ .`", 
         call. = FALSE)
  } else if (length(all.vars(update(formula, . ~ 0))) > 2) {
    stop("LHS must contain 1 or 2 variables", call. = FALSE)
  }
  
  x <- all.vars(update(formula, . ~ 0))
  vars <- trimws(unlist(strsplit(x, "[+|:*]")))
  missing_vars <- setdiff(vars, colnames(data))
  if (length(missing_vars) > 0) {
    stop(sprintf("Variable(s) %s specified in formula are not present in data",
                 paste(missing_vars, collapse = ", ")))
  }
 
  invisible(TRUE) 
}

is_team_term_valid <- function(formula, single) {
  rhs_terms <- attr(terms(update(formula, 0 ~ .)), "term.labels")
  team_term <- grep("player\\(.+\\)", rhs_terms, value = TRUE)
 
  if (length(team_term) > 1) {
    stop("Only one player(...) term is allowed with one or two variables.", 
        call. = FALSE)
    
  } else if (!any(grepl("player\\(.+\\)$", rhs_terms))) {
    stop("Formula requires specifying player(...) term.", 
         call. = FALSE)
    
  } else if (single && any(grepl("player\\([^)]+[|][^)]+\\)", rhs_terms))) {
    stop("This algorithm doesn't allow nesting players in teams.
        Formula should not contain player(...) term function with two variables. 
        Please specify only one variable inside of the player(...).",
        call. = FALSE)
  }
  
  
  team_term_vars <- gsub("player\\(|\\)", "", team_term)
  team_term_vars <- trimws(unlist(strsplit(x = team_term_vars, split =  "[|]")))
  
  if (!length(team_term_vars) %in% c(1, 2) &&
      all(team_term_vars != "")) {
    stop("Only one or two variables are allowed within player(...) term function: 
         Please specify as one of following:
         * player(player_var | team_var)
         * player(team_var)",
         call. = FALSE)
  }
  
  invisible(TRUE)
}

is_rhs_valid <- function(formula, data, only_team_term = FALSE, single = FALSE) {
  rhs_terms <- attr(terms(update(formula, 0 ~ .)), "term.labels")
  if (only_team_term && length(rhs_terms) > 1) {
    stop("This formula requires only one RHS term which is player(...) function.")
  }
 
  is_team_term_valid(formula, single)
  rhs_terms <- grep("player\\(", rhs_terms, value = TRUE, invert = TRUE)

  are_variables_in_dataset(
    union(unique(unlist(strsplit(rhs_terms, "[*:+|]+"))), 
          extract_team_terms(formula)),
    data
  )
  
  return(invisible(TRUE))
}

get_rank_name <- function(formula) {
  lhs <- all.vars(update(formula, . ~ 0))
  rank <- lhs[1]
  return(rank)
}

get_id_name <- function(formula) {
  lhs <- all.vars(update(formula, . ~ 0))
  id <- if (length(lhs) == 2) {
    lhs[2]    
  } else {
    warning("id variable is missing in formula.
            It will be assumed that all rows belongs to the same event id.")
    
    character(0)
  }

  return(id)
}

extract_team_terms <- function(formula) {
  is_team_term_valid(formula = formula, single = FALSE)
  rhs_terms <- attr(terms(update(formula, 0 ~ .)), "term.labels")
  team_terms <- grep("player\\(", rhs_terms, value = TRUE)
  
  x <- gsub("player\\(([^)]*)\\)", "\\1", team_terms, perl = TRUE) 
  vars <- trimws(unlist(strsplit(x, "[+|:*]")))
  
  return(vars)
}

get_team_name <- function(formula) {
  
  terms <- extract_team_terms(formula)
  
  if (length(terms) == 2) {
    terms[2]
  } else {
    terms[1]
  }
}

get_player_name <- function(formula) {
  terms <- extract_team_terms(formula)
  terms[1]
}

get_type <- function(x) {
  if (is.character(x) || is.factor(x)) {
    "character"
  } else if (is.numeric(x)) {
    "numeric"
  }
}

get_terms <- function(data, formula) {
  trm <- unlist(strsplit(attr(terms(formula), "term.labels"), "[ ][+][ ]+"))
  team_term_idx <- grep("player\\(", trm)
  if (length(team_term_idx) > 0) {
    x <- gsub("player\\(([^)]*)\\)", 
              "\\1", 
              trm[team_term_idx], 
              perl = TRUE)
    team_vars <- trimws(unlist(strsplit(x, "[+|:*]")))
    
    trm <- c(
      team_vars,
      trm[-team_term_idx]
    ) 
  }
  
  term_vars <- strsplit(trm, ":")
  
  are_variables_in_dataset(unlist(term_vars), data)
  
  classes <- lapply(data, get_type)
  term_vars <- lapply(term_vars, gsub, pattern = "([a-zA-Z]+\\()|(\\))", replacement = "")
  
  term_vars <- lapply(term_vars, function(x) unlist(classes[x]))
  term_vars <- lapply(term_vars, function(x) x[order(!x %in% c("character"))])
  
  return(term_vars)
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
             Please combine variables manualy and include them in formula.",
             call. = FALSE)
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
             Please combine variables manualy and include them in formula.",
             call. = FALSE)
      }            
    })  )
  
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
  
  colnames(df) <- names
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
           Please combine variables manualy and include them in formula.",
           call. = FALSE)
    }            
  }, FUN.VALUE = character(1))
  
  return(out)
}