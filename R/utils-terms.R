#' @importFrom stats terms
NULL

#' Stop unless `formula` is a non-empty `formula` object.
#' First gate in `.rating_run()` so we never silently fall through to a
#' confusing downstream error if the caller forgot the model spec.
#' @param formula caller's `formula` argument.
#' @return Invisibly `TRUE`.
#' @noRd
.validate_formula <- function(formula) {
  if (length(formula) == 0) {
    stop("Formula is not specified", call. = FALSE)
  } else if (!inherits(formula, "formula")) {
    stop("Formula incorrectly specified", call. = FALSE)
  }

  invisible(TRUE)
}

#' Validate the left-hand side of a model formula.
#'
#' Accepted shapes:
#' - `rank ~ ...`        -- single var; warns and treats every row as one event
#' - `rank | id ~ ...`   -- two vars separated by `|`
#'
#' Anything else (more than two vars, or two vars without `|`) is an error.
#' Also asserts that all named LHS columns exist in `data`.
#'
#' @param formula model formula.
#' @param data training data.frame.
#' @return Invisibly `TRUE`.
#' @noRd
.validate_lhs <- function(formula, data) {
  if (length(all.vars(update(formula, . ~ 0))) == 1) {
    warning("LHS of formula doesn't contain `| id` element.
            It will be assummed that all belongs to the same event id",
      call. = FALSE
    )
  } else if (
    length(all.vars(update(formula, . ~ 0))) == 2 &&
      !grepl("[|]", format(update(formula, . ~ 0)))
  ) {
    stop("LHS of formula must be seperated by `|` operator eg. `rank | id ~ .`",
      call. = FALSE
    )
  } else if (length(all.vars(update(formula, . ~ 0))) > 2) {
    stop("LHS must contain 1 or 2 variables", call. = FALSE)
  }

  x <- all.vars(update(formula, . ~ 0))
  vars <- trimws(unlist(strsplit(x, "[+|:*]")))
  missing_vars <- setdiff(vars, colnames(data))
  if (length(missing_vars) > 0) {
    stop(sprintf(
      "Variable(s) %s specified in formula are not present in data",
      paste(missing_vars, collapse = ", ")
    ))
  }

  invisible(TRUE)
}

#' Validate the `player(...)` term on the RHS of a model formula.
#'
#' Asserts that exactly one `player(...)` term is present, that it contains
#' one or two variables (`player(team)` or `player(player | team)`), and --
#' when `single = TRUE` -- that the nested `player(player | team)` form is not
#' used (DBL doesn't support player-in-team nesting).
#'
#' @param formula model formula.
#' @param single if `TRUE`, reject the nested `player(player | team)` form.
#' @return Invisibly `TRUE`.
#' @noRd
.validate_team_term <- function(formula, single) {
  rhs_terms <- attr(stats::terms(update(formula, 0 ~ .)), "term.labels")
  team_term <- grep("player\\(.+\\)", rhs_terms, value = TRUE)

  if (length(team_term) > 1) {
    stop("Only one player(...) term is allowed with one or two variables.",
      call. = FALSE
    )
  } else if (!any(grepl("player\\(.+\\)$", rhs_terms))) {
    stop("Formula requires specifying player(...) term.",
      call. = FALSE
    )
  } else if (single && any(grepl("player\\([^)]+[|][^)]+\\)", rhs_terms))) {
    stop("This algorithm doesn't allow nesting players in teams.
        Formula should not contain player(...) term function with two variables.
        Please specify only one variable inside of the player(...).",
      call. = FALSE
    )
  }


  team_term_vars <- gsub("player\\(|\\)", "", team_term)
  team_term_vars <- trimws(unlist(strsplit(x = team_term_vars, split = "[|]")))

  if (!length(team_term_vars) %in% c(1, 2) && all(team_term_vars != "")) {
    stop("Only one or two variables are allowed within player(...) term function:
         Please specify as one of following:
         * player(player_var | team_var)
         * player(team_var)",
      call. = FALSE
    )
  }

  invisible(TRUE)
}

#' Validate the right-hand side of a model formula end-to-end.
#'
#' Combines `.validate_team_term()` with a check that every non-`player(...)`
#' RHS variable exists in `data`. The `only_team_term` switch is used by
#' glicko/glicko2/bbt which forbid extra RHS terms; DBL allows them.
#'
#' @param formula model formula.
#' @param data training data.frame.
#' @param only_team_term if `TRUE`, fail when the RHS contains anything other
#'   than the single `player(...)` term.
#' @param single forwarded to `.validate_team_term()` (DBL passes `TRUE`).
#' @return Invisibly `TRUE`.
#' @noRd
.validate_rhs <- function(formula, data, only_team_term = FALSE, single = FALSE) {
  rhs_terms <- attr(stats::terms(update(formula, 0 ~ .)), "term.labels")
  if (only_team_term && length(rhs_terms) > 1) {
    stop("This formula requires only one RHS term which is player(...) function.")
  }

  .validate_team_term(formula, single)
  rhs_terms <- grep("player\\(", rhs_terms, value = TRUE, invert = TRUE)

  .validate_variables_present(
    union(
      unique(unlist(strsplit(rhs_terms, "[*:+|]+"))),
      .get_team_term_vars(formula)
    ),
    data
  )

  invisible(TRUE)
}

#' Return the name of the LHS rank column (always present, first LHS var).
#' @param formula model formula.
#' @return Length-1 character vector.
#' @noRd
.get_rank_name <- function(formula) {
  lhs <- all.vars(update(formula, . ~ 0))
  rank <- lhs[1]
  rank
}

#' Return the name of the LHS event-id column, or `character(0)` if missing.
#' Warns when missing -- `.rating_run()` then treats every row as the same event.
#' @param formula model formula.
#' @return Length-1 character vector or `character(0)`.
#' @noRd
.get_id_name <- function(formula) {
  lhs <- all.vars(update(formula, . ~ 0))
  id <- if (length(lhs) == 2) {
    lhs[2]
  } else {
    warning("id variable is missing in formula.
            It will be assumed that all rows belongs to the same event id.")

    character(0)
  }

  id
}

#' Extract the variables nested inside the `player(...)` term.
#'
#' For `player(rider)` returns `c("rider")`. For `player(player | team)`
#' returns `c("player", "team")` in source order. Used by `.get_team_var()`,
#' `.get_player_var()`, and the column-renaming step in `.rating_run()`.
#'
#' @param formula model formula.
#' @return Character vector of length 1 or 2.
#' @noRd
.get_team_term_vars <- function(formula) {
  .validate_team_term(formula = formula, single = FALSE)
  rhs_terms <- attr(stats::terms(update(formula, 0 ~ .)), "term.labels")
  team_terms <- grep("player\\(", rhs_terms, value = TRUE)

  x <- gsub("player\\(([^)]*)\\)", "\\1", team_terms, perl = TRUE)
  vars <- trimws(unlist(strsplit(x, "[+|:*]")))

  vars
}

#' Return the column name that represents the *team* in the formula.
#' For `player(team)` that's `"team"`; for `player(player | team)` it's `"team"`
#' (the second var). Always defined.
#' @param formula model formula.
#' @return Length-1 character vector.
#' @noRd
.get_team_var <- function(formula) {
  terms <- .get_team_term_vars(formula)

  if (length(terms) == 2) {
    terms[2]
  } else {
    terms[1]
  }
}

#' Return the column name that represents the individual *player* in the
#' formula. For the nested form `player(player | team)` that's `"player"`;
#' for the flat form `player(team)` it's the same as the team var (so the
#' caller can detect the flat case via `length(player) == 0`-style checks
#' upstream -- currently the flat form simply yields the team var).
#' @param formula model formula.
#' @return Length-1 character vector.
#' @noRd
.get_player_var <- function(formula) {
  terms <- .get_team_term_vars(formula)
  terms[1]
}

#' Pull the per-row event vectors out of `data`.
#'
#' Used by both player-rating methods and DBL: extracts the rank/id/team
#' columns named by the formula's LHS and team term, coerces them to the
#' types the C++ layer expects, and returns the row-aligned vectors plus
#' the unique-event index used to drive the event loop.
#'
#' @param formula model formula.
#' @param data training data.frame.
#' @return Named list with `rank`, `id`, `team` (column names), `rank_vec`,
#'   `id_vec`, `team_vec` (row-aligned vectors), and `unique_id`.
#' @noRd
.extract_event_vars <- function(formula, data) {
  rank <- .get_rank_name(formula)
  rank_vec <- as.integer(data[[rank]])

  id <- .get_id_name(formula)
  id_vec <- if (length(id) == 0) rep(1L, nrow(data)) else as.integer(data[[id]])
  unique_id <- unique(id_vec)

  team <- .get_team_var(formula)
  team_vec <- as.character(data[[team]])

  list(
    rank = rank, id = id, team = team,
    rank_vec = rank_vec, id_vec = id_vec, team_vec = team_vec,
    unique_id = unique_id
  )
}

#' Coarse type classifier used while building the DBL design matrix.
#' Collapses R types into the two buckets DBL cares about: `"character"`
#' (categorical -> one-hot) or `"numeric"` (kept as-is). Returns `NULL` for
#' anything else, which downstream code treats as unsupported.
#' @param x a column from `data`.
#' @return `"character"`, `"numeric"`, or `NULL`.
#' @noRd
.simple_type <- function(x) {
  if (is.character(x) || is.factor(x)) {
    "character"
  } else if (is.numeric(x)) {
    "numeric"
  }
}

#' Parse a DBL formula into a per-term classification used to build `MAP`/`X`.
#'
#' Each RHS term (whether a bare variable, an interaction `a:b`, or a variable
#' inside `player(...)`) is reduced to a named character vector of column
#' classes (via `.simple_type()`). For interactions of mixed types the
#' character variable is sorted first so downstream builders can rely on a
#' canonical order.
#'
#' Example: for `rank | id ~ player(name) + gate * factor1` over data where
#' `name`/`factor1` are character and `gate` is numeric, this returns a list
#' like `list(name = "character", c(factor1 = "character", gate = "numeric"))`.
#'
#' @param data the training data.frame (consulted to read each column's class).
#' @param formula model formula.
#' @return A list, one element per term, of named class vectors.
#' @noRd
.parse_term_classes <- function(data, formula) {
  trm <- unlist(strsplit(attr(stats::terms(formula), "term.labels"), "[ ][+][ ]+"))
  team_term_idx <- grep("player\\(", trm)
  if (length(team_term_idx) > 0) {
    x <- gsub("player\\(([^)]*)\\)",
      "\\1",
      trm[team_term_idx],
      perl = TRUE
    )
    team_vars <- trimws(unlist(strsplit(x, "[+|:*]")))

    trm <- c(
      team_vars,
      trm[-team_term_idx]
    )
  }

  term_vars <- strsplit(trm, ":")

  .validate_variables_present(unlist(term_vars), data)

  classes <- lapply(data, .simple_type)
  term_vars <- lapply(term_vars, gsub, pattern = "([a-zA-Z]+\\()|(\\))", replacement = "")

  term_vars <- lapply(term_vars, function(x) unlist(classes[x]))
  term_vars <- lapply(term_vars, function(x) x[order(!x %in% c("character"))])

  term_vars
}

#' Build the string-keyed parameter map for DBL (the C++ `map` argument).
#'
#' For each row of `data` and each term, emits the parameter key that the
#' DBL update step will read/write. Conventions:
#' - bare character column -> `"colname=value"`
#' - bare numeric column   -> `"colname"` (constant per row)
#' - char x char           -> `"a=val_a:b=val_b"`
#' - char x num            -> `"a=val_a:b"`
#' - num x num             -> `"a*b"` (single coefficient)
#'
#' Special case: when the formula has a single bare character term (e.g.
#' `~ player(name)`), the prefix is dropped so keys are just the level
#' values. This matches the naming used by `glicko_run()`/`bbt_run()` and
#' lets users index `coef(model)[name, ]` uniformly across methods.
#'
#' These keys become the row names of the named `r`/`rd` vectors so updates
#' from different rows aggregate by parameter identity.
#'
#' @param data the training data.frame.
#' @param terms output of `.parse_term_classes()`.
#' @return Character matrix with `nrow(data)` rows and `length(terms)` columns.
#' @noRd
.build_term_map <- function(data, terms) {
  single_char_term <- length(terms) == 1 &&
    length(terms[[1]]) == 1 &&
    terms[[1]] == "character"

  df <- as.data.frame(
    lapply(terms, function(term) {
      if (length(term) == 1) {
        if (term == "character") {
          if (single_char_term) {
            as.character(data[[names(term)]])
          } else {
            sprintf("%s=%s", names(term), data[[names(term)]])
          }
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
          call. = FALSE
        )
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

  df
}

#' Build the numeric design matrix for DBL (the C++ `x` argument).
#'
#' Companion to `.build_term_map()`: same shape, but holds the *numeric*
#' contribution rather than the parameter key. Conventions:
#' - bare character / char x char -> `1` (one-hot indicator)
#' - bare numeric                 -> the column itself
#' - char x num                   -> the numeric column (the char part lives in `map`)
#' - num x num                    -> the elementwise product
#'
#' @param data the training data.frame.
#' @param terms output of `.parse_term_classes()`.
#' @return Numeric matrix with `nrow(data)` rows and `length(terms)` columns.
#' @noRd
.build_design_matrix <- function(data, terms) {
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
          call. = FALSE
        )
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

  colnames(df) <- names
  df <- as.matrix(df)

  df
}

#' Reduce each parsed term to a single class label (`"character"` or
#' `"numeric"`), used as the C++ `cls` argument for DBL. Char x num
#' interactions collapse to `"character"` since they live on the param-key
#' side of the map.
#' @param data unused (kept for symmetry with sibling builders).
#' @param terms output of `.parse_term_classes()`.
#' @return Character vector of length `length(terms)`.
#' @noRd
.term_classes <- function(data, terms) {
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
        call. = FALSE
      )
    }
  }, FUN.VALUE = character(1))

  out
}
