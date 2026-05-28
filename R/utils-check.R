#' @importFrom stats setNames terms update
NULL

#' Stop if `data` was not supplied to a `*_run()` call.
#' @param data caller's `data` argument (typically captured via `missing()`).
#' @return Invisibly `NULL`. Called for its side effect.
#' @noRd
.validate_data <- function(data) {
  if (missing(data)) stop("Data is not provided", call. = FALSE)
}

#' Stop if `newdata` is missing any column required by the model formula.
#' Used by [predict.ratings()] before passing `newdata` to the C++ predictor.
#' @param vars character vector of required column names.
#' @param newnames `colnames(newdata)`.
#' @return Invisibly `NULL`. Called for its side effect.
#' @noRd
.validate_newdata_columns <- function(vars, newnames) {
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

#' Stop if any variable referenced by the formula is missing from `data`.
#' @param vars character vector of column names extracted from the formula.
#' @param data the user-supplied training/prediction data.frame.
#' @return Invisibly `TRUE`. Called for its side effect.
#' @noRd
.validate_variables_present <- function(vars, data) {
  missing_vars <- setdiff(vars, colnames(data))
  if (length(missing_vars) > 0) {
    stop(
      sprintf(
        "Variable(s) %s specified in formula not present in data",
        paste(missing_vars, collapse = ", ")
      ),
      call. = FALSE
    )
  }

  invisible(TRUE)
}

#' Type/range check for a numeric input vector.
#' @param x value to check.
#' @param var_name name to use in error messages.
#' @param min,max inclusive bounds.
#' @return Invisibly `NULL`. Called for its side effect.
#' @noRd
.check_numeric_argument <- function(x, var_name, min = -Inf, max = Inf) {
  if (!is.numeric(x)) {
    stop(sprintf("Variable %s should be of type numeric.", var_name),
      call. = FALSE
    )
  } else if (any(!is.finite(x))) {
    stop(
      sprintf("Variable %s contains non-finite values. All elements should be finite.", var_name),
      call. = FALSE
    )
  } else if (any(x < min | x > max)) {
    stop(sprintf("All values in variable %s should be in range [%s, %s]", var_name, min, max),
      call. = FALSE
    )
  }
}

#' Type/range check for an integer input vector. Used to assert that
#' `id`/`rank` columns coerced via `as.integer()` are well-formed.
#' @inheritParams .check_numeric_argument
#' @return Invisibly `NULL`. Called for its side effect.
#' @noRd
.check_integer_argument <- function(x, var_name, min = -Inf, max = Inf) {
  if (!is.integer(x)) {
    stop(
      sprintf("Variable %s should be of type integer.", var_name),
      call. = FALSE
    )
  } else if (any(!is.finite(x))) {
    stop(
      sprintf("Variable %s contains non-finite values. All elements should be finite.", var_name),
      call. = FALSE
    )
  } else if (any(x < min | x > max)) {
    stop(
      sprintf("All values in variable %s should be in range [%s, %s]", var_name, min, max),
      call. = FALSE
    )
  }
}

#' Type check for a character input vector. Disallows `NA`.
#' @param x value to check.
#' @param var_name name to use in error messages.
#' @return Invisibly `NULL`. Called for its side effect.
#' @noRd
.check_string_argument <- function(x, var_name) {
  if (!is.character(x)) {
    stop(sprintf("Variable %s should be of type character.", var_name), call. = FALSE)
  } else if (any(is.na(x))) {
    stop(
      sprintf("Variable %s contains non-finite values. All elements should be finite.", var_name),
      call. = FALSE
    )
  }
}

#' Assert that a scalar argument is length-1 and (if numeric) within bounds.
#' Used for `init_r`, `init_rd`, `init_sigma`, `tau`, `kappa`.
#' @param x value to check.
#' @param var_name name to use in error messages.
#' @param min,max inclusive bounds (only enforced when `x` is numeric).
#' @return Invisibly `NULL`. Called for its side effect.
#' @noRd
.check_single_argument <- function(x, var_name, min = -Inf, max = Inf) {
  if (length(x) > 1) {
    stop(
      sprintf("variable %s should be a single value", var_name),
      call. = FALSE
    )
  } else if (is.numeric(x) && x < min) {
    stop(
      sprintf("variable %s should be greater than %s", var_name, min),
      call. = FALSE
    )
  } else if (is.numeric(x) && x > max) {
    stop(
      sprintf("variable %s should be lower than %s", var_name, max),
      call. = FALSE
    )
  }
}

#' Resolve the user's `r` argument into a complete, named rating vector.
#'
#' One of three branches runs:
#' - if `r` is empty, build a fresh vector filled with `init_r` for every
#'   player in `unique_names`;
#' - if `r` is supplied but missing some names, warn and patch the gaps with
#'   `init_r` (preserving any provided ratings);
#' - if `r` already covers `unique_names`, validate and return as-is.
#'
#' @param r user-supplied named numeric vector (possibly empty).
#' @param init_r scalar default for missing names.
#' @param unique_names character vector of every player/parameter that must
#'   appear in the result.
#' @param player name of the formula term (used in warning messages only).
#' @return Named numeric vector covering exactly `unique_names`.
#' @noRd
.resolve_r <- function(r, init_r, unique_names, player) {
  if (length(r) == 0) {
    if (!is.finite(init_r)) {
      stop("init_r should be a finite number", call. = FALSE)
    }
    r <- setNames(rep(init_r, length(unique_names)), unique_names)
  } else if (any(duplicated(names(r)))) {
    stop("All names in r should be unique. Duplicated names not allowed.", call. = FALSE)
  } else if (any(!unique_names %in% names(r))) {
    warning(
      sprintf(
        "Some names in %s doesn't have a match in r.
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

  r
}

#' Resolve the user's `rd` argument into a complete, named RD vector.
#' Mirrors `.resolve_r()` but additionally requires `init_rd > 0` and
#' rejects negative values in a supplied `rd`.
#' @inheritParams .resolve_r
#' @return Named numeric vector covering exactly `unique_names`.
#' @noRd
.resolve_rd <- function(rd, init_rd, unique_names, player) {
  if (length(rd) == 0) {
    if (!is.finite(init_rd) || init_rd <= 0) {
      stop("init_rd value should be positive", call. = FALSE)
    }
    rd <- setNames(rep(init_rd, length(unique_names)), unique_names)
  } else if (any(duplicated(names(rd)))) {
    stop("All names in rd should be unique. Duplicated names not allowed.", call. = FALSE)
  } else if (any(!unique_names %in% names(rd))) {
    warning(
      sprintf(
        "Some names in %s doesn't have a match in rd.
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

  rd
}

#' Resolve the user's `sigma` argument into a complete, named volatility
#' vector -- only meaningful for Glicko-2. For other methods this short-circuits
#' to `numeric(0)` so the rest of `.rating_run()` can stay branch-free.
#' @inheritParams .resolve_r
#' @param sigma user-supplied named numeric vector (possibly empty).
#' @param init_sigma scalar default for missing names.
#' @param method one of `"glicko"`, `"glicko2"`, `"bbt"`, `"dbl"`.
#' @return Named numeric vector for Glicko-2; `numeric(0)` otherwise.
#' @noRd
.resolve_sigma <- function(sigma, init_sigma, unique_names, player, method) {
  if (method != "glicko2") {
    return(numeric(0))
  } else if (length(sigma) == 0) {
    if (!is.finite(init_sigma) || init_sigma <= 0) {
      stop("init_sigma value should be positive", call. = FALSE)
    }
    sigma <- setNames(rep(init_sigma, length(unique_names)), unique_names)
  } else if (any(duplicated(names(sigma)))) {
    stop("All names in sigma should be unique. Duplicated names not allowed.", call. = FALSE)
  } else if (any(!unique_names %in% names(sigma))) {
    warning(
      sprintf(
        "Some names in %s doesn't have a match in sigma.
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

  sigma
}

#' Assert that two named vectors share the exact same set of names.
#' Used to ensure `r` and `rd` describe the same set of players/parameters
#' before they reach the C++ layer. Error messages refer to the caller-side
#' variable names captured via `substitute()`.
#' @param x,y named vectors to compare.
#' @return Invisibly `TRUE`. Called for its side effect.
#' @noRd
.check_equal_names <- function(x, y) {
  xname <- deparse(substitute(x))
  yname <- deparse(substitute(y))

  diff1 <- setdiff(names(x), names(y))
  if (length(diff1) > 0) {
    stop(sprintf("Names of %s doesn't match %s names.", xname, yname),
      call. = FALSE
    )
  }

  diff2 <- setdiff(names(y), names(x))
  if (length(diff2) > 0) {
    stop(sprintf("Names of %s doesn't match %s names.", yname, xname),
      call. = FALSE
    )
  }

  invisible(TRUE)
}

#' Resolve a per-row weighting/lambda/share argument into a numeric vector
#' aligned with `data`.
#'
#' Accepts three input shapes:
#' - empty/NULL -> vector of `1.0` of length `nrow(data)`;
#' - a single column name from `data` -> that column, after a numeric/range check;
#' - a single numeric scalar -> recycled to length `nrow(data)`.
#'
#' Anything else is an error.
#' @param var user input (NULL, length-1 character, or length-1 numeric).
#' @param data the training/prediction data.frame.
#' @param argname argument name used in error messages.
#' @param min,max inclusive bounds passed to `.check_numeric_argument()`.
#' @return Numeric vector of length `nrow(data)`.
#' @noRd
.resolve_vec_arg <- function(var, data, argname, min = -Inf, max = Inf) {
  if (length(var) == 0) {
    var_vec <- rep(1.0, nrow(data))
  } else if (is.character(var) && length(var) == 1) {
    if (!var %in% colnames(data)) {
      stop(sprintf(
        "variable %s specified in %s is not present in data",
        var, argname
      ))
    }
    var_vec <- data[[var]] # 1 divided by n_it
    .check_numeric_argument(var_vec, var, min = min, max = max)
  } else if (is.numeric(var) && length(var) == 1) {
    .check_numeric_argument(var, argname, min = min, max = max)
    var_vec <- rep(var, nrow(data))
  } else {
    stop(
      sprintf("%s should be variable name from `data` argument or single numeric value.", argname)
    )
  }

  return(var_vec)
}
