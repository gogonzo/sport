#' Convert pairwise probabilities to positional probabilities
#'
#' Given pairwise win probabilities from a rating model, compute the marginal
#' probability of each player finishing in each position (1st, 2nd, ..., nth)
#' using the Plackett-Luce model with exact enumeration.
#'
#' @param object An object of class "estimates" (output of `glicko_run`, `glicko2_run`, `bbt_run`, or `dbl_run`).
#' @param newdata A data.frame with player names and event id columns matching the model formula.
#'   All rows should belong to a single event (heat). Rank column is not needed (set to NA internally).
#' @return A data.frame with columns: player name, and one column per position (pos_1, pos_2, ..., pos_n)
#'   containing the probability of finishing in that position.
#' @details
#' The function:
#' 1. Calls `predict()` to get pairwise P (probability of beating each opponent)
#' 2. Derives per-player strength from pairwise P using geometric mean
#' 3. Enumerates all n! permutations and computes Plackett-Luce probability for each
#' 4. Marginalizes to get per-player, per-position probabilities
#'
#' Exact enumeration is feasible for up to ~8 players (40320 permutations).
#' For speedway heats (2-4 riders) this is trivially fast.
#'
#' @examples
#' glicko <- glicko_run(
#'   data = gpheats[1:16, ],
#'   formula = rank | id ~ player(rider)
#' )
#' # Predict positional probabilities for a heat with 4 riders
#' newdata <- data.frame(
#'   id = rep(999L, 4),
#'   rider = c("ADAMS Leigh", "ALLEN Mark", "ANDERSEN Hans", "ARMITSTEAD Lee"),
#'   rank = rep(NA_integer_, 4)
#' )
#' positional(glicko, newdata)
#' @export
positional <- function(object, newdata) {
  if (!inherits(object, "estimates")) {
    stop("object must be of class 'estimates'", call. = FALSE)
  }

  formula <- attr(object, "formula")
  rhs_terms <- .extract_team_terms(formula)
  player_col <- if (length(rhs_terms) == 1) rhs_terms[1] else rhs_terms[2]

  players <- as.character(newdata[[player_col]])
  n <- length(players)

  if (n < 2) stop("Need at least 2 players", call. = FALSE)
  if (n > 8) stop("Exact enumeration limited to 8 players (", n, "! = ", factorial(n), " permutations)", call. = FALSE)

  # Get pairwise probabilities
  pairs <- predict(object, newdata)

  # Build pairwise P matrix: P_mat[i,j] = P(player_i beats player_j)
  P_mat <- matrix(0.5, nrow = n, ncol = n, dimnames = list(players, players))
  for (k in seq_len(nrow(pairs))) {
    pi <- as.character(pairs[[player_col]][k])
    pj <- as.character(pairs[["opponent"]][k])
    P_mat[pi, pj] <- pairs$P[k]
  }

  # Derive strength per player from pairwise P
  # Using log-linear: strength_i = exp(mean(log(P_mat[i,j] / P_mat[j,i]))) for j != i
  # This recovers the Bradley-Terry strength parameter
  strengths <- numeric(n)
  names(strengths) <- players
  for (i in seq_len(n)) {
    log_odds <- numeric(0)
    for (j in seq_len(n)) {
      if (i == j) next
      p_ij <- max(P_mat[i, j], 1e-10)
      p_ji <- max(P_mat[j, i], 1e-10)
      log_odds <- c(log_odds, log(p_ij / p_ji))
    }
    strengths[i] <- exp(mean(log_odds))
  }

  # Enumerate all permutations
  perms <- .permutations(n)

  # Compute Plackett-Luce probability for each permutation
  # P(perm) = prod_{k=1}^{n} strength[perm[k]] / sum_{j=k}^{n} strength[perm[j]]
  perm_probs <- numeric(nrow(perms))
  for (p in seq_len(nrow(perms))) {
    perm <- perms[p, ]
    prob <- 1.0
    for (k in seq_len(n - 1)) {
      remaining_strength <- sum(strengths[perm[k:n]])
      prob <- prob * strengths[perm[k]] / remaining_strength
    }
    perm_probs[p] <- prob
  }

  # Normalize (should sum to ~1 already, but ensure exactness)
  perm_probs <- perm_probs / sum(perm_probs)

  # Marginalize: P(player_i finishes in position k)
  pos_probs <- matrix(0, nrow = n, ncol = n)
  rownames(pos_probs) <- players
  colnames(pos_probs) <- paste0("pos_", seq_len(n))

  for (p in seq_len(nrow(perms))) {
    perm <- perms[p, ]
    for (k in seq_len(n)) {
      pos_probs[perm[k], k] <- pos_probs[perm[k], k] + perm_probs[p]
    }
  }

  result <- data.frame(player = players, pos_probs, stringsAsFactors = FALSE)
  names(result)[1] <- player_col
  result
}

#' Generate all permutations of 1:n
#' @param n integer
#' @return matrix with n! rows and n columns
#' @noRd
.permutations <- function(n) {
  if (n == 1) return(matrix(1L, nrow = 1, ncol = 1))
  sub <- .permutations(n - 1L)
  out <- matrix(0L, nrow = n * nrow(sub), ncol = n)
  idx <- 1L
  for (i in seq_len(n)) {
    for (j in seq_len(nrow(sub))) {
      row <- integer(n)
      row[1] <- i
      rest <- sub[j, ]
      rest[rest >= i] <- rest[rest >= i] + 1L
      row[2:n] <- rest
      out[idx, ] <- row
      idx <- idx + 1L
    }
  }
  out
}
