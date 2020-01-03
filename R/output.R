#' @import data.table
#' @import ggplot2
#' @importFrom stats reorder
NULL

#' Summarizing rating objects
#'
#' Summarizing rating objects
#' Summary for object of class `rating`
#' @param object of class rating
#' @param ... optional arguments
#' @return
#' List with following elements \itemize{
#' \item \code{formula} modeled formula.
#' \item \code{method} type of algorithm used.
#' \item \code{Overall Accuracy} named vector containing players ratings.
#' \item \code{r} data.frame summarized players ratings and model winning probabilities. Probabilities are returned only in models with one variable (ratings) \itemize{
#' \item \code{name} of a player
#' \item \code{r} players ratings
#' \item \code{rd} players ratings deviation
#' \item \code{`Model probability`} mean predicted probability of winning the challenge by the player.
#' \item \code{`True probability`} mean observed probability of winning the challenge by the player.
#' \item \code{`Accuracy`} Accuracy of prediction.
#' \item \code{`pairings`} number of pairwise occurrences.
#' }
#' }
#'
#' @examples
#' model <- glicko_run(formula = rank | id ~ player(rider), 
#'                     data = gpheats[1:102, ])
#' summary(model)
#' @export
summary.rating <- function(object, ...) {
  formula <- attr(object, "formula")
  team_terms <- extract_team_terms(formula)
  team_term_name <- get_team_name(formula)
  player_term_name <- get_player_name(formula)
  
  model_probs_teams <- object$pairs[
    , .(
      `Model probability` = round(mean(P), 3),
      `True probability` = round(mean(Y), 3),
      `Accuracy` = round(mean((P > .5) == Y), 3),
      `pairings` = length(P)
    ),
    team_term_name
    ]
  
  acc <- object$pairs[, .(`acc` = mean((P > .5) == Y), `pairings` = length(P)), ]
  
  players_ratings <- data.table(
    team = names(object$final_r),
    r = round(object$final_r, 3),
    rd = round(object$final_rd, 3)
  )
  names(players_ratings)[1] <- if (length(player_term_name) == 0) {
    team_term_name
  } else {
    player_term_name
  }
  
  
  r <- if (attr(object, "method") == "dbl" || length(team_terms) == 2) {
    players_ratings
  } else {
    merge(players_ratings, model_probs_teams, by = team_term_name)
  }
  
  out <- list(
    formula = attr(object, "formula"),
    method = attr(object, "method"),
    `Overall Accuracy` = acc$acc,
    `Number of pairs` = acc$pairings,
    r = r
  )
  return(out)
}


#' @export
print.rating <- function(x, ...) {
  model_probs_intervals <- x$pairs[
    , .(
      `Model probability` = round(mean(P), 3),
      `True probability` = round(mean(Y), 3),
      `Accuracy` = round(mean((P > .5) == Y), 3),
      `n` = length(P)
    ),
    list(Interval = cut(P, breaks = seq(0, 1, by = 0.1), include.lowest = TRUE))
    ][order(Interval)]
  
  
  out <- x$pairs[, .(n = length(P), `accuracy` = mean((P > .5) == Y))]
  
  cat(
    paste("\nCall:", format(attr(x, "formula"))),
    paste("\nNumber of unique pairs:", out$n / 2),
    paste("\nAccuracy of the model:", round(out$accuracy, 2)),
    "\nTrue probabilities and Accuracy in predicted intervals:",
    sep = "\n"
  )
  print(model_probs_intervals)
  invisible(0)
}

#' Plot rating object
#'
#' @param x of class rating
#' @param n number of teams to be plotted
#' @param players optional vector with names of the contestants (coefficients) to plot 
#' their evolution in time.
#' @param ... optional arguments
#' @export
plot.rating <- function(x, n = 10, players, ...) {
  formula <- attr(x, "formula")
  variable <- extract_team_terms(formula)[1]
  
  if (!missing(players)) {
    data <- x$r[x$r[[variable]] %in% players, ]
    ggplot(data, 
           aes_string(x = "id", 
                      y = "r", 
                      group = variable, 
                      color = variable)) +
      geom_line() +
      ggtitle("Ratings evolution") +
      theme_bw()
  } else {
    data <- data.frame(
      team = names(x$final_r),
      r = x$final_r,
      rd = x$final_rd,
      row.names = NULL,
      stringsAsFactors = F
    )
    names(data)[1] <- variable
    
    data <- data[order(data$r, decreasing = TRUE), ][1:n, ]
    data[[variable]] <- reorder(data[[variable]], nrow(data):1)
    ggplot(data, aes_string(x = variable, 
                            y = "r")) +
      ggtitle("Actual ratings") +
      geom_linerange(aes(ymin = r - rd * 1.98, 
                         ymax = r + rd * 1.98), 
                     size = 1 * 0.8, 
                     alpha = 0.4) +
      geom_point(colour = "grey20", size = 1) +
      coord_flip() +
      scale_x_discrete(variable) +
      scale_y_continuous("r") +
      theme_bw()
  }
}
