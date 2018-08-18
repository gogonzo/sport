#' @import data.table
#' @import ggplot2
#' @importFrom stats reorder
NULL

#' Summarizing sport objects
#' 
#' Summarizing sport objects
#' Summary for object of class `sport`
#' @param object of class sport
#' @param ... optional arguments
#' @return 
#' List with following elements \itemize{
#' \item \code{formula} modelled formula.
#' \item \code{method} type of algorithm used.
#' \item \code{Overall Accuracy} named vector containing players ratings.
#' \item \code{r} data.frame summarized players ratings and model winning probabilities. Probabilities are returned only in models with one variable (ratings) \itemize{
#' \item \code{name} of a player
#' \item \code{r} players ratings
#' \item \code{rd} players ratings deviation
#' \item \code{`Model probability`} mean predicted probability of winning the challange by the player.
#' \item \code{`True probability`} mean observed probability of winninh the challange by the player.
#' \item \code{`Accuracy`} Accuracy of prediction.
#' \item \code{`pairings`} number of pairwise occurences.
#' }
#' }
#' 
#' @examples
#' model <- glicko_run(rank|id~rider, gpheats[1:100,]) 
#' summary(model)
#' @export
summary.sport <- function(object,...){
  if(!"sport" %in% class(object) ) stop("Function summary() needs object of class sport")
  if(!any(c("glicko","glicko2","bbt","dbl") %in% attr(object,"method") )) stop("Function summary() needs object of class sport")
  
  model_probs_players <- object$pairs[,.( `Model probability` = mean(P),
                                          `True probability` = mean(Y),
                                          `Accuracy`   = mean( (P>.5) == Y ), 
                                          `pairings` = length(P)),
                                      name]
  
  acc <- object$pairs[,.(`acc`   = mean( (P>.5) == Y ), `pairings` = length(P)),]
  
  players_ratings <- data.table( name = names(object$final_r),
                                 r = object$final_r, 
                                 rd = object$final_rd )
  

  
  if( length(all.vars(update(attr(object,"formula"), 0~.)))==1 )
    r = players_ratings[model_probs_players, on = 'name']  else
    r = players_ratings
  
  out <- list( 
    formula = attr(object, "formula"),
    method = attr(object,"method"),
    `Overall Accuracy` = acc$acc,
    `Number of pairs` = acc$pairings,
    r = r
  )
  return(out)
}


#' @export
print.sport <- function(x,...){
  
  model_probs_intervals <- x$pairs[,.(
    `Model probability` = mean(P),
    `True probability` = mean(Y),
    `Accuracy`   = mean( (P>.5) == Y ), 
    `n` = length(P)),
    list( Interval = cut(P, breaks = seq(0,1,by=0.1), include.lowest = T))
    ][order(Interval)]
  
  
  out <- x$pairs[,.(n = length(P) , `accuracy` = mean( (P>.5)==Y)  )] 
    
  cat(
    paste("\nCall:", format(attr(x,"formula"))), 
    paste("\nNumber of unique pairs:", out$n/2),
    paste("\nAccuracy of the model:", round(out$accuracy,2)),
    "\nTrue probabilities and Accuracy in predicted intervals:",
    sep = "\n"
  )
  print(model_probs_intervals)
  invisible(0)
}

#' Plot sport object
#' 
#' @param object of class sport
#' @param n number of players to be plotted
#' @param ... optional arguments
#' @export
plot_sport <- function(object,n=10,...){
  data <- data.frame( 
    name = names(object$final_r), 
    r = object$final_r, 
    rd = object$final_rd, 
    row.names = NULL,
    stringsAsFactors = F)
  
  data <- data[order(data$r),]
  data$rank <- 1:nrow(data)
  data$name <- reorder(data$name, 1:nrow(data))
  

  ggplot( data[1:n,] , aes(x=name, y=r)) + 
    ggtitle('Actual ratings') + 
    geom_linerange(aes(ymin=r-rd*1.98, ymax= r+rd*1.98), size=1*0.8, alpha=0.4)  + 
    geom_point(colour="grey20", size=1) + 
    coord_flip() + 
    scale_x_discrete("Name") + 
    scale_y_continuous("Name") + 
    theme_bw()
}
