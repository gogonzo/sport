#' Ranking for multiple input
#'
#' Ranks multiple inputs with custom options
#'
#' @param a vector of attributes with prefix \code{a*} as variable to be ranked. Multiple arguments can be provided like \code{a1}, \code{a2}, ... \code{a10}. All arguments will be examined in given order to obtain ranking. If some observation have the same attribute (tie) then next argument will be decisive about ranking etc.
#' @param b vector of attributes with prefix \code{b*}
#' @param x vector with non-NA value pointing for an excluded from ranking. Ranking for \code{!is.na(x)} is being set to \code{NA} and others are updated
#' @param ties method of dealing with ties. Available 'first' and default='none'
#'
#' @return A list with the elements
#' \item{y}{The sum of the squared values.}
#' @note This is a very simple function.
#' @examples
#' df <-
#'   data.frame(
#'     group = c(rep("A", 4), rep("B", 4)),
#'     pts = c(60, 59, 40, 30, 30, 30, 20, 20),
#'     bal = c(25, 30, 20, 16, 12, 12, 10, 8),
#'     semi = c("A", "B", "A", "B", rep(NA, 4)),
#'     final = c(1, NA, -2, rep(NA, 5)),
#'     rel = c(rep(NA, 6), 1, 0),
#'     x = c(rep(NA, 3), 1, rep(NA, 4))
#'   )
#' @rdname ranking
#' @export


ranking <- function(..., ties = "none") {
  args <- list(...)
  # args <- list(a1=df$final, a2=df$semi, a3=df$pts, a4=df$bal, b1 =df$rel, x1=df$x)
  id <- 1:length(args[[1]])
  if (any(names(args) %in% c("a", "b", "c"))) {
    stop("argument name can not be named as a,b,c,d without suffix. Variables must be named a1, a2, b3, c4, d2, d3")
  }

  a <- grep("^a", names(args), value = T)
  b <- grep("^b", names(args), value = T)
  x <- grep("^x", names(args), value = T)
  g <- grep("^g", names(args), value = T)

  # assign
  for (i in 1:length(args)) {
    assign(names(args)[i], args[[i]])
  }

  # order absolute ----
  order_a <- toString(a)
  statement <- sprintf("idx <- order(%s,na.last = T, decreasing=T, method ='radix')", order_a)
  eval(parse(text = statement))
  id[idx] <- id

  # order head to head for selected (non NA)----
  if (length(b) > 0) {
    for (arg in b) {
      st1 <- sprintf("idx <- which(!is.na(args$%s))", arg)
      st2 <- sprintf("id[idx] <- id[idx][order(args$%s[idx],decreasing=T)]", arg)
      eval(parse(text = st1))
      eval(parse(text = st2))
    }
  }

  # all behind excluded position - 1  ----
  if (length(x) > 0) {
    for (i in which(!args$x %in% c(0, NA))) {
      id[id > id[i]] <- id[id > id[i]] - 1
      id[i] <- NA
    }
  }
  # tied if all args are equal -----
  if (ties == "none") {
    NULL
  } else if (ties == "first") {
    all <- sapply(args, cbind)
    for (i in 2:nrow(all)) {
      if (identical(all[i, ], all[i - 1, ])) {
        id[i] <- id[i - 1]
      }
    }
  } else if (ties == "average") {
    warning("Average not implemented yet, please use ties=c('first','none')")
  }

  return(id)
}
