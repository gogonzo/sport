#' Dynamic Logistic Regression
#' 
#' Dynamic Logistic Regression
#' R implementation of 'Dynamic Logistic Regression' William D. Penny and Stephen J. ROberts
#' @export
kalman_logit <- function(x, z, H, P, Bu, pa){
  if(missing(Bu)) Bu <- 0
  if(missing(pa)) pa <- 1
  F <- diag(length(x))
  diag(F) <- pa
  # parametry rozkladu (apriori)
  x <- F %*% x + Bu
  P <- F %*% P %*% t(F) # a prioryczne przypuszczenie na temat kszta?towania si? wariancji
  
  # wariancja aktywacji
  s2 <- t(x) %*% P %*% x
  Ks <- 1/sqrt(1 + (pi * s2)/8)
  
  # error
  z.hat  <- 1/( 1 + exp( -H %*% x))
  z.hat  <- 1/( 1 + exp( -Ks * (H %*% x )) )
  error <- z - z.hat
  
  # UPDATE
  delta <- 
    as.numeric( 
      z.hat * (1 - z.hat)  / 
      ( 1 + z.hat * (1 - z.hat) * s2 ) 
    ) * 
    (  P %*% t(H)  %*% t( P %*% t(H) ) )
  
  omega <- 
    ( P /as.numeric(1+ z.hat * (1 - z.hat) * s2) ) %*% 
    t(H) * 
    as.numeric(error) 
  
  
  P <- P - delta

  # P <- diag(x=diag(P))
  # x <- x + P %*% t(H) * as.numeric( error )
  x <- x + omega
  
  

    list( 
      x = x,
      z.hat = z.hat,
      error = error,
      P = P
    )
}