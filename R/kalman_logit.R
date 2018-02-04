#' @export
kalman_logit <- function(x.k, z.k, H.k, P.k, Bu, pa){
  if(missing(Bu)) Bu <- 0
  if(missing(pa)) pa <- 1
  F <- diag(length(x.k))
  diag(F) <- pa
  # parametry rozkladu (apriori)
  x.k <- F %*% x.k + Bu
  P.k <- F %*% P.k %*% t(F) # a prioryczne przypuszczenie na temat kszta?towania si? wariancji
  
  # wariancja aktywacji
  s2.k <- t(x.k) %*% P.k %*% x.k
  Ks.k <- 1/sqrt(1 + (pi*s2.k)/8)
  
  # error
  z.hat  <- 1/( 1 + exp( -H.k %*% x.k))
  z.hat  <- 1/( 1 + exp( -Ks.k * (H.k %*% x.k )) )
  error <- z.k - z.hat
  
  # UPDATE
  P.k <- 
    P.k - 
    as.numeric( z.hat * (1 - z.hat)  / ( 1 + z.hat * (1 - z.hat) * s2.k ) ) * 
    (  P.k %*% t(H.k)  %*% t( P.k %*% t(H.k) ) )
  #P.k <- diag(x=diag(P.k))
  x.k <- x.k + P.k %*% t(H.k) * as.numeric( error )
  x.k <- x.k + ( P.k/as.numeric(1+ z.hat * (1 - z.hat) * s2.k) ) %*% t(H.k) * as.numeric(error) 
  
  

    list( 
      x.k = x.k,
      z.hat = z.hat,
      error = error,
      P.k = P.k
    )
}