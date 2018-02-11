#' @export
dlr <- function(team_name,rank, X, H, S, Bu, pa){
  # rank <- c(3,4,1,2); X <- matrix(rnorm(8),ncol=2); S <- diag(rnorm(8)); H <- matrix(rnorm(8), nrow=4)
  k <- length(rank)
  home <- character(k*k-k)
  away <- character(k*k-k)
  P <- matrix(0,nrow=k*k-k, ncol=1)
  Y <- matrix(0,nrow=k*k-k, ncol=1)
  
  if(missing(Bu)) Bu <- 0
  if(missing(pa)) pa <- 1
  F <- diag(nrow = k)
  diag(F) <- pa
  
  # parametry rozkladu (apriori)
  X <- F %*% X + Bu
  S <- F %*% S %*% t(F) # a prioryczne przypuszczenie na temat kszta?towania si? wariancji
  
  OMEGA <- matrix(0, nrow=length(rank), ncol=1); 
  DELTA <- matrix(0, nrow=length(rank), ncol=length(rank)); 
  
  idx <- 0;
  for(i in 1:k){
    for(q in 1:k){
      if(q == i) next;
      idx <- idx + 1;
      home[ idx ] <- team_name[ i ]
      away[ idx ] <- team_name[ q ]
      
      x <- cbind(X[i,, drop=FALSE],X[i,, drop=FALSE])  
      h <- cbind( H[i,,drop=FALSE],H[q,,drop=FALSE])  
      s <- S[ c(i,q) , c(i,q) , drop=FALSE]
      y <- dplyr::case_when( rank[i]<rank[q]~1 , rank[i]>rank[q]~0 , T~.5 )
      
      # wariancja aktywacji
      s2 <- t(x) %*% s %*% x
      Ks <- 1/sqrt( 1 + (pi * s2)/8 )
      
      # error
      p  <- 1/( 1 + exp( -h %*% x))
      p  <- 1/( 1 + exp( -Ks * (h %*% x )) )
      P[ idx ] <- p
      Y[ idx ] <- y
      
      # UPDATE
      if(q < i) next;
      error <- y - p
      DELTA[c(i,q),c(i,q)] <- 
        DELTA[c(i,q),c(i,q)] + 
        as.numeric( 
          p * (1 - p)  / 
            ( 1 + p * (1 - p) * s2 ) 
        ) * 
        (  s %*% t(h)  %*% t( s %*% t(h) ) )
      
      OMEGA[c(i,q),] <- 
        OMEGA[c(i,q),] + 
        ( s /as.numeric(1 + p * (1 - p) * s2) ) %*% 
        t(h) * 
        as.numeric(error) 
    }
  }
  
  # S <- diag(x=diag(S))
  # X <- X + S %*% t(H) * as.numeric( error )
  S <- S - DELTA
  X <- X + OMEGA
  
  rownames(X) <- team_name
  rownames(S) <- team_name
  colnames(S) <- team_name
  
  
  list( 
    x = X,
    s = S,
    pairs = data.frame(home, away, P, Y)
  )
}