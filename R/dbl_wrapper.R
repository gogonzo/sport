dbl_wrapper <- function(ranks, R, RD, X){
  
  H  <- mapParams( X_list[[i]] )  
  X  <- getX( X_list[[i]] )
  R  <- apply(H,2, function(x) R_list[x])
  RD <- apply(H,2, function(x) RD_list[x])  
  
  R[is.na(R )] <- 0
  RD[is.na(RD)] <- 0
  
  ddl <- bdl( 
    names, 
    rank = ranks,  
    R  = R, 
    RD = RD,
    X  = X
  )
  
  update_R  <- tapply(ddl$OMEGA, H, sum, na.rm=T )
  update_RD <- tapply(ddl$DELTA, H, sum, na.rm=T )
  
  R_list[ names(update_R) ]   <- R_list[ names(update_R) ]   + update_R
  RD_list[ names(update_RD) ] <- RD_list[ names(update_RD) ] - update_RD

  
  
  
}