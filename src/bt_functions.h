NumericMatrix sqrt_mat_elems (NumericMatrix x){
  int n = x.nrow();
  int k = x.ncol();
  NumericMatrix powered(n,k);
  
  for (int i = 0; i < n ; ++i) {
    powered(i,_) = sqrt(x(i,_));
  }

  return powered;
}


NumericMatrix pow_mat_elems (NumericMatrix x){
  int n = x.nrow();
  int k = x.ncol();
  NumericMatrix powered(n,k);
  
  for (int i = 0; i < n ; ++i) {
    powered(i,_) = pow(x(i,_),2.0);
  }
  
  return powered;
}

double calc_s(int rank_i, int rank_j){
  if(rank_i < rank_j ){
    return 1.0;
  } else if( rank_i==rank_j){
    return 0.5;
  } else {
    return 0.0;
  }
}