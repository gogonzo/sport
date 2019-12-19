Rcpp::NumericMatrix pow_mat_elems(Rcpp::NumericMatrix x) {
  int n = x.nrow();
  int k = x.ncol();
  Rcpp::NumericMatrix powered(n,k);
  
  for (int i = 0; i < n ; ++i) {
    powered(i,_) = pow(x(i,_),2.0);
  }
  
  return powered;
}

double calc_s(int rank_i, int rank_j) {
  if(rank_i < rank_j ) {
    return 1.0;
  } else if( rank_i==rank_j) {
    return 0.5;
  } else {
    return 0.0;
  }
}

Rcpp::NumericVector pow_vec_elems(Rcpp::NumericVector x) {
  int n = x.size();
  Rcpp::NumericVector out(n);
  
  for (int i = 0; i < n ; ++i) {
    out(i) = pow(x(i),2.0);
  }
  
  return out;
}

Rcpp::NumericMatrix subset_rows(Rcpp::NumericMatrix x, Rcpp::NumericVector idx) {
  int n = idx.size();
  int k = x.ncol();
  Rcpp::NumericMatrix out(n, k);
  
  for (int i = 0; i < n; i++) {
    for (int j = 0; j < k; j++) {
      out(i, j) = x(idx(i), j);
    }
  }
  
  return out;
}