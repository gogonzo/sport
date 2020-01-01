Rcpp::NumericMatrix pow_mat_elems(Rcpp::NumericMatrix x) {
  int n = x.nrow();
  int k = x.ncol();
  Rcpp::NumericMatrix powered(n,k);
  
  for (int i = 0; i < n ; ++i) {
    powered(i, Rcpp::_) = pow(x(i, Rcpp::_),2.0);
  }
  
  return powered;
}

double calc_s(int rank_i, int rank_j) {
  if(rank_i < rank_j ) {
    return 1.0;
  } else if (rank_i == rank_j) {
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

Rcpp::NumericMatrix subset_rows(Rcpp::NumericMatrix x, Rcpp::IntegerVector idx) {
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

double teamPlayersSum(Rcpp::IntegerVector idx_it,
                  Rcpp::StringVector player,
                  Rcpp::StringVector player_names,
                  Rcpp::NumericVector r) {
  int n = idx_it.size();
  Rcpp::NumericVector out(n);
  Rcpp::StringVector player_vec_it = player[idx_it];
  Rcpp::IntegerVector player_idx = Rcpp::match(player_vec_it, player_names) - 1;
  Rcpp::NumericVector team_players_ratings = r[player_idx];
  
  return sum(team_players_ratings);
}

double teamPlayersSSQ(Rcpp::IntegerVector& idx_it,
                      Rcpp::StringVector& player,
                      Rcpp::NumericVector& sigma,
                      Rcpp::StringVector& player_names,
                      Rcpp::NumericVector& rd) {
  int n = idx_it.size();
  Rcpp::NumericVector out(n);
  Rcpp::StringVector player_vec_it = player[idx_it];
  Rcpp::NumericVector sigma_vec_it = sigma[idx_it];
  Rcpp::IntegerVector player_idx = Rcpp::match(player_vec_it, player_names) - 1;
  
  double ssq = 0;
  int idx;
  
  for (int i = 0; i < n; i++) {
    idx = player_idx(i);
    rd(idx) = sqrt(pow(rd(idx), 2.0) + pow(sigma_vec_it(i), 2.0));
    ssq += pow(rd(idx), 2.0); 
  }
  return ssq;
}


template <typename itype>
itype teamFirstValue(Rcpp::IntegerVector& idx_it, 
                     Rcpp::Vector<Rcpp::traits::r_sexptype_traits<itype>::rtype>& x) {
  int i = idx_it(0);  
  return x(i);
}

void updatePlayerR(Rcpp::IntegerVector& idx_it,
                   Rcpp::StringVector& player,
                   Rcpp::NumericVector& sigma,
                   Rcpp::StringVector& player_names,
                   Rcpp::NumericVector& rd) {
  int n = idx_it.size();
  Rcpp::NumericVector out(n);
  Rcpp::StringVector player_vec_it = player[idx_it];
  Rcpp::NumericVector sigma_vec_it = sigma[idx_it];
  Rcpp::IntegerVector player_idx = Rcpp::match(player_vec_it, player_names) - 1;
  
  double ssq = 0;
  int idx;
  
  for (int i = 0; i < n; i++) {
    idx = player_idx(i);
    rd(idx) = sqrt(pow(rd(idx), 2.0) + pow(sigma_vec_it(i), 2.0));
    ssq += pow(rd(idx), 2.0); 
  }
}

