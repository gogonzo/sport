namespace utils {

  template <typename itype>
  Rcpp::IntegerVector find(itype val,
                           Rcpp::Vector<Rcpp::traits::r_sexptype_traits<itype>::rtype>& x) {
      bool found = false;
      int n = x.size();
      Rcpp::IntegerVector y;
      
      for(int i = 0; i < n; i++) {
          if (x[i] == val) {
            y.push_back(i); 
            found = true;
          } else if (found) {
            return(y);
          }
      }
      return y;
  }
  
  
  template <int ITYPE>
  Rcpp::Vector<ITYPE> unique(Rcpp::Vector<ITYPE> x) {
    int n = x.size(), k = 0, j;
    if (n == 1) return(x);
    
    bool duplicated = false;
    Rcpp::Vector<ITYPE> res;
    
    for (int i = 0; i < n; i++) {
      duplicated = false;
      
      j = k - 1;
      while (!duplicated && j >= 0) {
        if (x(i) == res(j)) duplicated = true;
        j--;
      } 
      
      if (!duplicated) {
        res.push_back(x(i));
        k++;
      }
    }
    
    return res;
  }
  
  

  Rcpp::List get_ratings_list(Rcpp::List names_current, 
                              Rcpp::StringVector unique_names,
                              Rcpp::NumericVector ratings) {
    int n = names_current.size();
    Rcpp::IntegerVector names_idx;
    Rcpp::NumericVector out_i;
    Rcpp::StringVector names_i;
    Rcpp::List out(n);
    
    SEXP ll;
    for (int i = 0; i < n; i++) {
      ll = names_current(i);
      names_i = Rcpp::as<Rcpp::StringVector>(ll); 
      names_idx = Rcpp::match(names_i, unique_names) - 1;
      out_i = ratings[names_idx];
      out(i) = out_i;
    }
    
    return out;
  }
  
  Rcpp::NumericVector listSum(Rcpp::List l) {
    int n = l.size();
    Rcpp::NumericVector out(n);
    Rcpp::NumericVector l_i;
    SEXP ll;
    
    for (int i = 0; i < n; i++) {
      ll = l(i);
      l_i = Rcpp::as<Rcpp::NumericVector>(ll);
      out(i) = sum(l_i);
    }
    
    return out;
  }
  
  Rcpp::NumericVector listSumSq(Rcpp::List l) {
    int n = l.size();
    Rcpp::NumericVector out(n);
    Rcpp::NumericVector l_i;
    SEXP ll;
    
    for (int i = 0; i < n; i++) {
      ll = l(i);
      l_i = Rcpp::as<Rcpp::NumericVector>(ll);
      for (int j = 0; j < l_i.size(); j++) {
        out(i) += pow(l_i(j), 2.0);  
      }
    }

    return out;
  }
  
  Rcpp::NumericVector sqrtVector(Rcpp::NumericVector x) {
    int n = x.size();
    Rcpp::NumericVector out(n);
    for (int i = 0; i < n; i ++) {
      out(i) = sqrt(x(i));
    }
    return out;
  }
  

  
}



