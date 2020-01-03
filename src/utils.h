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
}



