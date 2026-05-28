#include <unordered_set>
#include <unordered_map>
#include <string>
#include <vector>

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


  // O(n) unique for StringVector using unordered_set
  inline Rcpp::StringVector unique(Rcpp::StringVector x) {
    int n = x.size();
    if (n <= 1) return x;

    std::unordered_set<std::string> seen;
    seen.reserve(n);
    Rcpp::StringVector res;

    for (int i = 0; i < n; i++) {
      std::string val = Rcpp::as<std::string>(x(i));
      if (seen.insert(val).second) {
        res.push_back(x(i));
      }
    }

    return res;
  }

  // O(n) unique for IntegerVector using unordered_set
  inline Rcpp::IntegerVector unique(Rcpp::IntegerVector x) {
    int n = x.size();
    if (n <= 1) return x;

    std::unordered_set<int> seen;
    seen.reserve(n);
    Rcpp::IntegerVector res;

    for (int i = 0; i < n; i++) {
      int val = x(i);
      if (seen.insert(val).second) {
        res.push_back(x(i));
      }
    }

    return res;
  }
}
