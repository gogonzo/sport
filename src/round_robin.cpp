#include <Rcpp.h>
using namespace Rcpp;

// [[Rcpp::export]]
Rcpp::IntegerMatrix round_robin(int N, int G = 1, int R = 1, bool home = true, Rcpp::Nullable<Rcpp::CharacterVector> NAMES = R_NilValue) {
  int NN = (N % 2) == 0 ? N : N + 1,
      K = R * (NN - 1),
      N2 = NN/2,
      idx = 0,
      a, b;
  bool hs;
  IntegerMatrix matches(G * floor(N/2) * K, 4);
  IntegerVector cur_order(NN);
  IntegerVector pre_order(NN);

  for (int g = 0; g < G; g++) {
    hs = true;
    std::iota(cur_order.begin(), cur_order.end(), 1);
    for (int r = 0; r < K; r++) {
      for (int t = 0; t < NN; t++) {
        pre_order(t) = cur_order(t); 
      }
      // switch home <-> away
      if (home) {
        hs = !hs;        
      }
      // round matches
      for(int p = 0; p < N2; p++) {
        a = pre_order(p);
        b = pre_order(NN - p - 1);
        
        if (a <= N && b <= N) {
          matches(idx, 0) = g + 1;
          matches(idx, 1) = r + 1;        
          matches(idx, hs?2:3) = a;
          matches(idx, hs?3:2) = b;         
          idx = idx + 1;
        }
      }
      // reorder
      cur_order(1) = pre_order(NN - 1);   
      for (int k = 2; k < NN; k++) {
        cur_order(k) = pre_order(k - 1);  
      }
    }
  }
  
  colnames(matches) = CharacterVector::create("group", "round", "a", "b");
  return matches;
}