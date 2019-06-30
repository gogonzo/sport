#include <Rcpp.h>
using namespace Rcpp;

// [[Rcpp::export]]
Rcpp::IntegerMatrix ko(int G = 1, int M = 1, bool home = true, Rcpp::Nullable<Rcpp::CharacterVector> NAMES = R_NilValue) {
  IntegerMatrix matches(G * M, 4);
  
  int idx = 0;
  for (int g = 0; g < G; g++) {
    for(int m = 0; m < M; m++) {
      matches(idx, 0) = g + 1;
      matches(idx, 1) = m + 1;
      
      if(home) {
        matches(idx, m%2?3:2) = g * 2 + 1;
        matches(idx, m%2?2:3) = g * 2 + 2;        
      } else {
        matches(idx, 2) = g * 2 + 1;
        matches(idx, 3) = g * 2 + 2;
      }

      idx += 1; 
    }
  }
  
  
  colnames(matches) = CharacterVector::create("group", "round", "a", "b");
  
  return matches;
}