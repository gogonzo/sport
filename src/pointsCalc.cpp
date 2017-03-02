#include <Rcpp.h>
using namespace Rcpp;
//' Calculate points
//' 
//' Calculates points of \code{event_id} participant with given parameters
//' 
//' @param x A single integer.
//' @export
// [[Rcpp::export]]
NumericVector pointsCalc(NumericVector event_id, CharacterVector team_name, NumericVector score) {
  int n = event_id.size();
  NumericVector points(n);
  for(int i = 0 ; i < n ; i++ ){
    points[i] = score[i] * 2;    
    
  }

  
  return points;
}