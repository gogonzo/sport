#include <Rcpp.h>
using namespace Rcpp;
//' Streak length of vector elements
//' 
//' Calculates series of consecutive elements
//' 
//' @param x A single integer.
//' @examples
//' x <- c(1,1,0,0,1,0,3,3,1,1)
//' streakLength(x)
//' @export
// [[Rcpp::export]]
NumericVector running_mean(NumericVector vec) {
  int n = vec.size();
  NumericVector means(n);
  
  if (NumericVector::is_na(vec[0])){
    means[0] = NumericVector::get_na();
  } else {
    means[0] = 1;
  }
  
  for(int i = 1; i < n; ++i) {
    if( vec[i] == vec[i-1]){
      means[i] =  means[i - 1] + 1;
      
    } else if(NumericVector::is_na(vec[i])){
      means[i] = NumericVector::get_na();
      
    } else {
      means[i] = 1;
      
    }
  }
  return means;
}

