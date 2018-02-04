#include <RcppArmadillo.h>
using namespace Rcpp;
#include "gonzo_functions.h"
//' Gonzo rating for single game
//' 
//' Calculates Gonzo rating for single game input
//' 
//' @export
// [[Rcpp::export]]
List gonzo(  
    IntegerVector rank,
    NumericVector mi_i, 
    NumericVector sig_i) {
  
  int n = rank.size();
  double c_iq = 0.0;
  double log_lik = 0.0;
  double lik_i = 1.0;
  NumericMatrix P_iq(n,n);
  NumericMatrix Lik_iq(n,n);
  NumericVector Lik_i(n);
  
  for( int i=0; i < n; i++){
    lik_i = 1.0;
    for( int q=0; q < n; q++){
      if(i!=q){
        c_iq = sqrt( pow( sig_i(i),2) + pow(sig_i(q),2));
        P_iq(i,q)  = exp( mi_i(i)/c_iq ) / ( exp( mi_i(i)/c_iq ) + exp( mi_i(q)/c_iq ) );
        Lik_iq(i,q)  = calcLik( P_iq(i,q), rank(i), rank(q));
        log_lik += log( Lik_iq(i,q) ); 
        lik_i *= Lik_iq(i,q);
      }
    }
    Lik_i(i) = lik_i;      
  }
  
  return List::create(
    _["P_iq"] = P_iq,
    _["Lik_iq"] = Lik_iq,
    _["Lik_i"] = Lik_i,
    _["log_lik"] = log_lik
    
  );
}