#include <RcppArmadillo.h>
using namespace Rcpp;
#include "bbt_functions.h"
//' Glicko rating for single game
//' 
//' Calculates Glicko rating for single game input
//' 
//' @param teams name of event participants.
//' @param rank classification of the event.
//' @param days days after previous match - indicator multiplying uncertainty of expectations.
//' @param r ratings of participants.
//' @param rd rating deviations of participants.
//' @param init_r initial rating for new competitors (contains NA). Default = 1500
//' @param init_rd initial rating deviations for new competitors. Default = 350
//' @return \code{r} updated ratings of participats
//' @return \code{rd} updated deviations of participants ratings
//' @return \code{expected} matrix of expected score. \code{expected[i, j] = P(i > j)} 
//' @export
// [[Rcpp::export]]
List 
  bbt(
    IntegerVector rank,
    NumericMatrix mi_ij, 
    NumericMatrix sig_ij,
    double kappa=0.95,
    double gamma = 1.0
  ) {
    int n = rank.size();
    int j = mi_ij.ncol();

    NumericVector mi_i  = rowSums(mi_ij);
    NumericVector sig2_i = rowSums(pow_mat_elems(sig_ij));
    NumericMatrix sig_share(n,j);
    NumericMatrix P_iq(n,n);
    NumericVector omega_i(n,0.0);
    NumericVector delta_i(n,0.0);
    double c_iq;
    
    
    for(int i = 0; i < n; i++){

      for(int q = 0; q<n; q++ ){
        if(i!=q){
          c_iq = sqrt( sig2_i(i) + sig2_i(q));
          if(gamma>1) gamma = sqrt(sig2_i(i)) / c_iq;
        
          P_iq(i,q)  = exp( mi_i(i)/c_iq ) / ( exp( mi_i(i)/c_iq ) + exp( mi_i(q)/c_iq) );
          omega_i(i) = 
            omega_i(i) + 
            sig2_i(i)/c_iq * 
            ( calc_s(rank(i), rank(q) ) - P_iq(i,q) );
          
          delta_i(i) = 
            delta_i(i) + 
            gamma * 
            pow( sqrt( sig2_i(i) ) / c_iq, 2.0 ) * 
            ( P_iq(i,q)*(1-P_iq(i,q)) );
        }
      }
    }

    for(int i = 0; i < n; i++){
      sig_share(i,_) = pow(sig_ij(i,_),2.0) / sig2_i(i);
      mi_ij(i,_) = mi_ij(i,_) + sig_share(i,_) * omega_i(i);
      sig_ij(i,_) = sqrt( pow(sig_ij(i,_),2.0) * pmax( 1 - sig_share(i,_) * delta_i(i), kappa ) );
      // if sig_ij < 0.0001 -> sig_ij=0.0001;
    }

    return List::create(
      _["mi_i"]     = mi_i,
      _["sig_i"]    = sig2_i,
      _["mi_ij"]    = mi_ij,
      _["sig_ij"]   = sig_ij,
      _["sig2_i"]   = sig2_i,
      _["sig2_share"]= sig_share,
      _["c_iq"]     = c_iq,
      _["omega"]    = omega_i,
      _["delta"]    = delta_i,
      _["P_iq"]     = P_iq
    );
  }