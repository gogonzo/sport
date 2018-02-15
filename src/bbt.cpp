#include <RcppArmadillo.h>
using namespace Rcpp;
#include "bbt_functions.h"
//' Bayesian Bradley-Terry model for single game
//' 
//' Calculates Glicko ratings based on Bayesian Bradley Terry model.
//' 
//' Algorithm based on 'A Bayesian Approximation Method for Online Ranking' by Ruby C. Weng and Chih-Jen Lin
//' @param rank.
//' @param r ratings of participants.
//' @param rd rating deviations of participants.
//' @param kappa
//' @param gamma
//' @return \code{r} updated ratings of participats
//' @return \code{rd} updated deviations of participants ratings
//' @return \code{expected} matrix of expected score. \code{expected[i, j] = P(i > j)} 
//' @export
// [[Rcpp::export]]
List 
  bbt(
    CharacterVector teams,
    IntegerVector rank,
    NumericMatrix r, 
    NumericMatrix rd,
    double kappa=0.0001,
    double gamma = 1.0
  ) {
    int n = rank.size();
    int j = r.ncol();
    int idx = 0;

    NumericVector mi  = rowSums(r);
    NumericVector sig2 = rowSums( pow_mat_elems(rd) );
    NumericMatrix rd_share(n,j);
    NumericVector omega(n,0.0);
    NumericVector delta(n,0.0);
    CharacterVector home(n*n-n);
    CharacterVector away(n*n-n);
    NumericVector P(n*n-n);
    NumericVector Y(n*n-n);
    
    double c;
    double init_r_i  = 25;
    double init_rd_i = 25/3;
    double beta = (25/6);
    
    
    for(int i = 0; i < n; i++){

      for(int q = 0; q<n; q++ ){
        if(i!=q){

          c = sqrt( sig2(i) + sig2(q) + pow(beta,2) );
          
          if(gamma==1) gamma = sqrt( sig2(i) ) / c;
        
          idx += 1;
          home( idx - 1 ) = teams[i];
          away( idx - 1 ) = teams[q];
          
          Y( idx - 1 ) = calc_s( rank(i), rank(q) );
          P( idx - 1 )  = exp( mi(i)/c ) / ( exp( mi(i)/c ) + exp( mi(q)/c) );
          
          omega(i) = 
            omega(i) + 
            sig2(i)/c * 
            ( calc_s( rank(i), rank(q) ) - P( idx - 1 ) );
          
          delta(i) = 
            delta(i) + 
            gamma * 
            pow( sqrt( sig2(i) ) / c, 2.0 ) * 
            ( P(idx - 1) * ( 1-P(idx - 1) ) );
        }
      }
    }

    for(int i = 0; i < n; i++){
      rd_share(i,_) = pow(rd(i,_),2.0) / sig2(i);
      
      r(i,_) = r(i,_) + rd_share(i,_) * omega(i);
      
      rd(i,_) = 
        sqrt( 
          pow(rd(i,_),2.0) * 
          pmax( 1 - rd_share(i,_) * delta(i) , kappa ) 
        );
    }
    

    return List::create(
      _["r"] = r,
      _["rd"]= rd,
      _["pairs"] = DataFrame::create(
        _["home"] = home,
        _["away"] = away,
        _["P"] = P,
        _["Y"] = Y
      )
    );
  }