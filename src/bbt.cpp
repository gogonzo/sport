#include <RcppArmadillo.h>
using namespace Rcpp;
#include "bbt.h"
//' Bayesian Bradley-Terry model for single game
//' 
//' Calculates Glicko ratings based on Bayesian Bradley Terry model.
//' 
//' Algorithm based on 'A Bayesian Approximation Method for Online Ranking' by Ruby C. Weng and Chih-Jen Lin
//' @param name of player.
//' @param rank in event.
//' @param r ratings of player.
//' @param rd rating deviations of player.
//' @param sig name of column in `data` containing rating volatility. Rating volitality is a value which multiplies prior `rd`. If `sig > 1` then prior `rd` increases, making estimate of `r` more uncertain.
//' @param weight name of column in `data` containing weights. Weights multiplies step update increasing/decreasing step impact on ratings estimates.
//' @param kappa small positive value to ensure rd positive after update. Higher value of `kappa` limits `rd` change size, and lower value of `kappa` allows `rd` update to be bigger. By default `kappa=0.0001`
//' @param gamma can help to control how fast the variance `rd` is reduced after updating. Lower `gamma` slow down decreasing of `rd`, which tends to reach zero to quickly. The default value is `gamma = rd/c`.
//' @param beta additional variance of performance. By default `beta = 25/6`.
//' @param init_r initial rating for new competitors (contains NA). Default = 25
//' @param init_rd initial rating deviations for new competitors. Default = 25/3
//' @return \code{r} updated ratings of participats
//' @return \code{rd} updated deviations of participants ratings
//' @return \code{expected} matrix of expected score. \code{expected[i, j] = P(i > j)} 
//' @export
// [[Rcpp::export]]
List 
  bbt(
    CharacterVector name,
    IntegerVector rank,
    NumericMatrix r, 
    NumericMatrix rd,
    NumericVector sig,
    NumericVector weight,
    double kappa=0.0001,
    double gamma = 1.0,
    double beta = 25/6,
    double init_r = 25,
    double init_rd = 25/3
  ) {
    int n = rank.size();
    int j = r.ncol();
    int idx = 0;

    NumericVector mi  = rowSums(r);
    NumericVector sig2 = rowSums( pow_mat_elems(rd) );
    NumericMatrix rd_share(n,j);
    NumericVector omega(n,0.0);
    NumericVector delta(n,0.0);
    CharacterVector team1(n*n-n);
    CharacterVector team2(n*n-n);
    NumericVector P(n*n-n);
    NumericVector Y(n*n-n);
    
    double c, rd_;
    
    for(int i = 0; i < n; i++){
      if( (rd[i] * sig[i]) < init_rd ) rd_ = rd[i] * sig[i]; else rd_ = init_rd;
      rd[ i ] = rd_;
    }
    
    for(int i = 0; i < n; i++){
      for(int q = 0; q<n; q++ ){
        if(i!=q){

          c = sqrt( sig2(i) + sig2(q) + pow(beta,2) );
          if(gamma==1) gamma = sqrt( sig2(i) ) / c;
        
          idx += 1;
          team1( idx - 1 ) = name[i];
          team2( idx - 1 ) = name[q];
          
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
      
      r(i,_) = r(i,_) + rd_share(i,_) * omega(i) * weight(i);
      rd(i,_) = sqrt( 
          pow(rd(i,_),2.0) * 
          weight(i) *
          pmax( 1 - rd_share(i,_) * delta(i) , kappa ) 
        );
    }
    

    return List::create(
      _["r"] = r,
      _["rd"]= rd,
      _["pairs"] = DataFrame::create(
        _["team1"] = team1,
        _["team2"] = team2,
        _["P"] = P,
        _["Y"] = Y
      )
    );
  }