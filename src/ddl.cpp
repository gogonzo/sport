#include <RcppArmadillo.h>
// [[Rcpp::depends(RcppArmadillo)]]
#include "dll_functions.h"
//' Bayesian Bradley-Terry model for single game
//' 
//' Calculates Glicko ratings based on Bayesian Bradley Terry model.
//' 
//' Algorithm based on 'A Bayesian Approximation Method for Online Ranking' by Ruby C. Weng and Chih-Jen Lin
//' @param rank.
//' @param mi_ij ratings of participants.
//' @param sig_ij rating deviations of participants.
//' @param kappa
//' @param gamma
//' @return \code{r} updated ratings of participats
//' @return \code{rd} updated deviations of participants ratings
//' @return \code{expected} matrix of expected score. \code{expected[i, j] = P(i > j)} 
//' @export
// [[Rcpp::export]]
Rcpp::List 
ddl(
  Rcpp::CharacterVector teams,
  Rcpp::IntegerVector rank,
  arma::mat X, 
  arma::mat H,
  arma::mat S

  ) {
  int n = X.n_rows;
  int k = X.n_cols;
  int idx = 0;
  arma::uvec pairs_blocks;
  arma::uvec idx_i;
  arma::uvec idx_q;
  arma::uvec idxs;
  arma::uvec col1;
  double pi = 3.1415926535;
  double s2;
  double Ks;
  double p;
  double y;
  double y_var;
  double error;
  
  
  arma::vec ha;
  ha << 1 << -1;
  col1 << 0;
  
  arma::mat OMEGA( n , k );
  arma::mat DELTA( k*n , k*n );
  
  // state transition apriori assumption about X and S, related to `F` and `R` parameters
  //X = X * F;
  //S = S * R;
  
  for(int i = 0; i < n; i++){
    idx_i << i;
    for(int q = 0; q<n; q++ ){
      if(i!=q){
        idx += 1; 
        idxs << i << q;
        idx_q << q;
        pairs_blocks << (i+1)*2-2 << (i+1)*2-1 << (q+1)*2-2 << (q+1)*2-1;
        
        arma::mat s  = S.submat( pairs_blocks , pairs_blocks ); 
        arma::mat x  = join_rows( X.rows( idx_i ), X.rows( idx_q )); 
        arma::mat h  = join_rows( H.rows( idx_i ), -H.rows( idx_q ));
        
        // activation variance
        s2 = arma::as_scalar( x * s * trans(x) );
        Ks = 1/sqrt( 1 + (pi * s2)/8 );
        
        // probability and output
        p = 1/( 1 + exp( -Ks * as_scalar( h * trans(x) )) );
        y = arma::as_scalar( calc_y( rank( i ) , rank( q ) ) );
        error = arma::as_scalar( p - y);
        
        // calculating update
        if(i < q){
          
          y_var = 1/( 1+p*(1-p)*s2  );
          arma::mat omega = ( s * arma::as_scalar(y_var) ) * trans(h) * arma::as_scalar(error);
          arma::mat delta = arma::as_scalar(  p*(1-p)/y_var ) * ( ( s*trans(h)) * trans( s*trans(h) ));

          OMEGA.rows( idxs ) =  OMEGA.rows( idxs ) +  trans( reshape(omega,2,k) ) ;
          DELTA( pairs_blocks, pairs_blocks ) = DELTA( pairs_blocks, pairs_blocks ) + delta; 
      }
      }
    }
  }
  S = S - DELTA;
  X = X + OMEGA;
  
  return Rcpp::List::create(
    Rcpp::Named("ranks") = rank,
    Rcpp::Named("X") = X,
    Rcpp::Named("S") = S,
    Rcpp::Named("OMEGA") = OMEGA,
    Rcpp::Named("DELTA") = DELTA
  );
}