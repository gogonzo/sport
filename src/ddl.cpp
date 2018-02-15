#include <RcppArmadillo.h>
using namespace Rcpp;
#include "dll_functions.h"
//' Bayesian Bradley-Terry model for single game
//' 
//' Calculates Glicko ratings based on Bayesian Bradley Terry model.
//' 
//' Algorithm based on 'A Bayesian Approximation Method for Online Ranking' by Ruby C. Weng and Chih-Jen Lin
//' @param teams player/team names.
//' @param rank.
//' @param R Matrix of coefficients (ratings).
//' @param X Matrix of player specifics.
//' @param RD Matrix of coefficients deviations
//' @return \code{r} updated ratings of participats
//' @return \code{rd} updated deviations of participants ratings
//' @return \code{expected} matrix of expected score. \code{expected[i, j] = P(i > j)} 
//' @export
// [[Rcpp::export]]
Rcpp::List 
ddl(
  CharacterVector teams,
  IntegerVector rank,
  NumericMatrix R, 
  NumericMatrix X,
  NumericMatrix RD
  ) {
  int n = R.nrow();
  int k = R.ncol();
  int idx = 0;
  double pi = 3.1415926535;
  double s2;
  double Ks;
  double p;
  double y;
  double y_var;
  double error;
  
  CharacterVector home(n*n-n);
  CharacterVector away(n*n-n);
  NumericVector P(n*n-n);
  NumericVector Y(n*n-n);
  
  NumericVector x_i(k);
  NumericVector x_q(k);
  NumericVector h_i(k);
  NumericVector h_q(k);
  NumericVector s_i(k);
  NumericVector s_q(k);
  NumericVector omega_i(k);
  NumericVector omega_q(k);
  

  NumericMatrix OMEGA( n , k );
  NumericMatrix DELTA( n , k );
  
  // state transition apriori assumption about R and RD, related to `F` and `R` parameters
  //R = R * F;
  //RD = RD * R;
  
  for(int i = 0; i < n; i++){
    x_i = R(i,_);
    h_i = X(i,_);
    s_i = RD(i,_);

    for(int q = 0; q<n; q++ ){
      if(i!=q){
        idx += 1; 
        home( idx - 1 ) = teams(i);
        away( idx - 1 ) = teams(q);
        
        x_q = R(q,_);
        h_q = -X(q,_);
        s_q = RD(q,_);
        
    
        // activation variance
        s2 = sum( x_i * s_i * x_i ) + sum( x_q * s_q * x_q );
        Ks = 1/sqrt( 1 + (pi * s2)/8 );
        
        // probability and output
        p = 1/( 1 + exp( -Ks * ( sum(x_i * h_i) + sum(x_q * h_q) )  ) );
        y = calc_y( rank( i ) , rank( q ) );
        error = y - p;
      
        P( idx - 1 ) = p;
        Y( idx - 1 ) = y;
        
        
        // calculating update
        if(i < q){
          y_var = 1/( 1+p*(1-p)*s2  );
        
          OMEGA(i,_) =  OMEGA(i,_) + ( ( s_i * y_var ) * ( h_i * error ) );
          OMEGA(q,_) =  OMEGA(q,_) + ( ( s_q * y_var ) * ( h_q * error ) );
          DELTA(i,_) = DELTA(i,_) +  
                      ( p*(1 - p) * y_var  ) * 
                      ( s_i * h_i ) * 
                      ( s_i * h_i ) ; 
          
          DELTA(q,_) = DELTA(q,_) +  
            ( p*(1 - p) * y_var  ) * 
            ( s_q * h_q ) * 
            ( s_q * h_q ) ; 
      
        }
      }
    }
  }
  RD += -DELTA;
  R += OMEGA;
  
  Rcpp::List dimnms = Rcpp::List::create(teams, teams);
  rownames(OMEGA) = teams;
  rownames(DELTA) = teams;
  rownames(X)     = teams;
  colnames(OMEGA) = colnames(R);
  colnames(DELTA) = colnames(RD);
  colnames(R)     = colnames(RD);

    
  return Rcpp::List::create(
    Rcpp::Named("R") = R,
    Rcpp::Named("X") = X,
    Rcpp::Named("RD") = RD,
    Rcpp::Named("OMEGA") = OMEGA,
    Rcpp::Named("DELTA") = DELTA,
    Rcpp::Named("pairs") = DataFrame::create(
      Rcpp::Named("home") = home,
      Rcpp::Named("away") = away,
      Rcpp::Named("P") = P,
      Rcpp::Named("Y") = Y
    )
  );
}