#include <RcppArmadillo.h>
using namespace Rcpp;
#include "dbl.h"

// [[Rcpp::export]]
Rcpp::List 
  dbl(
    CharacterVector name,
    IntegerVector rank,
    NumericMatrix X,
    NumericVector R, 
    NumericVector RD,
    NumericVector beta,
    NumericVector weight,
    CharacterVector identifier,
    double tau = 0.05
  ) {
  int n = X.nrow();
  int k = X.ncol();
  int idx = 0;
  double pi = 3.1415926535;
  double s2;
  double Ks;
  double p;
  double y;
  double y_var;
  double error;
  
  CharacterVector team2(n*n-n);
  CharacterVector team1(n*n-n);
  CharacterVector identifierp(n*n-n);
  NumericVector P(n*n-n);
  NumericVector Y(n*n-n);
  NumericVector r_update(k);
  NumericVector rd_update(k);
  NumericVector x_i(k);
  NumericVector x_q(k);
  NumericVector h_i(k);
  NumericVector h_q(k);
  NumericVector s_i(k);
  NumericVector s_q(k);
  double delta_;
  
  NumericMatrix OMEGA( n , k );
  NumericMatrix DELTA( n , k );
  
  // state transition apriori assumption about R and RD, related to `F` and `R` parameters
  //R = R * F;
  //RD = RD * R;
  
  for(int i = 0; i < n; i++){
    x_i = R;
    h_i = X(i,_);
    s_i = RD;
    
    for(int q = 0; q<n; q++ ){
      if(i!=q){
        idx += 1;
        identifierp( idx - 1 ) = identifier(i);
        team1( idx - 1 ) = name(i);
        team2( idx - 1 ) = name(q);
        
        x_q = R;
        h_q = -X(q,_);
        s_q = RD;

        // activation variance
        s2 = sum( h_i * s_i * h_i ) * beta(i) + sum( h_q * s_q * h_q ) * beta(q);
        Ks = 1/sqrt( 1 + pi * s2/8 );
        
        // probability and output
        p = 1/( 1 + exp( -Ks * (sum(x_i * h_i) + sum(x_q * h_q))  ) );
        y = dlr_calc_y( rank( i ) , rank( q ) );
        error = y - p;
        
        P( idx - 1 ) = p;
        Y( idx - 1 ) = y;
        
        // calculating update
        y_var = 1/( 1+p*(1-p)*s2  );

        
        OMEGA(i,_) =  OMEGA(i,_) + ( ( s_i * y_var ) * ( h_i * error ) );
        OMEGA(q,_) =  OMEGA(q,_) + ( ( s_q * y_var ) * ( h_q * error ) );
        DELTA(i,_) = DELTA(i,_) +  
          ( p*(1 - p) * y_var  ) * 
          ( s_i * h_i ) * 
          ( s_i * h_i ); 
        
        DELTA(q,_) = DELTA(q,_) + 
          ( p * (1 - p) * y_var  ) * 
          ( s_q * h_q ) * 
          ( s_q * h_q ); 
      }
    }
  }
  
  for(int i = 0; i < k; i++){
    delta_ = -sum( DELTA(_,i) * weight ) / 2 / (k*1.0 - 1.0);
    if(  -delta_ > RD(i)*tau) 
      delta_ = RD(i) * tau * delta_/std::abs(delta_);
    RD(i) += delta_; 
    R(i)  += sum( OMEGA(_,i) * weight ) / 2;
  }

  // UPDATE according to mapping

  r_update.names()  = colnames(X);
  rd_update.names() = colnames(X);
  
  return Rcpp::List::create(
    Rcpp::Named("r") = R,
    Rcpp::Named("rd") = RD,
    
    Rcpp::Named("r_df") = DataFrame::create(
      Rcpp::Named("name") = colnames(X),
      Rcpp::Named("r") = R,
      Rcpp::Named("rd") = RD,
      Rcpp::Named("stringsAsFactors") = false
    ),
    Rcpp::Named("pairs") = DataFrame::create(
      Rcpp::Named("name") = team1,
      Rcpp::Named("opponent") = team2,
      Rcpp::Named("P") = P,
      Rcpp::Named("Y") = Y,
      Rcpp::Named("stringsAsFactors") = false
    ),
    Rcpp::Named("identifierp") = identifierp,
    Rcpp::Named("identifier") = identifier
  );
}