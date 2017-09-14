#include <Rcpp.h>
#include "glicko_functions.h"
using namespace Rcpp;
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
//' @examples
//'glicko(
//'  teams = c( "A", "B", "C", "D" ), 
//'  rank  = c( 3, 4, 1, 2 ), 
//'  days  = c( 0, 0, 0, 0),
//'  r     = c( 1500, 1400, 1550, 1700 ) , 
//'  rd    = c( 200,  30,   100,  300 ),
//'  init_r  = 1500,
//'  init_rd = 100
//')
//' @export
// [[Rcpp::export]]
List 
  glicko(
    CharacterVector teams, 
    std::vector<int> rank,
    NumericVector r, 
    NumericVector rd,
    NumericVector days = NumericVector::create(0),
    double init_r  = 1500.00,
    double init_rd = 350.00
  ) {
  
    int n = teams.size();
    int d_size = days.size();
    double 
      q   = log(10)/400,
      rd_ = 0.0,
      var  = 0.0,
      err = 0.0;
    NumericVector g_rd(n);
    NumericVector var_i(n);
    NumericVector err_i(n);
    NumericVector delta_i(n);
    NumericMatrix E_s(n, n);
    
    // precalculate 
    for(int i = 0; i < n; i++){
      if(d_size < i + 1) days.push_back(0);
      
      if( NumericVector::is_na(r[i]) ) 
        r[i] = init_r, rd[i] = init_rd;

      rd_ = sqrt( pow(rd[i],2) + pow( days[i], 2 ) );
      if( rd_ < init_rd ) rd[i] = rd_;
      g_rd[i] = calcGRd( rd[i] );
        
    }
    
    // GLICKO RATING
    for(int i = 0; i < n; i++){
      var  = 0;
      err = 0;
      
      for(int j = 0; j < n; j ++){
        if(j != i){
          E_s(i,j) = calcPGlicko( g_rd[j] , r[i] , r[j] );
          var = calcVar( var, g_rd[j], E_s(i,j) );
          err = calcErr( err, g_rd[j], E_s(i,j), rank[i], rank[j]);
        }
      }
      
      // this event ratings
      err_i[i]   = err;
      var_i[i]   = var;
      delta_i[i] = 1/ ( pow(q, 2) * var );
      
    }
    
    // update parameters 
    for(int i = 0; i < n; i++){
      r[i]     = r[i] + q/( 1/pow(rd[i],2) + 1/delta_i[i] ) * err_i[i];
      rd[i]    = sqrt(  1/( 1/pow(rd[i],2) + 1/delta_i[i]) ); 
    }    
    
  Rcpp::List dimnms = Rcpp::List::create(teams, teams);
  E_s.attr("dimnames") = dimnms;
  r.names()  = teams;
  rd.names() = teams;
    
  return List::create(
    _["r"]    = r,
    _["rd"]   = rd,
    _["expected"] = E_s
  );  
}


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
//' @examples
//'glicko2(
//'  teams = c( "A", "B", "C", "D" ), 
//'  rank  = c( 3, 4, 1, 2 ), 
//'  days  = c( 0, 0, 0, 0),
//'  r     = c( 1500, 1400, 1550, 1700 ) , 
//'  rd    = c( 200,  30,   100,  300 ),
//'  sig   = c( .06, .06, .05, .07),
//'  tau   = .5,
//'  init_r  = 1500,
//'  init_rd = 100
//')
//' @export
// [[Rcpp::export]]
List 
  glicko2(
    CharacterVector teams, 
    std::vector<int> rank,
    NumericVector r, 
    NumericVector rd,
    NumericVector sig,
    NumericVector days = NumericVector::create(0),
    double tau = .5,
    double init_r  = 1500.00,
    double init_rd = 350.00
  ) {
    
    int n = teams.size();
    int d_size = days.size();
    double 
      err  = 0.0,
        var = 0.0, 
        A  = 0.0;
    NumericVector mu(n);
    NumericVector phi(n);
    NumericVector g_phi(n);
    NumericVector var_i(n);
    NumericVector err_i(n);
    NumericVector delta_i(n);
    NumericMatrix E_s(n, n);
    
    // precalculate 
    for(int i = 0; i < n; i++){
      if(d_size < i + 1) days.push_back(0);
      if( NumericVector::is_na(r[i]) ) 
        r[i] = init_r, rd[i] = init_rd;
      
      // rescale params to glicko2 scale
      mu[i]    = r2mu( r[i] );
      phi[i]   = rd2phi( rd[i] );
      g_phi[i] = calcGPhi( phi[i] );
      
    }
    
    // Sum deviations from expectations  
    for(int i = 0; i < n; i++){
      var = 0, err  = 0;
      for(int j = 0; j < n; j ++){
        if(j != i){
          E_s(i,j) = calcPGlicko2( g_phi[j] , mu[i] , mu[j] );
          
          var = calcVar(var, g_phi[j], E_s(i,j) );
          err = calcErr(err, g_phi[j], E_s(i,j), rank[i], rank[j]);
        }
      }
      var_i[i]  = 1/var;
      err_i[i] = err;
      delta_i[i] = 1/var * err;
    }
    
    
    // update parameters
    for(int i = 0; i < n; i++){
      // Rcpp::Rcout << "----- i =" << i <<  std::endl;
      A = optimSigma(delta_i[i], sig[i], phi[i], var_i[i], tau);
      sig[i] = exp( A/2 );
      
      phi[i] = updatePhi(phi[i], var_i[i], sig[i]);
      mu[i]  = updateMu( mu[i], phi[i], err_i[i]);
      
      r[i]   = mu2r( mu[i] );
      rd[i]  = phi2rd( phi[i] ); 
    }
    
    
    Rcpp::List dimnms = Rcpp::List::create(teams, teams);
    E_s.attr("dimnames") = dimnms;
    r.names()  = teams;
    rd.names() = teams;
    
    return List::create(
      _["r"]    = r,
      _["rd"]   = rd,
      _["sigma"] = sig,
      _["E_s"] = E_s
    );  
  }


