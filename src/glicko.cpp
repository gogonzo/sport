#include <Rcpp.h>
#include "glicko_functions.h"
using namespace Rcpp;
//' Glicko rating for single game
//'
//' Calculates Glicko rating for single game input
//' 
//' @param team_name name of event participants.
//' @param rank classification of the event.
//' @param time time after previous match - indicator multiplying uncertainty of expectations.
//' @param r ratings of participants.
//' @param rd rating deviations of participants.
//' @param init_r initial rating for new competitors (contains NA). Default = 1500
//' @param init_rd initial rating deviations for new competitors. Default = 350
//' @return \code{r} updated ratings of participats
//' @return \code{rd} updated deviations of participants ratings
//' @return \code{expected} matrix of expected score. \code{expected[i, j] = P(i > j)} 
//' @examples
//'glicko(
//'  team_name = c( "A", "B", "C", "D" ), 
//'  rank  = c( 3, 4, 1, 2 ), 
//'  time  = c( 0, 0, 0, 0),
//'  r     = c( 1500, 1400, 1550, 1700 ) , 
//'  rd    = c( 200,  30,   100,  300 ),
//'  init_r  = 1500,
//'  init_rd = 100
//')
//'glicko(
//' team_name = c( "A", "B" ), 
//'   rank  = c( 1, 2 ), 
//'   time  = c( 0, 0),
//'   r     = c( 1400, 1500 ) , 
//'   rd    = c( 80,  150),
//'   init_r  = 1500,
//'   init_rd = 100
//')
//' @export
// [[Rcpp::export]]
List 
  glicko(
    CharacterVector team_name, 
    std::vector<int> rank,
    NumericVector r, 
    NumericVector rd,
    NumericVector time = NumericVector::create(0),
    double init_r  = 1500.00,
    double init_rd = 350.00,
    double gamma = 1
  ) {
    
    int n = team_name.size();
    int d_size = time.size();
    int idx = 0;
    
    double 
      q   = log(10)/400,
        rd_ = 0.0,
        var  = 0.0,
        err = 0.0;
    NumericVector g_rd(n);
    NumericVector var_i(n);
    NumericVector err_i(n);
    NumericVector delta_i(n);
    CharacterVector team1(n*n-n);
    CharacterVector team2(n*n-n);
    NumericVector P(n*n-n);
    NumericVector Y(n*n-n);
    
    // precalculate 
    for(int i = 0; i < n; i++){
      if(d_size < i + 1) time.push_back(0);
      
      if( NumericVector::is_na(r[i]) ) 
        r[i] = init_r, rd[i] = init_rd;
      
      
      // modification - rd + time since last event 
      rd_ = sqrt( pow(rd[i],2) + pow( time[i], 2 ) );
      if( rd_ < init_rd ) rd[i] = rd_;
      g_rd[i] = calcGRd( rd[i] );
      
    }
    
    // GLICKO RATING
    for(int i = 0; i < n; i++){
      var  = 0;
      err = 0;
      
      for(int j = 0; j < n; j ++){
        if(j != i){
          idx += 1;
          
          team1( idx - 1 ) = team_name[i];
          team2( idx - 1 ) = team_name[j];
          
          P( idx - 1 ) = calcPGlicko( calcGRd( sqrt( pow(rd[i],2) + pow( rd[j], 2 ) ) ) , r[i] , r[j] );
          Y( idx - 1 ) = calcZ( rank[i], rank[j] );
          var = calcVar( var, g_rd[j], P( idx - 1) );
          err = calcErr( err, g_rd[j], P( idx -1 ), rank[i], rank[j]);
        }
      }
      
      // this event ratings
      err_i[i]   = err;
      var_i[i]   = var;
      delta_i[i] =  gamma * 1/ ( pow(q, 2) * var );
      
    }
    
    // update parameters 
    for(int i = 0; i < n; i++){
      r[i]     = r[i] + q/( 1/pow(rd[i],2) + 1/delta_i[i] ) * err_i[i];
      rd[i]    = sqrt(  1/( 1/pow(rd[i],2) + 1/delta_i[i]) ); 
    }    
    
    Rcpp::List dimnms = Rcpp::List::create(team_name, team_name);
    r.names()  = team_name;
    rd.names() = team_name;
    
    return List::create(
      _["r"]    = r,
      _["rd"]   = rd,
      _["pairs"] = DataFrame::create(
        _["team1"] = team1,
        _["team2"] = team2,
        _["P"] = P,
        _["Y"] = Y
      )
    );  
  } 



//' Glicko2 rating for single game
//' 
//' Calculates Glicko2 rating for single game input
//' 
//' @param team_name name of event participants.
//' @param rank classification of the event.
//' @param time time after previous match - indicator multiplying uncertainty of expectations.
//' @param r ratings of participants.
//' @param rd rating deviations of participants.
//' @param init_r initial rating for new competitors (contains NA). Default = 1500
//' @param init_rd initial rating deviations for new competitors. Default = 350
//' @return \code{r} updated ratings of participats
//' @return \code{rd} updated deviations of participants ratings
//' @return \code{expected} matrix of expected score. \code{expected[i, j] = P(i > j)} 
//' @examples
//'glicko2(
//'  team_name = c( "A", "B", "C", "D" ), 
//'  rank  = c( 3, 4, 1, 2 ), 
//'  time  = c( 0, 0, 0, 0),
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
    CharacterVector team_name, 
    std::vector<int> rank,
    NumericVector r, 
    NumericVector rd,
    NumericVector sig,
    NumericVector time = NumericVector::create(0),
    double tau = .5,
    double init_r  = 1500.00,
    double init_rd = 350.00
  ) {
    
    int n = team_name.size();
    int d_size = time.size();
    int idx = 0;
    double err  = 0.0, var = 0.0, A  = 0.0, rd_;
    NumericVector mu(n);
    NumericVector phi(n);
    NumericVector g_phi(n);
    NumericVector var_i(n);
    NumericVector err_i(n);
    NumericVector delta_i(n);
    CharacterVector team1(n*n-n);
    CharacterVector team2(n*n-n);
    NumericVector P(n*n-n);
    NumericVector Y(n*n-n);
    
    
    // precalculate 
    for(int i = 0; i < n; i++){
      if(d_size < i + 1) time.push_back(0);
      if( NumericVector::is_na(r[i]) ) 
        r[i] = init_r, rd[i] = init_rd;
      
      // modification - rd + time since last event  
      rd_ = sqrt( pow(rd( i ),2) + pow( time( i ), 2 ) );
      if( rd_ < init_rd ) rd[i] = rd_;
      
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
          idx += 1;
          team1( idx - 1 ) = team_name[i];
          team2( idx - 1 ) = team_name[j];
          
          Y( idx - 1 ) = calcZ( rank[i], rank[j] );
          P( idx - 1 ) = calcPGlicko2( sqrt( pow(g_phi[i],2.0) + pow(g_phi[j],2.0) ) , mu[i] , mu[j] );
          
          var = calcVar(var, g_phi[j], P( idx - 1 ) );
          err = calcErr(err, g_phi[j], P( idx - 1 ), rank[i], rank[j]);
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
      if(phi[i]>(init_rd/173.7178)) phi[i] = init_rd/173.7178;
      
      mu[i]  = updateMu( mu[i], phi[i], err_i[i]);
      
      r[i]   = mu2r( mu[i] );
      rd[i]  = phi2rd( phi[i] ); 
    }
    
    
    Rcpp::List dimnms = Rcpp::List::create(team_name, team_name);
    r.names()  = team_name;
    rd.names() = team_name;
    
    return List::create(
      _["r"]    = r,
      _["rd"]   = rd,
      _["sigma"] = sig,
      _["pairs"] = DataFrame::create(
        _["team1"] = team1,
        _["team2"] = team2,
        _["P"] = P,
        _["Y"] = Y
      )
    );  
  } 


