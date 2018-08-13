#include <Rcpp.h>
#include "glicko.h"
using namespace Rcpp;

// [[Rcpp::export]]
List 
  glicko(
    CharacterVector name, 
    std::vector<int> rank,
    NumericVector r, 
    NumericVector rd,
    NumericVector sig,
    NumericVector weight,
    double init_r  = 1500.00,
    double init_rd = 350.00,
    double gamma = 1
  ) {
    
    int n = name.size();
    int idx = 0;
    
    double 
      q   = log(10)/400,
      var  = 0.0,
      err = 0.0,
      rd_;
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
      if( NumericVector::is_na(r[i]) ) 
        r[i] = init_r, rd[i] = init_rd;
      
      
      // modification - rd + time since last event 
      if( (rd[i] * sig[i]) < init_rd ) 
        rd_ = rd[i] * sig[i]; else rd_ = init_rd;
      g_rd[i] = calcGRd( rd_ );
      
    }
    
    // GLICKO RATING
    for(int i = 0; i < n; i++){
      var  = 0;
      err = 0;
      
      for(int j = 0; j < n; j ++){
        if(j != i){
          idx += 1;
          
          team1( idx - 1 ) = name[i];
          team2( idx - 1 ) = name[j];
          
          P( idx - 1 ) = calcPGlicko( calcGRd( sqrt( pow(rd[i],2) + pow( rd[j], 2 ) ) ) , r[i] , r[j] );
          Y( idx - 1 ) = calcZ( rank[i], rank[j] );
          var = calcVar( var, g_rd[j], P( idx - 1) );
          err = calcErr( err, g_rd[j], P( idx - 1 ), rank[i], rank[j]);
          
        }
      }
      
      // this event ratings
      err_i[i]   = err;
      var_i[i]   = var;
      delta_i[i] =  gamma * 1/ ( pow(q, 2) * var );
      
    }
    
    // update parameters 
    for(int i = 0; i < n; i++){
      r[i]     = r[i] + q/( 1/pow(rd[i],2) + 1/delta_i[i] ) * err_i[i] * weight[i];
      rd[i]    = sqrt(  1/( 1/pow(rd[i],2) + 1/delta_i[i] * weight[i] )); 
    }    
    
    Rcpp::List dimnms = Rcpp::List::create(name, name);
    r.names()  = name;
    rd.names() = name;
    
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

// [[Rcpp::export]]
List 
  glicko2(
    CharacterVector name, 
    std::vector<int> rank,
    NumericVector r, 
    NumericVector rd,
    NumericVector sig,
    NumericVector weight,
    double tau = .5,
    double init_r  = 1500.00,
    double init_rd = 350.00
  ) {
    
    int n = name.size();
    int idx = 0;
    double err  = 0.0, var = 0.0, A  = 0.0;
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
          team1( idx - 1 ) = name[i];
          team2( idx - 1 ) = name[j];
          
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
      
      phi[i] = updatePhi(phi[i], var_i[i], sig[i], weight[i]);
      if(phi[i]>(init_rd/173.7178)) phi[i] = init_rd/173.7178;
      
      mu[i]  = updateMu( mu[i], phi[i], err_i[i], weight[i]);
      
      r[i]   = mu2r( mu[i] );
      rd[i]  = phi2rd( phi[i] ); 
    }
    
    
    Rcpp::List dimnms = Rcpp::List::create(name, name);
    r.names()  = name;
    rd.names() = name;
    
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


