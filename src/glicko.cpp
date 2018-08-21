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
    NumericVector sigma,
    NumericVector weight,
    CharacterVector identifier,
    double init_r  = 1500.00,
    double init_rd = 350.00,
    double gamma = 1,
    double kappa = .5
  ) {
    
    int n = name.size();
    int idx = 0;
    
    double 
      q   = log(10)/400,
      var  = 0.0,
      err = 0.0, 
      new_rd_;
    NumericVector g_rd(n);
    NumericVector var_i(n);
    NumericVector err_i(n);
    NumericVector delta_i(n);
    CharacterVector identifierp(n*n-n);
    CharacterVector team1(n*n-n);
    CharacterVector team2(n*n-n);
    NumericVector P(n*n-n);
    NumericVector Y(n*n-n);
    
    // precalculate 
    for(int i = 0; i < n; i++){
      if( NumericVector::is_na(r[i]) ) 
        r[i] = init_r, rd[i] = init_rd;
      if( ( sqrt( pow(rd[i],2) + pow(sigma[i],2)) ) < init_rd ) 
        rd[i] = sqrt( pow(rd[i],2) + pow(sigma[i],2)); else rd[i] = init_rd;
      g_rd[i] = calcGRd( rd[i] );
      
    }
    
    // GLICKO RATING
    for(int i = 0; i < n; i++){
      var  = 0;
      err = 0;
      
      for(int j = 0; j < n; j ++){
        if(j != i){
          idx += 1;
          
          identifierp( idx - 1) = identifier[i];
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
      
      new_rd_ = sqrt(  1/( 1/pow(rd[i],2) + 1/( delta_i[i] * weight[i]) ));
      if( new_rd_ < rd(i) * kappa ) 
        new_rd_ = rd(i) * kappa;
      rd[i]    = new_rd_;
    }
    
    Rcpp::List dimnms = Rcpp::List::create(name, name);
    r.names()  = name;
    rd.names() = name;
    
    return List::create(
      _["r"]    = r,
      _["rd"]   = rd,
      _["r_df"] = DataFrame::create(
        _["name"] = name,
        _["r"]    = r,
        _["rd"]   = rd
      ),
      _["pairs"] = DataFrame::create(
        _["name"] = team1,
        _["opponent"] = team2,
        _["P"] = P,
        _["Y"] = Y
      ),
      _["identifierp"] = identifierp,
      _["identifier"] = identifier
    );  
  } 

// [[Rcpp::export]]
List 
  glicko2(
    CharacterVector name, 
    std::vector<int> rank,
    NumericVector r, 
    NumericVector rd,
    NumericVector sigma,
    NumericVector weight,
    double kappa,
    CharacterVector identifier,
    double tau = .5,
    double init_r  = 1500.00,
    double init_rd = 350.00
  ) {
    
    int n = name.size();
    int idx = 0;
    double err  = 0.0, var = 0.0, A  = 0.0, new_rd_;
    NumericVector mu(n);
    NumericVector phi(n);
    NumericVector g_phi(n);
    NumericVector var_i(n);
    NumericVector err_i(n);
    NumericVector delta_i(n);
    CharacterVector identifierp(n*n-n);
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
          identifierp( idx - 1) = identifier[i];
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
      A = optimSigma(delta_i[i], sigma[i], phi[i], var_i[i], tau);
      sigma[i] = exp( A/2 );
      
      phi[i] = updatePhi(phi[i], var_i[i], sigma[i], weight[i]);
      if(phi[i]>(init_rd/173.7178)) phi[i] = init_rd/173.7178;
      mu[i]  = updateMu( mu[i], phi[i], err_i[i], weight[i]);
      
      r[i]   = mu2r( mu[i] );
      new_rd_ = phi2rd( phi[i] );
      if( new_rd_ < rd(i) * kappa ) new_rd_ = rd(i) * kappa;
      rd[i]    = new_rd_;
    
    }
    
    
    Rcpp::List dimnms = Rcpp::List::create(name, name);
    r.names()  = name;
    rd.names() = name;
    
    return List::create(
      _["r"]     = r,
      _["rd"]    = rd,
      _["sigma"] = sigma,
      _["r_df"]  = DataFrame::create(
        _["name"] = name,
        _["r"]    = r,
        _["rd"]   = rd,
        _["sigma"] = sigma,
        _["stringsAsFactors"] = false
      ),
      _["pairs"] = DataFrame::create(
        _["name"]     = team1,
        _["opponent"] = team2,
        _["P"] = P,
        _["Y"] = Y,
        _["stringsAsFactors"] = false
      ),
      _["identifierp"] = identifierp,
      _["identifier"]  = identifier
    );  
  } 



