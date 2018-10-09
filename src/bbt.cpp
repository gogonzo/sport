#include <Rcpp.h>
using namespace Rcpp;
#include "bbt.h"

// [[Rcpp::export]]
List 
  bbt(
    CharacterVector name,
    IntegerVector rank,
    NumericMatrix r, 
    NumericMatrix rd,
    NumericVector sigma,
    NumericVector weight,
    CharacterVector identifier,
    double kappa = 0.5,
    double gamma = 999.0,
    double beta = 25/6,
    double init_r = 25.0,
    double init_rd = 25/3
  ) {
    int n = rank.size();
    int j = r.ncol();
    int idx = 0;

    NumericVector mi  = rowSums(r);
    NumericVector sigma2 = rowSums( pow_mat_elems(rd) );
    NumericMatrix rd_share(n,j);
    NumericVector omega(n,0.0);
    NumericVector delta(n,0.0);
    CharacterVector identifierp(n*n-n);
    CharacterVector team1(n*n-n);
    CharacterVector team2(n*n-n);
    NumericVector P(n*n-n);
    NumericVector Y(n*n-n);
    NumericVector rd_update(j);
    double c;

    
    for(int i = 0; i < n; i++){
      if( ( sqrt( pow(rd[i],2) + pow(sigma[i],2.0)) ) < init_rd ) 
        rd[i] = sqrt( pow(rd[i],2) + pow(sigma[i],2.0)); else rd[i] = init_rd;
    }
    
    for(int i = 0; i < n; i++){
      for(int q = 0; q<n; q++ ){
        if(i!=q){

          c = sqrt( sigma2(i) + sigma2(q) + pow(beta,2.0) );
          if(gamma==999) gamma = sqrt( sigma2(i) ) / c;
        
          idx += 1;
          identifierp( idx - 1 ) = identifier[i];
          team1( idx - 1 ) = name[i];
          team2( idx - 1 ) = name[q];
          
          Y( idx - 1 ) = calc_s( rank(i), rank(q) );
          P( idx - 1 ) = exp( mi(i)/c ) / ( exp( mi(i)/c ) + exp( mi(q)/c) );
          
          omega(i) = 
            omega(i) + 
            sigma2(i)/c * 
            ( Y( idx - 1 ) - P( idx - 1 ) );
          
          delta(i) = 
            delta(i) + 
            gamma * 
            pow( sqrt( sigma2(i) ) / c, 2.0 ) * 
            ( P(idx - 1) * ( 1-P(idx - 1) ) );
        }
      }
    }

    for(int i = 0; i < n; i++){
      rd_share(i,_) = pow(rd(i,_),2.0) / sigma2(i);
      
      r(i,_) = r(i,_) + rd_share(i,_) * omega(i) * weight(i);
      rd_update = ( rd(i,_) - 
                    sqrt(  pow(rd(i,_),2.0) * ( 1 - rd_share(i,_) * delta(i)))
                  ) * weight(i);

      
      // kappa for all participants
      rd(i,_) = pmax( rd(i,_) - rd_update , rd(i,_) * kappa );
    }

    return List::create(
      _["r"]     = r,
      _["rd"]    = rd,
      _["r_df"]  = DataFrame::create(
        _["name"] = name,
        _["r"]    = r,
        _["rd"]   = rd,
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