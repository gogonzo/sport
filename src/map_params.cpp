#include <RcppArmadillo.h>
using namespace Rcpp;
NumericMatrix
  mapParams(
    CharacterMatrix map,
    NumericVector params
  ){
    int n = map.nrow();
    int k = map.ncol();
    LogicalVector idx;
    NumericMatrix params_out(n,k);
    CharacterVector params_names = params.names();
    
    
    for(int i=0; i<n; i++){
      for(int j=0; i<k; i++){
        //bool idx = params_names == map(i,j);
        params_out(i,j) = params(0);
      
      }      
    }
    
    return(params_out);
  }