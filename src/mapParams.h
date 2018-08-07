#include <Rcpp.h>
using namespace Rcpp;

namespace impl {

  // one factor
  template <int RTYPE>
  IntegerMatrix factor2dummy_( const Vector<RTYPE>& x ){
    const Vector<RTYPE> levels = unique(x);
    IntegerMatrix dummy( x.size() , levels.size() );
    for(int i = 0; i < x.size(); i++)
      for(int j = 0; j < levels.size(); j++)
        if( x(i) == levels(j) ) dummy(i,j) = 1;

    return(dummy);
  }
  
  // factors interactions 
  template <int RTYPE>
  IntegerMatrix factors2dummy_( const Vector<RTYPE>& x, const Vector<RTYPE>& y){
    const Vector<RTYPE> levels = unique(x + y);
    IntegerMatrix dummy( x.size() , levels.size() );
    for(int i = 0; i < x.size(); i++)
      for(int j = 0; j < levels.size(); j++)
        if( x(i) == levels(j) ) dummy(i,j) = 1;
        
    return(dummy);
  }
  
  // numeric:factor interaction
  template <int RTYPE>
  NumericMatrix numeric2dummy_( NumericVector y, const Vector<RTYPE>& x ){
    const Vector<RTYPE> levels = unique(x);
    NumericMatrix term( x.size() , levels.size() );
    for(int i = 0; i < x.size(); i++)
      for(int j = 0; j < levels.size(); j++)
        if( x(i) == levels(j) ) term(i,j) = y(i);
        
    return(term);
  }
  
  // numeric:numeric interaction
  template <int RTYPE>
  NumericVector numerics2dummy_( NumericVector y, const NumericVector x ){
    NumericVector term( x.size() );
    return(term);
  }
  
}
