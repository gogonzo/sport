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

    colnames(dummy) = levels;
    return(dummy);
  }
}
  
