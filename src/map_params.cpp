#include <Rcpp.h>
using namespace Rcpp;
#include "map_params.h"

// [[Rcpp::export]]
IntegerMatrix factor2dummy( SEXP factor ) {
  switch ( TYPEOF( factor ) ) {
      case INTSXP:  return impl::factor2dummy_<INTSXP>(as<IntegerVector>( factor ));
      case REALSXP: return impl::factor2dummy_<REALSXP>(as<NumericVector>( factor ));
      case STRSXP:  return impl::factor2dummy_<STRSXP>(as<CharacterVector>( factor ));
    default: {
      warning(
        "Invalid SEXPTYPE %d (%s).\n",
        TYPEOF( factor ), type2name( factor )
      );
      return 0;
    }
  }
}