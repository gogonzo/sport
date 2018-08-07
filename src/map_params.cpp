#include <Rcpp.h>
using namespace Rcpp;
#include "mapParams.h"

//' Dummy matrix from factor
//'
//' Dummy matrix from factor
//' @export
// [[Rcpp::export]]
IntegerMatrix factor2dummy( SEXP variable, int type ) {
  switch ( TYPEOF( variable ) ) {
    case INTSXP:  return impl::factor2dummy_<INTSXP>(as<IntegerVector>( variable ));
    case REALSXP: return impl::factor2dummy_<REALSXP>(as<NumericVector>( variable ));
    case STRSXP:  return impl::factor2dummy_<STRSXP>(as<CharacterVector>( variable ));
    default: {
      warning(
        "Invalid SEXPTYPE %d (%s).\n",
        TYPEOF( variable ), type2name( variable )
      );
      return 0;
    }
  }
}