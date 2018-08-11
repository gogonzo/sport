// Generated by using Rcpp::compileAttributes() -> do not edit by hand
// Generator token: 10BE3573-1514-4C36-9D1C-5A225CD40393

#include <RcppArmadillo.h>
#include <Rcpp.h>

using namespace Rcpp;

// bbt
List bbt(CharacterVector name, IntegerVector rank, NumericMatrix r, NumericMatrix rd, NumericVector sig, NumericVector weight, double kappa, double gamma, double beta, double init_r, double init_rd);
RcppExport SEXP _sport_bbt(SEXP nameSEXP, SEXP rankSEXP, SEXP rSEXP, SEXP rdSEXP, SEXP sigSEXP, SEXP weightSEXP, SEXP kappaSEXP, SEXP gammaSEXP, SEXP betaSEXP, SEXP init_rSEXP, SEXP init_rdSEXP) {
BEGIN_RCPP
    Rcpp::RObject rcpp_result_gen;
    Rcpp::RNGScope rcpp_rngScope_gen;
    Rcpp::traits::input_parameter< CharacterVector >::type name(nameSEXP);
    Rcpp::traits::input_parameter< IntegerVector >::type rank(rankSEXP);
    Rcpp::traits::input_parameter< NumericMatrix >::type r(rSEXP);
    Rcpp::traits::input_parameter< NumericMatrix >::type rd(rdSEXP);
    Rcpp::traits::input_parameter< NumericVector >::type sig(sigSEXP);
    Rcpp::traits::input_parameter< NumericVector >::type weight(weightSEXP);
    Rcpp::traits::input_parameter< double >::type kappa(kappaSEXP);
    Rcpp::traits::input_parameter< double >::type gamma(gammaSEXP);
    Rcpp::traits::input_parameter< double >::type beta(betaSEXP);
    Rcpp::traits::input_parameter< double >::type init_r(init_rSEXP);
    Rcpp::traits::input_parameter< double >::type init_rd(init_rdSEXP);
    rcpp_result_gen = Rcpp::wrap(bbt(name, rank, r, rd, sig, weight, kappa, gamma, beta, init_r, init_rd));
    return rcpp_result_gen;
END_RCPP
}
// dbl
Rcpp::List dbl(CharacterVector name, IntegerVector rank, NumericMatrix X, NumericVector R, NumericVector RD, NumericVector sig, NumericVector weight);
RcppExport SEXP _sport_dbl(SEXP nameSEXP, SEXP rankSEXP, SEXP XSEXP, SEXP RSEXP, SEXP RDSEXP, SEXP sigSEXP, SEXP weightSEXP) {
BEGIN_RCPP
    Rcpp::RObject rcpp_result_gen;
    Rcpp::RNGScope rcpp_rngScope_gen;
    Rcpp::traits::input_parameter< CharacterVector >::type name(nameSEXP);
    Rcpp::traits::input_parameter< IntegerVector >::type rank(rankSEXP);
    Rcpp::traits::input_parameter< NumericMatrix >::type X(XSEXP);
    Rcpp::traits::input_parameter< NumericVector >::type R(RSEXP);
    Rcpp::traits::input_parameter< NumericVector >::type RD(RDSEXP);
    Rcpp::traits::input_parameter< NumericVector >::type sig(sigSEXP);
    Rcpp::traits::input_parameter< NumericVector >::type weight(weightSEXP);
    rcpp_result_gen = Rcpp::wrap(dbl(name, rank, X, R, RD, sig, weight));
    return rcpp_result_gen;
END_RCPP
}
// glicko2
List glicko2(CharacterVector name, std::vector<int> rank, NumericVector r, NumericVector rd, NumericVector sig, NumericVector weight, double tau, double init_r, double init_rd);
RcppExport SEXP _sport_glicko2(SEXP nameSEXP, SEXP rankSEXP, SEXP rSEXP, SEXP rdSEXP, SEXP sigSEXP, SEXP weightSEXP, SEXP tauSEXP, SEXP init_rSEXP, SEXP init_rdSEXP) {
BEGIN_RCPP
    Rcpp::RObject rcpp_result_gen;
    Rcpp::RNGScope rcpp_rngScope_gen;
    Rcpp::traits::input_parameter< CharacterVector >::type name(nameSEXP);
    Rcpp::traits::input_parameter< std::vector<int> >::type rank(rankSEXP);
    Rcpp::traits::input_parameter< NumericVector >::type r(rSEXP);
    Rcpp::traits::input_parameter< NumericVector >::type rd(rdSEXP);
    Rcpp::traits::input_parameter< NumericVector >::type sig(sigSEXP);
    Rcpp::traits::input_parameter< NumericVector >::type weight(weightSEXP);
    Rcpp::traits::input_parameter< double >::type tau(tauSEXP);
    Rcpp::traits::input_parameter< double >::type init_r(init_rSEXP);
    Rcpp::traits::input_parameter< double >::type init_rd(init_rdSEXP);
    rcpp_result_gen = Rcpp::wrap(glicko2(name, rank, r, rd, sig, weight, tau, init_r, init_rd));
    return rcpp_result_gen;
END_RCPP
}
// harkness
List harkness(Rcpp::StringVector teams, std::vector<int> rank, std::vector<int> days, std::vector<double> r, std::vector<double> rd, double init_r, double init_rd);
RcppExport SEXP _sport_harkness(SEXP teamsSEXP, SEXP rankSEXP, SEXP daysSEXP, SEXP rSEXP, SEXP rdSEXP, SEXP init_rSEXP, SEXP init_rdSEXP) {
BEGIN_RCPP
    Rcpp::RObject rcpp_result_gen;
    Rcpp::RNGScope rcpp_rngScope_gen;
    Rcpp::traits::input_parameter< Rcpp::StringVector >::type teams(teamsSEXP);
    Rcpp::traits::input_parameter< std::vector<int> >::type rank(rankSEXP);
    Rcpp::traits::input_parameter< std::vector<int> >::type days(daysSEXP);
    Rcpp::traits::input_parameter< std::vector<double> >::type r(rSEXP);
    Rcpp::traits::input_parameter< std::vector<double> >::type rd(rdSEXP);
    Rcpp::traits::input_parameter< double >::type init_r(init_rSEXP);
    Rcpp::traits::input_parameter< double >::type init_rd(init_rdSEXP);
    rcpp_result_gen = Rcpp::wrap(harkness(teams, rank, days, r, rd, init_r, init_rd));
    return rcpp_result_gen;
END_RCPP
}
// factor2dummy
IntegerMatrix factor2dummy(SEXP factor);
RcppExport SEXP _sport_factor2dummy(SEXP factorSEXP) {
BEGIN_RCPP
    Rcpp::RObject rcpp_result_gen;
    Rcpp::RNGScope rcpp_rngScope_gen;
    Rcpp::traits::input_parameter< SEXP >::type factor(factorSEXP);
    rcpp_result_gen = Rcpp::wrap(factor2dummy(factor));
    return rcpp_result_gen;
END_RCPP
}

static const R_CallMethodDef CallEntries[] = {
    {"_sport_bbt", (DL_FUNC) &_sport_bbt, 11},
    {"_sport_dbl", (DL_FUNC) &_sport_dbl, 7},
    {"_sport_glicko2", (DL_FUNC) &_sport_glicko2, 9},
    {"_sport_harkness", (DL_FUNC) &_sport_harkness, 7},
    {"_sport_factor2dummy", (DL_FUNC) &_sport_factor2dummy, 1},
    {NULL, NULL, 0}
};

RcppExport void R_init_sport(DllInfo *dll) {
    R_registerRoutines(dll, NULL, CallEntries, NULL, NULL);
    R_useDynamicSymbols(dll, FALSE);
}
