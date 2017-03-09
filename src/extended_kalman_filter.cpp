// [[Rcpp::depends(RcppArmadillo)]]
#include <RcppArmadillo.h>
using namespace Rcpp;

arma::vec stateFunE(arma::vec x, arma::mat F, arma::vec u){
  return( x + F * u );
}

//' Extended Kalman filter for single game
//' 
//' Calculates score by smoothing
//' 
//' @param teams name of event participants.
//' @param rank classification of the event.
//' @param days days after previous match - indicator multiplying uncertainty of expectations.
//' @param r ratings of participants.
//' @param rd rating deviations of participants.
//' @param init_r initial rating for new competitors (contains NA). Default = 1500
//' @param init_rd initial rating deviations for new competitors. Default = 350
//' @return \code{r} updated ratings of participats
//' @return \code{rd} updated deviations of participants ratings
//' @return \code{expected} matrix of expected score. \code{expected[i, j] = P(i > j)} 
//' @examples
//'EKF(
//'  x    = c( 1500, 1400, 1550, 1700 ) , 
//'  F    = diag(4),
//'  B    = matrix(c( 200,  30,   100,  300 ),2),
//'  u    = c( 1, 1, 1, 1),
//'  z    = c(0,1,0,0),
//'  H    = matrix(c( .06, .06, .05, .07),2)
//')
//' @export
// [[Rcpp::export]]
List 
  EKF(
    arma::vec x,
    arma::mat F,
    arma::mat B,
    arma::vec u,
    arma::vec z,
    arma::mat H
  ) {
    
    arma::mat ans = stateFunE(x, F, u);
  
    return List::create(
      _["x"] = x,
      _["and"] = ans
    );  
  }

