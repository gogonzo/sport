// [[Rcpp::depends(RcppArmadillo)]]
#include <RcppArmadillo.h>
using namespace Rcpp;

arma::vec stateFun(arma::vec x, arma::mat F, arma::vec u){
  return( x + F * u );
}    
arma::vec measureFun(arma::vec x, arma::mat F, arma::vec u){
  return( x + F * u );
}


//' Kalman filter for single game
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
//' B <- diag(4)
//' diag(B) <- c( 200,  30,   100,  300 ) 
//'KF(
//'  x    = c( 1500, 1400, 1550, 1700 ) , 
//'  F    = B,
//'  B    = matrix(c( 200,  30,   100,  300 ),2),
//'  u    = c( 3, 4, 2, 1),
//'  z    = c(3,4,2,1),
//'  H    = matrix(c( .06, .06, .05, .07),2)
//')
//' @export
// [[Rcpp::export]]
List 
  KF(
    arma::vec x,
    arma::mat F,
    arma::mat B,
    arma::vec u,
    arma::vec z,
    arma::mat H
  ) {
    
    arma::mat Z = arma::mat(4,4).fill(0);
    //arma::mat ans = stateFun(x, F, u);

    for(int i = 0; i < 4; i++)
      for(int j = 0; j < 4; j++)
        if(i != j)
          if(z[i] <= z[j])
            Z(i, j) = 1;
    
    
    return List::create(
      _["x"] = x,
      _["Z"] = Z
    );  
  }

