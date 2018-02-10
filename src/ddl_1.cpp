#include <RcppArmadillo.h>
// [[Rcpp::depends(RcppArmadillo)]]

//' Bayesian Bradley-Terry model for single game
//' 
//' Calculates Glicko ratings based on Bayesian Bradley Terry model.
//' 
//' Algorithm based on 'A Bayesian Approximation Method for Online Ranking' by Ruby C. Weng and Chih-Jen Lin
//' @param rank.
//' @param mi_ij ratings of participants.
//' @param sig_ij rating deviations of participants.
//' @param kappa
//' @param gamma
//' @return \code{r} updated ratings of participats
//' @return \code{rd} updated deviations of participants ratings
//' @return \code{expected} matrix of expected score. \code{expected[i, j] = P(i > j)} 
//' @export
// [[Rcpp::export]]
List 
ddl(
  Rcpp::CharacterVector teams,
  arma::vec rank,
  arma::mat X, 
  arma::mat H,
  arma::mat S
) {
  int n = rank.size();
  int k = X.n_cols();
  int idx = 0;
  
  for(int i = 0; i < n; i++){
    
    for(int q = 0; q<n; q++ ){
      if(i!=q){
      
        idx += 1;
        
        
      }
    }
  }
  
  
  
  return Rcpp::List::create(
    Rcpp::Named("X") = X,
    Rcpp::Named("S") = S
  );
}