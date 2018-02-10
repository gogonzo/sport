#include <RcppEigen.h>
// [[Rcpp::depends(RcppEigen)]]
using Eigen::Map;               	      // 'maps' rather than copies 
using Eigen::MatrixXd;                  // variable size matrix, double precision
using Eigen::VectorXd;                  // variable size vector, double precision
using Eigen::SelfAdjointEigenSolver;    // one of the eigenvalue solvers

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
List 
  ddl2(
    CharacterVector teams,
    IntegerVector rank,
    NumericMatrix X, 
    NumericMatrix H,
    NumericMatrix S,
    NumericMatrix Bu = 0,
    NumericVector pa = 1
  ) {
    int n = rank.size();
    int k = X.ncol();
    int idx = 0;
    
    CharacterVector home(n*n-n);
    CharacterVector away(n*n-n);
    NumericVector P(n*n-n);
    NumericVector Y(n*n-n);
    NumericMatrix F(n,n);
    
    NumericMatrix x(k,1);
    NumericMatrix h(1,k);
    NumericMatrix s(k,k);
    
    for(int i = 0; i < n; i++){
      
      for(int q = 0; q<n; q++ ){
        if(i!=q){
          

          idx += 1;
          home( idx - 1 ) = teams[i];
          away( idx - 1 ) = teams[q];
          
        }
      }
    }
    

    
    return List::create(
      _["X"]  = X,
      _["S"] = S,
      _["pairs"] = DataFrame::create(
        _["home"] = home,
        _["away"] = away,
        _["P"] = P,
        _["Y"] = Y
      )
    );
  }