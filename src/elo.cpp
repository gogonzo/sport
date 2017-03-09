#include <Rcpp.h>
using namespace Rcpp;
//' FIDE rating for single game
//' 
//' FIDE Rating Regulations effective from 1 July 2014
//' 
//' @param teams name of event participants.
//' @param rank classification of the event.
//' @param days days after previous match - indicator multiplying uncertainty of expectations.
//' @param r ratings of participants.
//' @param rd rating deviations of participants.
//' @param init_r initial rating for new competitors (contains NA). Default = 1500
//' @param init_rd initial rating deviations for new competitors. Default = 350
//' @examples
//'elo(
//'  teams = c( "A", "B", "C", "D", "E", "F" ), 
//'  rank  = c( 3, 2, 3, 5, 6, 1), 
//'  r     = c( 1613, 1609, 1477, 1388, 1586, 1720 ) , 
//'  init_r  = 1500,
//'  init_rd = 100
//')
//' @export
// [[Rcpp::export]]
List 
  fide(
    CharacterVector teams, 
    std::vector<int> rank,
    NumericVector r, 
    int K = 32,
    double init_r  = 1500,
    double init_rd = 350
  ) {
    
    int n = teams.size();
    double r_  = 0;
    NumericMatrix E_s(n, n);
    
    // precalculate 
    for(int i = 0; i < n; i++){
      if( NumericVector::is_na(r[i]) ) {
        r[i]    = init_r;
      }
    }
    
    
    // ELO RATING
    for(int i = 0; i < n; i++){
      r_ = 0;
      
      for(int j = 0; j < n; j ++){
        if(j != i){
          E_s(i,j) = pow(10, r[i]/400)/( pow(10, r[i]/400) + pow(10, r[j]/400) );
          
          if( rank[i] < rank[j] ) {
            r_ += ( 1 - E_s(i,j) );
          } else if(rank[i] == rank[j]) { 
            r_ += ( .5 - E_s(i,j) ); 
          } else {
            r_ += ( - E_s(i,j) ); 
            
          }
        }
      }
      
      // this event ratings
      r[i]  = r[i] + K * r_;
      
    }
    
    Rcpp::List dimnms = Rcpp::List::create(teams, teams);
    E_s.attr("dimnames") = dimnms;
    r.names()  = teams;
    
    return List::create(
      _["r"]    = r,
      _["expected"] = E_s
    );  
  }