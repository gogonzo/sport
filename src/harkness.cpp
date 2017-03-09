#include <Rcpp.h>
using namespace Rcpp;
//' Harkness rating for single game
//' 
//' Calculates Harkness rating for single game input
//' 
//' @param teams name of event participants.
//' @param rank classification of the event.
//' @param days days after previous match - indicator multiplying uncertainty of expectations.
//' @param r ratings of participants.
//' @param rd rating deviations of participants.
//' @param init_r initial rating for new competitors (contains NA). Default = 1500
//' @param init_rd initial rating deviations for new competitors. Default = 350
//' @examples
//'  teams <- c("A","B","C","D")
//'  rank  <- c(3 , 4 , 1, 2)
//'  days  <- rep(0, 4)
//'  r     <- c(1500, 1400, 1550, 1700) 
//'  rd    <- c(200,  30,   100,  300)
//'  harkness(
//'    teams = teams, 
//'    rank  = rank, 
//'    days  = days,
//'    r     = r, 
//'    rd    = rd,
//'    init_r  = 1500,
//'    init_rd = 100)
//' @export
// [[Rcpp::export]]
List 
  harkness(
    Rcpp::StringVector teams, 
    std::vector<int> rank,
    std::vector<int> days,
    std::vector<double> r, 
    std::vector<double> rd,
    double init_r  = 1500,
    double init_rd = 350
  ) {
    
    Rcpp::Environment global = Rcpp::Environment::global_env();
    std::map<std::string, std::string> map;
    
    int n = teams.size();
    double 
      pi  = std::atan(1)*4,
        q   = log(10)/400,
        E_s = 0,
        d2  = 0,
        r_  = 0;
    NumericVector g_rd(n);
    
    // precalculate 
    for(int i = 0; i < n; i++){
      if( NumericVector::is_na(r[i]) ) {
        r[i]    = init_r;
        rd[i]   = init_rd;
        g_rd[i] = 1/sqrt(1 + 3 * pow(q,2) * pow(rd[i], 2) / pow(pi,2) );      
        
      } else {
        //rd[i]   = std::min( sqrt( pow(rd[i],2) + pow( days[i],2 ) ), init_rd );
        g_rd[i] = 1/sqrt(1 + 3 * pow(q,2) * pow(rd[i], 2) / pow(pi,2) );
        
      }
    }
    
    // Harkness RATING
    for(int i = 0; i < n; i++){
      d2  = 0;
      r_ = 0;
      
      for(int j = 0; j < n; j ++){
        if(j != i){
          E_s = 1/(1 + pow(10, -g_rd[j] * (r[i] - r[j])/400));
          d2   += pow(g_rd[j],2) * E_s * ( 1 - E_s );
          
          if( rank[i] < rank[j] ) 
            r_ += g_rd[j] * ( 1 - E_s );
          else 
            r_ += g_rd[j] * ( - E_s ); 
        }
      }
      
      // this event ratings
      d2  = pow( pow(q, 2) * d2, -1);
      r[i]  = r[i] + q/( 1/pow(rd[i],2) + 1/d2 ) * r_;
      rd[i] = sqrt(   1/( 1/pow(rd[i],2) + 1/d2) ); 
      
    }
    
    
    // objects r and rd returned to environment
    // global["r"]     = r;
    // global["rd"]    = rd;
    
    return List::create(
      _["r_new"]    = r,
      _["rd_new"]   = rd
    );  
  }

