#include <Rcpp.h>
using namespace Rcpp;
//' Glicko rating for single game
//' 
//' Calculates Glicko rating for single game input
//' 
//' @param r ratings of game participants.
//' @param RD rating deviations.
//' @export
// [[Rcpp::export]]
List 
  glicko(
    Rcpp::StringVector teams, 
    std::vector<int> rank,
    std::vector<int> days,
    Rcpp::StringVector names,
    std::vector<double> r, 
    std::vector<double> rd,
    double init_r  = 1500,
    double init_rd = 350
  ) {
  
    Rcpp::Environment global = Rcpp::Environment::global_env();
    std::map<std::string, std::string> map;
    
    int 
      n = teams.size(), 
      k = names.size();
    
    double 
      pi  = std::atan(1)*4,
      q   = log(10)/400,
      E_s = 0,
      d2  = 0,
      r_  = 0;
    
    NumericVector 
      idx(n), 
      r2(n), 
      rd2(n), 
      g_rd(n);
    
    // idx of i-team in the vector of all teams
    for(int i = 0; i < n; i++){
      for(int j = 0; j < (k + n); j ++){
        
        if( j < k && teams[i] == names[j] ) {
          idx[i]  = j;
          r2[i]   = r[j];
          rd2[i]  = std::min( sqrt( pow(rd[j],2) + pow(days[j],2) ), init_rd );
          g_rd[i] = 1/sqrt(1 + 3 * pow(q,2) * pow(rd[i], 2) / pow(pi,2) );      
          break;
          
        } else if(j >= k){
          names.push_back(teams[i]);
          r.push_back(init_r);
          rd.push_back(init_rd);
          
          idx[i] = j;
          r2[i]  = r[j];
          rd2[i] = rd[j];
          g_rd[i] = 1/sqrt(1 + 3 * pow(q,2) * pow(rd[i], 2) / pow(pi,2) );      
          break;
          
        } 
      }
    }
    
    // GLICKO RATING
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
      r2[i]  = r2[i] + q/( 1/pow(rd2[i],2) + 1/d2 ) * r_;
      rd2[i] = sqrt(   1/( 1/pow(rd2[i],2) + 1/d2) ); 
      
      // global ratings update
      r [ idx[i] ] = r2 [i];
      rd[ idx[i] ] = rd2[i];
    }
    
    
  // objects r and rd returned to environment
  global["r"]     = r;
  global["rd"]    = rd;
  global["names"] = names;
    
  return List::create(
    _["idx"]   = idx, 
    _["teams"] = teams,
    _["r2"]    = r2,
    _["rd2"]   = rd2,
    _["g_rd"]  = g_rd
  );  
}

