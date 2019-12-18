#include <Rcpp.h>
#include "glicko.h"
#include "utils.h"

using namespace Rcpp;
// [[Rcpp::plugins(cpp11)]]

#define DBG(MSG,X) Rprintf("%20s SEXP=<%p>. List=%p\n", MSG, (SEXP)X, &X ) ;

class Ratings {
private:
  // match arguments
  std::string alg;
  int id_i; 
  Rcpp::IntegerVector idx_rows;
  Rcpp::IntegerVector id_vec;
  Rcpp::IntegerVector rank_vec;
  Rcpp::StringVector name_vec;
  Rcpp::NumericVector sigma_vec;
  Rcpp::NumericVector weight_vec;
  
  // rating arguments
  Rcpp::StringVector r_names;
  double init_r;
  double init_rd;
  double gamma;
  double kappa;
  double tau;
  double beta;
  
  // current iteration
  Rcpp::IntegerVector id_vec_current;
  Rcpp::IntegerVector rank_vec_current;
  Rcpp::StringVector name_vec_current;
  Rcpp::NumericVector sigma_vec_current;
  Rcpp::NumericVector weight_vec_current;
  
  Rcpp::IntegerVector idx_rating;
  Rcpp::NumericVector r_current;
  Rcpp::NumericVector rd_current;
  
public:
  Rcpp::NumericVector r;
  Rcpp::NumericVector rd;
  Rcpp::NumericVector sigma;
  Rcpp::List out_r;
  Rcpp::List out_p;  
  
  void play(int id_i_val) {
    id_i = id_i_val;
    idx_rows = utils::find<int>(id_vec, id_i);
    id_vec_current = id_vec[idx_rows];
    rank_vec_current = rank_vec[idx_rows];
    name_vec_current = name_vec[idx_rows];
    sigma_vec_current = sigma_vec[idx_rows];
    weight_vec_current = weight_vec[idx_rows];
    
    idx_rating = Rcpp::match(name_vec_current, r_names) - 1;
    r_current = r[idx_rating];
    rd_current = rd[idx_rating];

    calc_glicko();
  } 
  void calc_glicko() {
    int n = name_vec_current.size();
    int idx = 0;
    
    double q = log(10.0)/400.0, var, err, rd_update;
    NumericVector g_rd(n);
    NumericVector var_i(n);
    NumericVector err_i(n);
    NumericVector delta_i(n);
    StringVector team1(n*n-n);
    StringVector team2(n*n-n);
    NumericVector P(n*n-n);
    NumericVector Y(n*n-n);
    
    // precalculate 
    for(int i = 0; i < n; i++){
      if(NumericVector::is_na(r[i])) 
        r_current[i] = init_r, 
        rd_current[i] = init_rd;
      if((sqrt(pow(rd_current[i], 2.0) + pow(sigma_vec_current[i],2.0))) < init_rd) {
        rd_current[i] = sqrt(pow(rd_current[i], 2.0) + pow(sigma_vec_current[i],2.0));
      } else{ 
        rd_current[i] = init_rd;
      }
      g_rd[i] = calcGRd(rd_current[i] );
    }
    
    // GLICKO RATING
    for(int i = 0; i < n; i++){
      var = 0.0;
      err = 0.0;
      
      for(int j = 0; j < n; j ++){
        if(j != i){
          idx += 1;
          
          team1( idx - 1 ) = name_vec_current[i];
          team2( idx - 1 ) = name_vec_current[j];
          
          P( idx - 1 ) = calcPGlicko(calcGRd(sqrt(pow(rd_current[i], 2.0) + 
                                                  pow(rd_current[j], 2.0))), r_current[i], r_current[j]);
          Y( idx - 1 ) = calcZ(rank_vec_current(i), rank_vec_current(j));
          var += calcVar(g_rd[j], P(idx - 1));
          err += calcErr(g_rd[j], P(idx - 1), rank_vec_current(i), rank_vec_current(j));
          
        } else { 
          continue; 
        }
      }
      
      // this event ratings
      err_i[i]   = err;
      var_i[i]   = var;
      delta_i[i] =  gamma * 1 / (pow(q, 2.0) * var);
      
    }
    
    // update parameters 
    for(int i = 0; i < n; i++) {
      r_current[i]   += q / (1 / pow(rd_current[i], 2.0) + 1 / delta_i[i]) * err_i[i] * weight_vec_current[i]; 
      rd_update = (rd_current[i] - sqrt(1 / (1 / pow(rd_current[i], 2.0) + 1/ (delta_i[i])))) * weight_vec_current[i];
      if(rd_update > (rd(i) * (1 - kappa))) {
        rd_current(i) = rd_current(i) * kappa;
      } else {
        rd_current(i) -= rd_update;
      }
    }
    
    
    Rcpp::DataFrame out_r_i = Rcpp::DataFrame::create(
      _["name"] = name_vec_current,
      _["r"]    = r[idx_rating],
      _["rd"]   = rd[idx_rating],
      _["stringsAsFactors"] = false
    );
    
    Rcpp::DataFrame out_p_i = Rcpp::DataFrame::create(
      _["name"] = team1,
      _["opponent"] = team2,
      _["P"] = P,
      _["Y"] = Y,
      _["stringsAsFactors"] = false
    );
    
    out_r.push_back(out_r_i);
    out_p.push_back(out_p_i);
    
    r[idx_rating]  = r_current;
    rd[idx_rating] = rd_current;
  };
  void calc_glicko2() {};
  
  // glicko
  Ratings(Rcpp::IntegerVector& id_vec,
          Rcpp::IntegerVector& rank_vec,
          Rcpp::StringVector& name_vec,
          Rcpp::NumericVector& sigma_vec,
          Rcpp::NumericVector& weight_vec,
          Rcpp::StringVector& names,
          Rcpp::NumericVector r, 
          Rcpp::NumericVector rd,
          double init_r,
          double init_rd,
          double gamma,
          double kappa);
  
  // glicko2
  Ratings(Rcpp::IntegerVector& id_vec,
          Rcpp::IntegerVector& rank_vec,
          Rcpp::StringVector& name_vec,
          Rcpp::NumericVector& weight_vec,
          Rcpp::StringVector& names,
          Rcpp::NumericVector r, 
          Rcpp::NumericVector rd,
          Rcpp::NumericVector sigma,
          double init_r,
          double init_rd,
          double kappa,
          double tau);
  
  
  // bbt
  Ratings(Rcpp::IntegerVector& id_vec,
          Rcpp::IntegerVector& rank_vec,
          Rcpp::StringVector& name_vec,
          Rcpp::NumericVector& sigma_vec,
          Rcpp::NumericVector& weight_vec,
          
          Rcpp::StringVector& names,
          Rcpp::NumericVector r, 
          Rcpp::NumericVector rd,
          Rcpp::NumericVector sigma,
          double init_r,
          double init_rd,
          double kappa,
          double gamma,
          double beta);
  //dbl 
  
  
  void test() {
    DBG("r in Ratings class: %p", r);
  }
};

// glicko constructor
Ratings::Ratings(
  Rcpp::IntegerVector& id_vec_val,
  Rcpp::IntegerVector& rank_vec_val,
  Rcpp::StringVector& name_vec_val, 
  Rcpp::NumericVector& sigma_vec_val,
  Rcpp::NumericVector& weight_vec_val,
  
  Rcpp::StringVector& r_names_val,
  Rcpp::NumericVector r_val, 
  Rcpp::NumericVector rd_val,
  double init_r_val,
  double init_rd_val,
  double gamma_val,
  double kappa_val) {
  
    int k = r_names_val.size();
    if (r_val.size() == 0) {
      r_val = NumericVector(k, init_r_val);
    } else if (r_val.size() == k) {
      r = clone(r_val);
    } else {
      Rcpp::stop("wrong size of r - it should be either equal to rd or empty.");
    }
    
    if (rd_val.size() == 0) {
      rd_val = NumericVector(k, init_rd_val);
    } else if (rd_val.size() == k) {
      rd = clone(rd_val);
    } else {
      Rcpp::stop("wrong size of rd - it should be either equal to r or empty.");
    }
    
    
    // rank and name must be of length n
    id_vec = id_vec_val;
    rank_vec = rank_vec_val;
    name_vec = name_vec_val;
    sigma_vec = sigma_vec_val;
    weight_vec = weight_vec_val;
    
    r_names = r_names_val;
    init_r = init_r_val;
    init_rd = init_rd_val;
    gamma = gamma_val;
    kappa = kappa_val;
  }


//' Glicko
//' @examples
//' x <- data.frame(id = 0, name = "a", rank = 1, r = 2, rd = 3, sigma = 4, weight = 5, identifier = 6)
//' glicko(
//'   id = x[["id"]], 
//'   name = x[["name"]], 
//'   rank = x[["rank"]], 
//'   r = x[["rank"]], 
//'   rd = x[["rd"]], 
//'   sigma = x[["sigma"]], 
//'   weight = x[["weight"]], 
//'   identifier = x[["identifier"]])
// [[Rcpp::export]]
List 
  glicko(
    IntegerVector id,
    IntegerVector rank,
    StringVector name, 
    NumericVector r, 
    NumericVector rd,
    NumericVector sigma,
    NumericVector weight,
    double init_r  = 1500.00,
    double init_rd = 350.00,
    double gamma = 1.0,
    double kappa = .5) {

    int id_i;
    IntegerVector id_uniq = utils::unique(id);
    StringVector player_names = r.names();

    Ratings ratings{
      id, rank, name, sigma, weight,         // match vectors
      player_names, r, rd, 1500, 350, 1, .5  // ratings
    };
    
    for (int i = 0; i < id_uniq.size(); i++) {
      id_i = id_uniq(i);
      ratings.play(id_i);
    }
   
   return Rcpp::List::create(
     _["r"] = ratings.out_r,
     _["p"] = ratings.out_p,
     _["final_r"] = ratings.r,
     _["final_rd"] = ratings.rd
   );
  }


// [[Rcpp::export]]
List 
  glicko2(
    StringVector name, 
    IntegerVector rank,
    NumericVector r, 
    NumericVector rd,
    NumericVector sigma,
    NumericVector weight,
    double kappa,
    double tau = .5,
    double init_r  = 1500.00,
    double init_rd = 350.00
  ) {
    
    int n = name.size();
    int idx = 0;
    double err, var, A, rd_update;
    NumericVector mu(n);
    NumericVector phi(n);
    NumericVector g_phi(n);
    NumericVector var_i(n);
    NumericVector err_i(n);
    NumericVector delta_i(n);
    StringVector team1(n*n-n);
    StringVector team2(n*n-n);
    NumericVector P(n*n-n);
    NumericVector Y(n*n-n);
    
    
    // precalculate 
    for(int i = 0; i < n; i++){
      // rescale params to glicko2 scale
      mu[i]    = r2mu(r[i]);
      phi[i]   = rd2phi(rd[i]);
      g_phi[i] = calcGPhi(phi[i]);
    }
    
    // Sum deviations from expectations  
    for (int i = 0; i < n; i++) {
      var = 0.0, err  = 0.0;
      for (int j = 0; j < n; j++) {
        if (j != i) {
          idx += 1;
          team1(idx - 1) = name[i];
          team2(idx - 1) = name[j];
          
          Y(idx - 1) = calcZ( rank(i), rank(j));
          P(idx - 1) = calcPGlicko2(sqrt(pow(g_phi[i], 2.0) + pow(g_phi[j], 2.0)), mu[i], mu[j]);
          
          var += calcVar(g_phi[j], P( idx - 1 ));
          err += calcErr(g_phi[j], P( idx - 1 ), rank(i), rank(j));
        } else { 
          continue; 
        }
      }
      var_i[i]  = 1/var;
      err_i[i]  = err;
      delta_i[i] = 1/var * err;
    }
    
    
    // update parameters
    for(int i = 0; i < n; i++){
      A = optimSigma(delta_i[i], sigma[i], phi[i], var_i[i], tau);
      sigma[i] = exp( A/2 );
      
      phi[i] = updatePhi(phi[i], var_i[i], sigma[i]);
      if(phi[i]>(init_rd/173.7178)) phi[i] = init_rd/173.7178;
      
      r[i]  = mu2r( mu[i] + pow(phi[i],2.0) * err_i[i] * weight[i] );
      rd_update = ( rd[i] - phi2rd( phi[i] ) ) * weight[i];
      if(rd_update > (rd(i) * (1-kappa))) {
        rd[i] = rd(i) * kappa;
      } else {
        rd[i] = rd[i] - rd_update;
      }
    }
    
    Rcpp::List dimnms = Rcpp::List::create(name, name);
    r.names()  = name;
    rd.names() = name;
    return List::create(
      _["r"]     = r,
      _["rd"]    = rd,
      _["sigma"] = sigma,
      _["r_df"]  = DataFrame::create(
        _["name"] = name,
        _["r"]    = r,
        _["rd"]   = rd,
        _["sigma"] = sigma,
        _["stringsAsFactors"] = false
      ),
      _["pairs"] = DataFrame::create(
        _["name"]     = team1,
        _["opponent"] = team2,
        _["P"] = P,
        _["Y"] = Y,
        _["stringsAsFactors"] = false
      )
    );  
    
  }


