#include <Rcpp.h>
#include "bbt.h"
#include "glicko.h"
#include "utils.h"
#include "constructors.h"

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
  Rcpp::NumericVector beta_vec;
  Rcpp::NumericVector weight_vec;
  
  // rating arguments
  Rcpp::StringVector r_names;
  double init_r;
  double init_rd;
  double init_sigma;
  double gamma;
  double kappa;
  double tau;
  double beta;
  
  // current iteration 
  Rcpp::IntegerVector id_vec_current;
  Rcpp::IntegerVector rank_vec_current;
  Rcpp::StringVector name_vec_current;
  Rcpp::NumericVector sigma_vec_current;
  Rcpp::NumericVector beta_vec_current;
  Rcpp::NumericVector weight_vec_current;
  
  Rcpp::IntegerVector idx_rating;
  Rcpp::NumericVector r_current;
  Rcpp::NumericVector rd_current;
  Rcpp::NumericVector sigma_current;
  
public:
  Rcpp::NumericVector r;
  Rcpp::NumericVector rd;
  Rcpp::NumericVector sigma;
  Rcpp::List out_r;
  Rcpp::List out_p;  
  
  void calc_glicko(int id_i_val) {
    id_i = id_i_val;
    idx_rows = utils::find<int>(id_i, id_vec);
    id_vec_current = id_vec[idx_rows];
    rank_vec_current = rank_vec[idx_rows];
    name_vec_current = name_vec[idx_rows];
    sigma_vec_current = sigma_vec[idx_rows];
    weight_vec_current = weight_vec[idx_rows];
    
    idx_rating = Rcpp::match(name_vec_current, r_names) - 1;
    r_current = r[idx_rating];
    rd_current = rd[idx_rating];
    
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
    for (int i = 0; i < n; i++) {
      if (NumericVector::is_na(r[i])) 
        r_current[i] = init_r, 
          rd_current[i] = init_rd;
      if((sqrt(pow(rd_current[i], 2.0) + pow(sigma_vec_current[i], 2.0))) < init_rd) {
        rd_current[i] = sqrt(pow(rd_current[i], 2.0) + pow(sigma_vec_current[i], 2.0));
      } else{ 
        rd_current[i] = init_rd;
      }
      g_rd[i] = calcGRd(rd_current[i]);
    }
    
    Rcpp::DataFrame out_r_i = Rcpp::DataFrame::create(
      _["name"] = name_vec_current,
      _["r"]    = r_current,
      _["rd"]   = rd_current,
      _["stringsAsFactors"] = false
    );
    
    // GLICKO RATING
    for (int i = 0; i < n; i++) {
      var = 0.0;
      err = 0.0;
      
      for (int j = 0; j < n; j++) {
        if (j != i) {
          idx += 1;
          
          team1( idx - 1 ) = name_vec_current[i];
          team2( idx - 1 ) = name_vec_current[j];
          
          P(idx - 1) = calcPGlicko(calcGRd(sqrt(pow(rd_current[i], 2.0) + 
            pow(rd_current[j], 2.0))), r_current[i], r_current[j]);
          Y(idx - 1) = calcZ(rank_vec_current(i), rank_vec_current(j));
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
    for (int i = 0; i < n; i++) {
      r_current[i] += q / (1 / pow(rd_current[i], 2.0) + 1 / delta_i[i]) * err_i[i] * weight_vec_current[i]; 
      rd_update = (rd_current[i] - sqrt(1 / (1 / pow(rd_current[i], 2.0) + 1/ (delta_i[i])))) * weight_vec_current[i];
      if (rd_update > (rd(i) * (1 - kappa))) {
        rd_current(i) = rd_current(i) * kappa;
      } else {
        rd_current(i) -= rd_update;
      }
    }
    
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
    
  } 
  void calc_glicko2(int id_i_val) {
    
    id_i = id_i_val;
    idx_rows = utils::find<int>(id_i, id_vec);
    id_vec_current = id_vec[idx_rows];
    rank_vec_current = rank_vec[idx_rows];
    name_vec_current = name_vec[idx_rows];
    weight_vec_current = weight_vec[idx_rows];
    
    idx_rating = Rcpp::match(name_vec_current, r_names) - 1;
    r_current = r[idx_rating];
    rd_current = rd[idx_rating];
    sigma_current = sigma[idx_rating];
    
    Rcpp::DataFrame out_r_i = Rcpp::DataFrame::create(
      _["id"]    = id_i,
      _["name"]  = name_vec_current,
      _["r"]     = r[idx_rating],
      _["rd"]    = rd[idx_rating],
      _["sigma"] = sigma[idx_rating],
      _["stringsAsFactors"] = false
    );
    
    int n = name_vec_current.size();
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
    for (int i = 0; i < n; i++){
      // rescale params to glicko2 scale
      mu(i)    = r2mu(r_current(i));
      phi(i)   = rd2phi(rd_current(i));
      g_phi(i) = calcGPhi(phi(i));
    }
    
    // Sum deviations from expectations  
    for (int i = 0; i < n; i++) {
      var = 0.0, err  = 0.0;
      for (int j = 0; j < n; j++) {
        if (j != i) {
          idx += 1;
          team1(idx - 1) = name_vec_current(i);
          team2(idx - 1) = name_vec_current(j);
          
          Y(idx - 1) = calcZ(rank_vec_current(i), rank_vec_current(j));
          P(idx - 1) = calcPGlicko2(sqrt(pow(g_phi(i), 2.0) + pow(g_phi(j), 2.0)), mu(i), mu(j));
          
          var += calcVar(g_phi(j), P(idx - 1));
          err += calcErr(g_phi(j), P(idx - 1), rank_vec_current(i), rank_vec_current(j));
        } else { 
          continue; 
        }
      }
      var_i(i)  = 1 / var;
      err_i(i)  = err;
      delta_i(i) = 1 / var * err;
    }
    
    // update parameters
    for (int i = 0; i < n; i++) {
      A = optimSigma(delta_i(i), sigma_current(i), phi(i), var_i(i), tau);
      sigma_current(i) = exp(A / 2);
      
      phi[i] = updatePhi(phi(i), var_i(i), sigma_current(i));
      if(phi[i] > (init_rd / 173.7178)) phi[i] = init_rd / 173.7178;
      
      r[i]  = mu2r(mu(i) + pow(phi(i), 2.0) * err_i(i) * weight_vec_current(i));
      rd_update = (rd(i) - phi2rd(phi(i))) * weight_vec_current(i);
      if(rd_update > (rd(i) * (1 - kappa))) {
        rd_current(i) = rd_current(i) * kappa;
      } else {
        rd_current(i) = rd_current(i) - rd_update;
      }
    }
    
    Rcpp::DataFrame out_p_i = Rcpp::DataFrame::create(
      _["id"] = id_i,
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
    sigma[idx_rating] = sigma_current;
    
  };
  // glicko {
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
  // }
  // 
  // glicko2{
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
          double init_sigma,
          double kappa,
          double tau);
  // }
  // 
  // bbt{
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
  // }
  // 
  //dbl {
  // } 
  
  
  void test() {
    DBG("r in Ratings class: %p", r);
  }
};

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
  
  // rank and name must be of length n
  id_vec = id_vec_val;
  rank_vec = rank_vec_val;
  name_vec = name_vec_val;
  sigma_vec = sigma_vec_val;
  weight_vec = weight_vec_val;
  
  r_names = r_names_val;
  r = clone(r_val);
  rd = clone(rd_val);
  init_r = init_r_val;
  init_rd = init_rd_val;
  gamma = gamma_val;
  kappa = kappa_val;
}


Ratings::Ratings(
  Rcpp::IntegerVector& id_vec_val,
  Rcpp::IntegerVector& rank_vec_val,
  Rcpp::StringVector& name_vec_val, 
  Rcpp::NumericVector& weight_vec_val,
  
  Rcpp::StringVector& r_names_val,
  Rcpp::NumericVector r_val, 
  Rcpp::NumericVector rd_val,
  Rcpp::NumericVector sigma_val,
  
  double init_r_val,
  double init_rd_val,
  double init_sigma_val,
  double gamma_val,
  double kappa_val) {
  
  // rank and name must be of length n
  id_vec = id_vec_val;
  rank_vec = rank_vec_val;
  name_vec = name_vec_val;
  weight_vec = weight_vec_val;
  
  r_names = r_names_val;
  r = clone(r_val);
  rd = clone(rd_val);
  sigma = clone(sigma_val);
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
      id, rank, name, sigma, weight,                      // match vectors
      player_names, r, rd, init_r, init_rd, gamma, kappa  // ratings
    };
    
    for (int i = 0; i < id_uniq.size(); i++) {
      id_i = id_uniq(i);
      ratings.calc_glicko(id_i);
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
    IntegerVector id,
    IntegerVector rank,
    StringVector name, 
    NumericVector r, 
    NumericVector rd,
    NumericVector sigma,
    NumericVector weight,
    double init_r  = 1500.00,
    double init_rd = 350.00,
    double init_sigma = 0.05,
    double kappa = 1.0,
    double tau = .5
  ) {
    
    int id_i;
    IntegerVector id_uniq = utils::unique(id);
    StringVector player_names = r.names();
    
    Ratings ratings{
      id, rank, name, weight,         // match vectors
      player_names, r, rd, sigma, init_r, init_rd, init_sigma, kappa, tau  // ratings
    };
    
    for (int i = 0; i < id_uniq.size(); i++) {
      id_i = id_uniq(i);
      ratings.calc_glicko2(id_i);
    }
    
    return Rcpp::List::create(
      _["r"] = ratings.out_r,
      _["p"] = ratings.out_p,
      _["final_r"] = ratings.r,
      _["final_rd"] = ratings.rd
    );
    
    
  }


// [[Rcpp::export]]
Rcpp::List bbt(
    Rcpp::IntegerVector id,
    Rcpp::IntegerVector rank,
    Rcpp::StringVector team,
    Rcpp::StringVector player,
    Rcpp::NumericVector r_val, 
    Rcpp::NumericVector rd_val,
    Rcpp::NumericVector lambda,
    Rcpp::NumericVector weight,
    double kappa = 0.5,
    double gamma = 999.0,
    double beta = 25 / 6,
    double init_r = 25.0,
    double init_rd = 25 / 3) {
  
  Rcpp::NumericVector r = clone(r_val);
  Rcpp::NumericVector rd = clone(rd_val);
  
  int n = rank.size();
  
  Rcpp::List out_r;
  Rcpp::List out_p;  
  
  Rcpp::IntegerVector idx_i;
  Rcpp::IntegerVector idx_it;
  Rcpp::IntegerVector id_vec_i;
  Rcpp::IntegerVector rank_vec_i;
  Rcpp::StringVector team_vec_i;
  Rcpp::StringVector player_vec_i;
  
  Rcpp::StringVector player_vec_it;
  Rcpp::IntegerVector idx_rating_it;
  Rcpp::NumericVector r_it;
  Rcpp::NumericVector rd_it;
  
  int idx, idx_r, idx_df;
  int id_i;
  std::string team_t;  
  Rcpp::IntegerVector unique_id = utils::unique(id); // delegate unique_id to R - or find better solution
  
  Rcpp::StringVector unique_team_i;
  Rcpp::StringVector player_names = r.names();

  double c, r_sum, rd_ssq;
  
  for (int i = 0; i < unique_id.size(); i++) {
    id_i = unique_id(i);
    
    // player specific variables
    idx_i = utils::find<int>(id_i, id);
    team_vec_i = team[idx_i];
  
    // team specific variables
    unique_team_i = utils::unique(team_vec_i);
    int k = unique_team_i.size();
    Rcpp::IntegerVector rank_vec_it(k);
    Rcpp::NumericVector r_it(k);
    Rcpp::NumericVector rd_it(k);
    
    for (int t = 0; t < k; t++) {
      team_t = unique_team_i(t);
      idx_it = utils::find<std::string>(team_t, team_vec_i) + idx_i(0);
      
      player_vec_it = player[idx_it];
      idx_rating_it = Rcpp::match(player_vec_it, player_names) - 1;
      
      r_sum = 0;
      rd_ssq = 0;
      for (int p = 0; p < idx_rating_it.size(); p++) {
        idx_r = idx_rating_it(p);
        idx_df = idx_it(p);
        r_sum += r(idx_r);
        rd_ssq += pow(rd(idx_r) * lambda(idx_df), 2.0);
      }
      
      rank_vec_it(t) = rank(idx_df);
      r_it(t)  = r_sum;
      rd_it(t) = sqrt(rd_ssq);
    }
    
    // calculate update for teams
    CharacterVector team1(k * k - k);
    CharacterVector team2(k * k - k);
    NumericVector P(k * k - k);
    NumericVector Y(k * k - k);
    NumericVector omega(k, 0.0);
    NumericVector delta(k, 0.0);
    
    idx = 0;
    for (int p = 0; p < k; p++) {
      for (int q = 0; q < k; q++) {
        if (p != q) {
          c = sqrt(pow(rd_it(p), 2.0) + pow(rd_it(q), 2.0) + pow(beta, 2.0));
          if (gamma == 999) gamma = rd_it(p) / c;
          
          team1(idx) = unique_team_i(p);
          team2(idx) = unique_team_i(q);
          Y(idx) = calc_s(rank_vec_it(p), rank_vec_it(q));
          P(idx) = exp(r_it(p) / c) / 
            (exp(r_it(p) / c) +  exp(r_it(q) / c));
          
          omega(p) = omega(p) +
            pow(rd_it(p), 2.0) / c *
            (Y(idx) - P(idx));
          
          delta(p) = 
            delta(p) +
            gamma *
            pow(rd_it(p) / c, 2.0) *
            (P(idx) * (1 - P(idx)));
          
          idx += 1;   
        }
      }
    }
    
    Rcpp::Rcout << "omega: " << omega << std::endl;
    Rcpp::Rcout << "delta: " << delta << std::endl;
    
    // update player ratings
    for (int t = 0; t < k; t++) {
      team_t = unique_team_i(t);
      idx_it = utils::find<std::string>(team_t, team_vec_i) + idx_i(0);
      
      player_vec_it = player[idx_it];
      idx_rating_it = Rcpp::match(player_vec_it, player_names) - 1;
  
      for (int p = 0; p < idx_rating_it.size(); p++) {
        idx_r = idx_rating_it(p);
        idx_df = idx_it(p);
        
        r(idx) = r(idx_r) + 
          rd(idx_r) / sum(rd_it) * omega(t) * weight(idx_df);
        
        rd(idx_r) = rd(idx_r) - 
          sqrt(pow(rd(idx_r), 2.0) * (1 - rd(idx_r) / sum(rd_it) * delta(t)) * weight(idx_df));
        
      }
    }
    
    
    
    
    out_r = Rcpp::List::create(
      _["r"] = r, 
      _["rd"] = rd
    );
    out_p = Rcpp::DataFrame::create(
      _["team"] = team1,
      _["opponent"] = team2,
      _["Y"] = Y,
      _["P"] = P
    );
  }
  
  return Rcpp::List::create(
    _["r"] = out_r, _["p"] = out_p
  );
} 
