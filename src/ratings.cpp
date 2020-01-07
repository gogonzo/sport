#include <Rcpp.h>
#include "ratings.h"
#include "utils.h"
#include "bbt.h"

using namespace Rcpp;
// [[Rcpp::plugins(cpp11)]]

#define DBG(MSG,X) Rprintf("%20s SEXP=<%p>. List=%p\n", MSG, (SEXP)X, &X);

class Ratings {
private:
  // match arguments
  std::string alg;
  int id_i; 
  Rcpp::IntegerVector id_vec;
  Rcpp::IntegerVector rank_vec;
  Rcpp::StringVector team_vec;
  Rcpp::StringVector player_vec;
  Rcpp::NumericVector lambda_vec;
  Rcpp::NumericVector share_vec;
  Rcpp::NumericVector weight_vec;
  
  // rating arguments
  Rcpp::StringVector player_names;
  double init_r;
  double init_rd;
  double init_sigma;
  double beta = 25 / 6; // go to publications and find way to apply gamma in all algorithms
  double gamma = 1.0;   // go to publications and find way to apply gamma in all algorithms
  double kappa;
  double tau;
  
  // in id_i loop
  Rcpp::IntegerVector idx_i;
  Rcpp::StringVector team_vec_i;
  Rcpp::StringVector player_vec_i;
  Rcpp::IntegerVector idx_rating_i;
  Rcpp::NumericVector r_vec_i;
  Rcpp::NumericVector rd_vec_i;
  Rcpp::NumericVector sigma_vec_i;
  Rcpp::StringVector unique_team_i;
  
  // in team_it loop
  Rcpp::IntegerVector idx_it;
  Rcpp::IntegerVector id_vec_it;
  Rcpp::IntegerVector rank_vec_it;
  Rcpp::StringVector player_vec_it;
  Rcpp::IntegerVector idx_rating_it;
  Rcpp::NumericVector r_it;
  Rcpp::NumericVector rd2_it;
  Rcpp::NumericVector rd_it;
  Rcpp::NumericVector sigma2_it;
  Rcpp::NumericVector g_it; // just g instead of g_rd
  
public:
  Rcpp::NumericVector r;
  Rcpp::NumericVector rd;
  Rcpp::NumericVector sigma;
  Rcpp::List out_r;
  Rcpp::List out_p;  
  
  void gatherTeams(int id_i) {
    idx_i = utils::find<int>(id_i, id_vec);
    if (idx_i.size() == 1) return;
    team_vec_i = team_vec[idx_i];
    player_vec_i = player_vec[idx_i];
    
    idx_rating_i = match(player_vec_i, player_names) - 1;
    r_vec_i = r[idx_rating_i];
    rd_vec_i = rd[idx_rating_i];
    if (sigma.size() > 0) {
      sigma_vec_i = sigma[idx_rating_i];
    }
    
    // team specific variables
    unique_team_i = utils::unique(team_vec_i);
    
    int k = unique_team_i.size();
    Rcpp::IntegerVector rank_vec_it_(k);
    Rcpp::NumericVector r_it_(k);
    Rcpp::NumericVector rd_it_(k);
    Rcpp::NumericVector rd2_it_(k);
    Rcpp::NumericVector sigma2_it_(k);
    double r_sum, rd_ssq, sigma_ssq, idx_r, idx_df;
    std::string team_t;
    
    for (int t = 0; t < k; t++) {
      team_t = unique_team_i(t);
      idx_it = utils::find<std::string>(team_t, team_vec_i) + idx_i(0);
      
      player_vec_it = player_vec[idx_it];
      idx_rating_it = Rcpp::match(player_vec_it, player_names) - 1;
      
      r_sum = 0.0;
      rd_ssq = 0.0;
      sigma_ssq = 0.0;
      for (int p = 0; p < idx_rating_it.size(); p++) {
        idx_r = idx_rating_it(p);
        idx_df = idx_it(p);
        
        // update rd before by lambda
        r_sum += r(idx_r) * share_vec(idx_df);
        rd_ssq += pow(rd(idx_r) * lambda_vec(idx_df) * share_vec(idx_df), 2.0);
        if (sigma.size() > 0) {
          sigma_ssq += pow(sigma(idx_r) * share_vec(idx_df), 2.0);          
        }
      }
      
      rank_vec_it_(t) = rank_vec(idx_df);
      
      r_it_(t)  = r_sum;
      rd_it_(t) = sqrt(rd_ssq);
      rd2_it_(t) = rd_ssq;
      if (sigma.size() > 0) {
        sigma2_it_(t) = sigma_ssq;          
      }
    }
    
    this -> id_i = id_i;
    this -> r_it = r_it_;
    this -> rd_it = rd_it_;
    this -> rd2_it = rd2_it_;
    this -> sigma2_it = sigma2_it_;
    this -> rank_vec_it = rank_vec_it_;
    
    Rcpp::DataFrame out_r_i = Rcpp::DataFrame::create(
      _["id"] = id_i,
      _["team"] = team_vec_i,
      _["player"] = player_vec_i,
      _["r"] = r_vec_i, 
      _["rd"] = rd_vec_i,
      _["sigma"] = sigma.size() > 0 ? sigma_vec_i : 1.0,
      _["stringsAsFactors"] = false
    );  
    
    out_r.push_back(out_r_i);
  };
  void precalculateGlicko() {
    if (idx_i.size() == 1) return;
    // precalculate g
    int k = unique_team_i.size();
    Rcpp::NumericVector g_it_(k);
    
    for (int t = 0; t < k; t++) {
      g_it_(t) = calcGRd(rd_it(t));
    }
    
    this -> g_it = g_it_;
  };
  void precalculateGlicko2() {
    if (idx_i.size() == 1) return;
    // precalculate g
    int k = unique_team_i.size();
    Rcpp::NumericVector g_it_(k);
    
    for (int t = 0; t < k; t++) {
      r_it(t) = r2mu(r_it(t));
      rd_it(t) = rd2phi(rd_it(t));
      g_it_(t) = calcGPhi(rd_it(t));
    }
    
    this -> g_it = g_it_;
  };
  void updateGlicko() {
    if (idx_i.size() == 1) return;
    // GLICKO RATING
    double var, err, q = log(10.0) / 400.0;
    int idx = 0, k = unique_team_i.size();
    CharacterVector team1(k * k - k);
    CharacterVector team2(k * k - k);
    NumericVector P(k * k - k);
    NumericVector Y(k * k - k);
    NumericVector delta(k, 0.0);
    NumericVector error(k, 0.0);
    NumericVector variance(k, 0.0);
    
    for (int p = 0; p < k; p++) {
      var = 0.0;
      err = 0.0;
      for (int o = 0; o < k; o++) {
        if (o != p) {
          
          team1(idx) = unique_team_i[p];
          team2(idx) = unique_team_i[o];
          
          P(idx) = calcPGlicko(
            calcGRd(sqrt(rd2_it(p) + rd2_it(o))), 
            r_it(p), 
            r_it(o)
          );
          
          Y(idx) = calcZ(rank_vec_it(p), rank_vec_it(o));
          
          var += calcVar(g_it(o), P(idx));
          err += calcErr(g_it(o), P(idx), rank_vec_it(p), rank_vec_it(o)); 
          
          // this event ratings
          error(p)   = err;
          variance(p)   = var;
          delta(p) =  gamma * 1.0 / (pow(q, 2.0) * var);
          
          idx++;
        }
      }
    }
    
    // update parameters 
    std::string team_t;
    int idx_r, idx_df;
    double rd_update, r_update, multiplier;
    for (int t = 0; t < k; t++) {
      team_t = unique_team_i(t);
      idx_it = utils::find<std::string>(team_t, team_vec_i) + idx_i(0);
      
      player_vec_it = player_vec[idx_it];
      idx_rating_it = Rcpp::match(player_vec_it, player_names) - 1;
      
      
      r_update = q / 
        (1 / rd2_it(t) + 1 / delta(t)) * 
        error(t);
      
      rd_update = (
        sqrt(rd2_it(t)) - 
          sqrt(1 / (1 / rd2_it(t) + 1 / (delta(t))))
      );
      
      
      for (int p = 0; p < idx_rating_it.size(); p++) {
        idx_r = idx_rating_it(p);
        idx_df = idx_it(p);
        
        multiplier = (pow(rd(idx_r), 2.0) / rd2_it(t)) *
          weight_vec(idx_df) *
          share_vec(idx_df);
        
        r(idx_r) = r(idx_r) + r_update * multiplier;
        
        rd(idx_r) = ((rd(idx_r) - rd_update * multiplier) <  (rd(idx_r) * kappa)) ?
        rd(idx_r) * kappa : rd(idx_r) - rd_update * multiplier;
      }
    }
    
    Rcpp::DataFrame out_p_i = Rcpp::DataFrame::create(
      _["id"] = id_i,
      _["team"] = team1,
      _["opponent"] = team2,
      _["Y"] = Y,
      _["P"] = P,
      _["stringsAsFactors"] = false
    );
    
    out_p.push_back(out_p_i);
  }
  void updateGlicko2() {
    if (idx_i.size() == 1) return;
    double var, err;
    int idx = 0, k = unique_team_i.size();
    CharacterVector team1(k * k - k);
    CharacterVector team2(k * k - k);
    NumericVector P(k * k - k);
    NumericVector Y(k * k - k);
    NumericVector delta(k, 0.0);
    NumericVector error(k, 0.0);
    NumericVector variance(k, 0.0);
    
    for (int p = 0; p < k; p++) {
      var = 0.0;
      err = 0.0;
      for (int o = 0; o < k; o++) {
        if (o != p) {
          team1(idx) = unique_team_i[p];
          team2(idx) = unique_team_i[o];
          
          P(idx) = calcPGlicko2(
            // if g_it(o) only below then result will be as in glicko2 publication
            sqrt(pow(g_it(p), 2.0) + pow(g_it(o), 2.0)), // pow(beta, 2.0) can be added here
            r_it(p),
            r_it(o));
          
          Y(idx) = calcZ(rank_vec_it(p), rank_vec_it(o));
          
          var += calcVar(g_it(o), P(idx));
          err += calcErr(g_it(o), P(idx), rank_vec_it(p), rank_vec_it(o)); 
        } else { 
          continue; 
        }
        idx += 1;
      }
      // this event ratings
      error(p)   = err;
      variance(p)   = 1 / var;
      delta(p) =  1 / var * err;
    }
    Rcpp::DataFrame out_p_i = Rcpp::DataFrame::create(
      _["id"] = id_i,
      _["team"] = team1,
      _["opponent"] = team2,
      _["Y"] = Y,
      _["P"] = P,
      _["stringsAsFactors"] = false
    );
    
    out_p.push_back(out_p_i);
    
    // update parameters
    std::string team_t;
    int idx_r, idx_df;
    double sigma_new, phi_new, r_update, rd_update, A, multiplier;
    
    for (int t = 0; t < k; t++) {
      // r_it and rd_it is scaled to mu and phi, rd2_it is normal
      team_t = unique_team_i(t);
      idx_it = utils::find<std::string>(team_t, team_vec_i) + idx_i(0);
      
      player_vec_it = player_vec[idx_it];
      idx_rating_it = Rcpp::match(player_vec_it, player_names) - 1;
      
      A = optimSigma(delta(t), sqrt(sigma2_it(t)), rd_it(t), variance(t), tau);
      sigma_new = exp(A / 2);  
      
      phi_new = updatePhi(rd_it(t), variance(t), sigma_new);
      if (phi_new > (init_rd/173.7178)) phi_new = init_rd/173.7178;
      
      r_update = pow(phi_new, 2.0) * error(t) * 173.7178;
      rd_update = sqrt(rd2_it(t)) - phi_new * 173.7178;
      
      if (rd_update > (sqrt(rd2_it(t)) * (1 - kappa))) {
        rd_update = sqrt(rd2_it(t)) * (1 - kappa);
      } 
      
      for (int p = 0; p < idx_rating_it.size(); p++) {
        idx_r = idx_rating_it(p);
        idx_df = idx_it(p);
        
        multiplier = (pow(rd(idx_r), 2.0) / rd2_it(t)) *
          weight_vec(idx_df) *
          share_vec(idx_df);
        
        r(idx_r) = r(idx_r) + r_update * multiplier;
        rd(idx_r) = rd(idx_r) - rd_update * multiplier;
        sigma(idx_r) = sigma(idx_r) + (sigma(idx_r) - sigma_new) * multiplier;
      }
    }
  }
  void updateBBT() {
    if (idx_i.size() == 1) return;
    int idx = 0, k = unique_team_i.size();
    double c, gamma;
    
    // calculate update for teams
    CharacterVector team1(k * k - k);
    CharacterVector team2(k * k - k);
    NumericVector P(k * k - k);
    NumericVector Y(k * k - k);
    NumericVector omega(k, 0.0);
    NumericVector delta(k, 0.0);
    
    for (int p = 0; p < k; p++) {
      for (int o = 0; o < k; o++) {
        if (p != o) {
          c = sqrt(rd2_it(p) + rd2_it(o) + pow(beta, 2.0)); // find a way to apply in other algorithms
          gamma = sqrt(rd2_it(p)) / c;
          
          team1(idx) = unique_team_i(p);
          team2(idx) = unique_team_i(o);
          Y(idx) = calc_s(rank_vec_it(p), rank_vec_it(o));
          P(idx) = exp(r_it(p) / c) / 
            (exp(r_it(p) / c) +  exp(r_it(o) / c));
          
          omega(p) = omega(p) + 
            rd2_it(p) / 
            c *
            (Y(idx) - P(idx));
          
          delta(p) = delta(p) +
            gamma *
            pow(sqrt(rd2_it(p)) / c, 2.0) *
            (P(idx) * (1 - P(idx)));
          
          idx += 1;   
        }
      }
    }
    
    // update player ratings
    double multiplier, rd_update;
    int idx_r, idx_df;
    std::string team_t;
    for (int t = 0; t < k; t++) {
      team_t = unique_team_i(t);
      idx_it = utils::find<std::string>(team_t, team_vec_i) + idx_i(0);
      
      player_vec_it = player_vec[idx_it];
      idx_rating_it = Rcpp::match(player_vec_it, player_names) - 1;
      
      for (int p = 0; p < idx_rating_it.size(); p++) {
        idx_r = idx_rating_it(p);
        idx_df = idx_it(p);
        
        multiplier = (pow(rd(idx_r), 2.0) / rd2_it(t)) *
          weight_vec(idx_df) *
          share_vec(idx_df);
        
        r(idx_r) = r(idx_r) + omega(t) * multiplier;
        
        rd_update = (
          rd(idx_r) - 
            sqrt(pow(rd(idx_r), 2.0) * (1 - pow(rd(idx_r), 2.0) / sum(rd2_it) * delta(t)))
        ) * multiplier;
        
        rd(idx_r) = ((rd(idx_r) - rd_update) <  (rd(idx_r) * kappa)) ?
        rd(idx_r) * kappa : 
          rd(idx_r) - rd_update;
      }
    }
    
    Rcpp::DataFrame out_p_i = Rcpp::DataFrame::create(
      _["id"] = id_i,
      _["team"] = team1,
      _["opponent"] = team2,
      _["Y"] = Y,
      _["P"] = P,
      _["stringsAsFactors"] = false
    );
    
    out_p.push_back(out_p_i);
  }
  
  Ratings(Rcpp::IntegerVector& id_vec,
          Rcpp::IntegerVector& rank_vec,
          
          Rcpp::StringVector& team_vec,
          Rcpp::StringVector& player_vec,
          
          Rcpp::NumericVector& lambda_vec,
          Rcpp::NumericVector& share_vec,
          Rcpp::NumericVector& weight_vec,
          
          Rcpp::NumericVector r, 
          Rcpp::NumericVector rd,
          Rcpp::NumericVector sigma,
          
          double init_r,
          double init_rd,
          double init_sigma,
          
          double kappa,
          double tau);
  
  void test() {
    DBG("r in Ratings class: %p", r);
  }
};

Ratings::Ratings(
  Rcpp::IntegerVector& id_vec_val,
  Rcpp::IntegerVector& rank_vec_val,
  
  Rcpp::StringVector& team_vec_val, 
  Rcpp::StringVector& player_vec_val, 
  
  Rcpp::NumericVector& lambda_vec_val,
  Rcpp::NumericVector& share_vec_val,
  Rcpp::NumericVector& weight_vec_val,
  
  Rcpp::NumericVector r_val, 
  Rcpp::NumericVector rd_val,
  Rcpp::NumericVector sigma_val,
  
  double init_r_val,
  double init_rd_val,
  double init_sigma_val,
  
  double kappa_val,
  double tau_val) {
  
  // rank and name must be of length n
  this -> id_vec = id_vec_val;
  this -> rank_vec = rank_vec_val;
  this -> team_vec = team_vec_val;
  this -> player_vec = player_vec_val;
  
  this -> lambda_vec = lambda_vec_val;
  this -> share_vec = share_vec_val;
  this -> weight_vec = weight_vec_val;
  
  this -> player_names = r_val.names();
  this -> r = clone(r_val);
  this -> rd = clone(rd_val);
  this -> sigma = clone(sigma_val);
  
  this -> init_r = init_r_val;
  this -> init_rd = init_rd_val;
  this -> init_sigma = init_sigma_val;
  
  this -> kappa = kappa_val;
  this -> tau = tau_val;
}

// [[Rcpp::export]]
List 
  glicko(
    IntegerVector unique_id,
    IntegerVector id,
    IntegerVector rank,
    StringVector team, 
    StringVector player,
    
    NumericVector r, 
    NumericVector rd,
    NumericVector sigma,
    
    NumericVector share,
    NumericVector lambda,
    NumericVector weight,
    
    double init_r,
    double init_rd,
    double init_sigma,
    double kappa,
    double tau
  ) {
    
    int id_i;
    StringVector player_names = r.names();
    
    
    Ratings ratings{
      id, rank, team, player, lambda, share, weight, // match vectors
      r, rd, sigma, init_r, init_rd, init_sigma,     // ratings 
      kappa, tau                                     // constants
    };
    
    for (int i = 0; i < unique_id.size(); i++) {
      id_i = unique_id(i);
      ratings.gatherTeams(id_i);
      ratings.precalculateGlicko();
      ratings.updateGlicko();
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
    IntegerVector unique_id,
    IntegerVector id,
    IntegerVector rank,
    StringVector team, 
    StringVector player,
    
    NumericVector r, 
    NumericVector rd,
    NumericVector sigma,
    
    NumericVector share,
    NumericVector lambda,
    NumericVector weight,
    
    double init_r,
    double init_rd,
    double init_sigma,
    double kappa,
    double tau
  ) {
    
    int id_i;
    StringVector player_names = r.names();
    
    Ratings ratings{
      id, rank, team, player, lambda, share, weight, // match vectors
      r, rd, sigma, init_r, init_rd, init_sigma,     // ratings 
      kappa, tau                                     // constants
    };
    
    for (int i = 0; i < unique_id.size(); i++) {
      id_i = unique_id(i);
      ratings.gatherTeams(id_i);
      ratings.precalculateGlicko2();
      ratings.updateGlicko2();
    }
    
    return Rcpp::List::create(
      _["r"] = ratings.out_r,
      _["p"] = ratings.out_p,
      _["final_r"] = ratings.r,
      _["final_rd"] = ratings.rd,
      _["final_sigma"] = ratings.sigma
    );
    
    
  }


// [[Rcpp::export]]
Rcpp::List bbt(
    IntegerVector unique_id,
    IntegerVector id,
    IntegerVector rank,
    StringVector team, 
    StringVector player,
    
    NumericVector r, 
    NumericVector rd,
    NumericVector sigma,
    
    NumericVector share,
    NumericVector lambda,
    NumericVector weight,
    
    double init_r,
    double init_rd,
    double init_sigma,
    
    double kappa,
    double tau) {
  
  int id_i;
  StringVector player_names = r.names();
  
  Ratings ratings{
    id, rank, team, player, lambda, share, weight, // match vectors
    r, rd, sigma, init_r, init_rd, init_sigma,     // ratings 
    kappa, tau                                     // constants
  };
  
  for (int i = 0; i < unique_id.size(); i++) {
    id_i = unique_id(i);
    ratings.gatherTeams(id_i);
    ratings.updateBBT();
  }
  
  return Rcpp::List::create(
    _["r"] = ratings.out_r,
    _["p"] = ratings.out_p,
    _["final_r"] = ratings.r,
    _["final_rd"] = ratings.rd
  );
  
} 

// [[Rcpp::export]]
Rcpp::List 
  dbl(
    Rcpp::IntegerVector unique_id,
    Rcpp::IntegerVector id_vec,
    Rcpp::IntegerVector rank_vec,
    Rcpp::StringVector team_vec,
    Rcpp::StringMatrix MAP,
    Rcpp::NumericMatrix X,
    Rcpp::StringVector cls,
    Rcpp::NumericVector R, 
    Rcpp::NumericVector RD,
    Rcpp::NumericVector lambda_vec,
    Rcpp::NumericVector weight_vec,
    double kappa
  ) {
    Rcpp::List out_p = Rcpp::List::create();
    Rcpp::List out_r = Rcpp::List::create();
    
    int idx;
    double pi = 3.1415926535;
    double s2, Ks, prob, y, y_var, error, delta_;
    
    Rcpp::NumericVector idx_i;
    Rcpp::StringVector team_vec_i;
    Rcpp::IntegerVector rank_vec_i;
    Rcpp::NumericVector lambda_vec_i;
    Rcpp::NumericVector weight_vec_i;
    
    Rcpp::StringMatrix MAP_i;
    Rcpp::NumericMatrix X_i;
    Rcpp::IntegerMatrix Idx_i;
    Rcpp::NumericMatrix R_i;
    Rcpp::NumericMatrix RD_i;
    
    int id_i;
    Rcpp::StringVector param_names = R.names();
    for (int i = 0; i < unique_id.size(); i++) {
      id_i  = unique_id(i);
      idx_i = utils::find<int>(id_i, id_vec);
      if (idx_i.size() == 1) continue;
      
      team_vec_i = team_vec[idx_i];
      rank_vec_i = rank_vec[idx_i];
      lambda_vec_i = lambda_vec[idx_i];
      weight_vec_i = weight_vec[idx_i];
      
      MAP_i = subset_matrix(MAP, idx_i);
      X_i   = subset_matrix(X, idx_i);
      Idx_i = term_matrix_idx(MAP_i, param_names);
      R_i   = term_matrix(Idx_i, R);
      RD_i  = term_matrix(Idx_i, RD);
      
      int n = MAP_i.nrow();
      int k = MAP_i.ncol();
      
      Rcpp::StringVector team2(n * n - n);
      Rcpp::StringVector team1(n * n - n);
      Rcpp::NumericVector P(n * n - n);
      Rcpp::NumericVector Y(n * n - n);
      Rcpp::NumericVector r_update(k);
      Rcpp::NumericVector rd_update(k);
      Rcpp::NumericVector x_p(k);
      Rcpp::NumericVector x_q(k);
      Rcpp::NumericVector r_p(k);
      Rcpp::NumericVector r_q(k);
      Rcpp::NumericVector rd_p(k);
      Rcpp::NumericVector rd_q(k);
      
      Rcpp::NumericMatrix OMEGA(n, k);
      Rcpp::NumericMatrix DELTA(n, k);
      
      idx = 0;
      for (int p = 0; p < n; p++) {
        x_p = X_i(p,_);
        r_p = R_i(p,_);
        rd_p = RD_i(p,_) * lambda_vec_i(p);
        for (int q = 0; q < n; q++) {
          if (p != q) {
            
            x_q  = -X_i(q,_);
            r_q  =  R_i(q,_);
            rd_q =  RD_i(q,_) * lambda_vec_i(q);
            
            team1(idx) = team_vec_i(p);
            team2(idx) =  team_vec_i(q);
            
            // activation variance
            s2 = sum(x_p * rd_p * x_p) + sum(x_q * rd_q * x_q);
            Ks = 1 / sqrt(1 + pi * s2 / 8);
            
            // probability and output
            prob = 1 / (1 + exp(-Ks * (sum(r_p * x_p) + sum(r_q * x_q))));
            y = dbl_calc_y(rank_vec_i(p), rank_vec_i(q));
            error = y - prob;
            
            P(idx) = prob;
            Y(idx) = y;
            
            // calculating update
            y_var = 1 / (1 + prob * (1 - prob) * s2);
            
            OMEGA(p, _) =  OMEGA(p, _) + 
              ((rd_p * y_var) * 
              (x_p * error));
            
            DELTA(p, _) = DELTA(p, _) +  
              (prob * (1 - prob) * y_var) * 
              (rd_p * x_p) * 
              (rd_p * x_p); 
            
            idx++;
          }
        }
      }
      
      // update params and save history 
      Rcpp::StringVector team_long(n * k);
      Rcpp::StringVector param_long(n * k);
      Rcpp::NumericVector R_long(n * k);
      Rcpp::NumericVector RD_long(n * k);
      idx = 0;
      for (int i = 0; i < n; i++) {
        for (int j = 0; j < k; j++) {
          team_long(idx) = team_vec_i(i);
          param_long(idx) = MAP_i(i, j);
          R_long(idx) = R_i(i, j);
          RD_long(idx) = RD_i(i, j);
          idx++;
          
          R(Idx_i(i, j)) = R(Idx_i(i, j)) + 
            OMEGA(i, j) *
            weight_vec_i(i) / 
            k;
          
          delta_ = DELTA(i, j) * weight_vec_i(i) / k;
          
          if (delta_ > (RD(Idx_i(i, j)) * (1 - kappa))) {
            RD(Idx_i(i, j)) = RD(Idx_i(i, j)) - 
              RD(Idx_i(i, j)) * 
              (1 - kappa);           
            
          } else {
            RD(Idx_i(i, j)) = 
              RD(Idx_i(i, j)) - 
              delta_;            
          }
        } 
      }
      
      Rcpp::DataFrame out_r_i = Rcpp::DataFrame::create(
        _["id"] = id_i,
        _["team"] = team_long,
        _["param"] = param_long,
        _["R"] = R_long,
        _["RD"] = RD_long,
        _["stringsAsFactors"] = false
      );      
      
      
      Rcpp::DataFrame out_p_i = Rcpp::DataFrame::create(
        _["id"] = id_i,
        _["team"] = team1,
        _["opponent"] = team2,
        _["Y"] = Y,
        _["P"] = P,
        _["stringsAsFactors"] = false
      );
      
      
      out_p.push_back(out_p_i);
      out_r.push_back(out_r_i);
      
    }
    
    return Rcpp::List::create(
      _["r"] = out_r,
      _["p"] = out_p,
      _["final_r"] = R,
      _["final_rd"] = RD
      
    );
  }
