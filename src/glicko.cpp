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
  Rcpp::IntegerVector id_vec;
  Rcpp::IntegerVector rank_vec;
  Rcpp::StringVector team_vec;
  Rcpp::StringVector player_vec;
  Rcpp::NumericVector beta_vec;
  Rcpp::NumericVector lambda_vec;
  Rcpp::NumericVector share_vec;
  Rcpp::NumericVector weight_vec;
  
  // rating arguments
  Rcpp::StringVector player_names;
  double init_r;
  double init_rd;
  double init_sigma;
  double beta;
  double gamma;
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
        rd(idx_r) = rd(idx_r) * lambda_vec(idx_df);
        r_sum += r(idx_r) * share_vec(idx_df);
        rd_ssq += pow(rd(idx_r) * share_vec(idx_df), 2.0);
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
    
    // precalculate g
    int k = unique_team_i.size();
    Rcpp::NumericVector g_it_(k);
    
    for (int t = 0; t < k; t++) {
      g_it_(t) = calcGRd(rd_it(t));
    }
    
    this -> g_it = g_it_;
  };
  void precalculateGlicko2() {
    
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
          delta(p) =  gamma * 1 / (pow(q, 2.0) * var);
          
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
          
          P(idx) = calcPGlicko2(
            // if g_it(o) only below then result will be as in glicko2 publication
            sqrt(pow(g_it(p), 2.0) + pow(g_it(o), 2.0)), 
            r_it(p),
            r_it(o));
          
          Y(idx) = calcZ(rank_vec_it(p), rank_vec_it(o));
          
          var += calcVar(g_it(o), P(idx));
          err += calcErr(g_it(o), P(idx), rank_vec_it(p), rank_vec_it(o)); 
        } else { 
          continue; 
        }
      }
      // this event ratings
      error(p)   = err;
      variance(p)   = 1 / var;
      delta(p) =  1 / var * err;
      idx += 1;
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
          c = sqrt(rd2_it(p) + rd2_it(o) + pow(beta, 2.0));
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
          
          double beta,
          double gamma,
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
  
  double beta_val,
  double gamma_val,
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
  
  this -> beta = beta_val;
  this -> gamma = gamma_val;
  this -> kappa = kappa_val;
  this -> tau = tau_val;
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
    double beta,
    double gamma,
    double kappa,
    double tau
    ) {

    int id_i;
    StringVector player_names = r.names();
    
    
    Ratings ratings{
      id, rank, team, player, lambda, share, weight, // match vectors
      r, rd, sigma, init_r, init_rd, init_sigma,     // ratings 
      beta, gamma, kappa, tau                        // constants
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
    double beta,
    double gamma,
    double kappa,
    double tau
  ) {
    
    int id_i;
    StringVector player_names = r.names();

    Ratings ratings{
      id, rank, team, player, lambda, share, weight, // match vectors
      r, rd, sigma, init_r, init_rd, init_sigma,     // ratings 
      beta, gamma, kappa, tau                        // constants
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
    
    double beta,
    double gamma,
    double kappa,
    double tau) {
  
  int id_i;
  StringVector player_names = r.names();
  
  Ratings ratings{
    id, rank, team, player, lambda, share, weight, // match vectors
    r, rd, sigma, init_r, init_rd, init_sigma,     // ratings 
    beta, gamma, kappa, tau                        // constants
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
Rcpp::List bbt2(
    Rcpp::IntegerVector unique_id,
    Rcpp::IntegerVector id,
    Rcpp::IntegerVector rank,
    Rcpp::StringVector team,
    Rcpp::StringVector player,
    Rcpp::NumericVector r_val, 
    Rcpp::NumericVector rd_val,
    Rcpp::NumericVector lambda,
    Rcpp::NumericVector share,
    Rcpp::NumericVector weight,
    double kappa = 0.5,
    double beta = 25 / 6,
    double init_r = 25.0,
    double init_rd = 25 / 3) {
  
  Rcpp::NumericVector r = clone(r_val);
  Rcpp::NumericVector rd = clone(rd_val);
  
  Rcpp::List out_r, out_p;  
  Rcpp::DataFrame out_r_i, out_p_i;  
  
  Rcpp::IntegerVector idx_i;
  Rcpp::IntegerVector idx_it;
  Rcpp::IntegerVector idx_rating_i;
  Rcpp::IntegerVector id_vec_i;
  Rcpp::IntegerVector rank_vec_i;
  Rcpp::StringVector team_vec_i;
  Rcpp::StringVector player_vec_i;
  Rcpp::NumericVector r_vec_i;
  Rcpp::NumericVector rd_vec_i;
  Rcpp::StringVector unique_team_i;
  
  Rcpp::StringVector player_names = r.names();  
  Rcpp::StringVector player_vec_it;
  Rcpp::IntegerVector idx_rating_it;
  Rcpp::NumericVector r_it;
  Rcpp::NumericVector rd2_it;
  
  int idx, idx_r, idx_df;
  int id_i;
  std::string team_t;
  
  double c, gamma, r_sum, rd_ssq, rd_update;
  
  for (int i = 0; i < unique_id.size(); i++) {
    id_i = unique_id(i);
    
    // player specific variables
    idx_i = utils::find<int>(id_i, id);
    team_vec_i = team[idx_i];
    player_vec_i = player[idx_i];
    idx_rating_i = match(player_vec_i, player_names) - 1;
    r_vec_i = r[idx_rating_i];
    rd_vec_i = rd[idx_rating_i];
    
    // team specific variables
    unique_team_i = utils::unique(team_vec_i);
    
    int k = unique_team_i.size();
    Rcpp::IntegerVector rank_vec_it(k);
    Rcpp::NumericVector r_it(k);
    Rcpp::NumericVector rd2_it(k);
    
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
        
        // update rd before by lambda
        rd(idx_r) = rd(idx_r) * lambda(idx_df);
        
        r_sum += r(idx_r) * share(idx_df);
        rd_ssq += pow(rd(idx_r) * share(idx_df), 2.0);
      }
      
      rank_vec_it(t) = rank(idx_df);
      r_it(t)  = r_sum;
      rd2_it(t) = rd_ssq;
    }
    
    out_r_i = Rcpp::DataFrame::create(
      _["id"] = id_i,
      _["team"] = team_vec_i,
      _["player"] = player_vec_i,
      _["r"] = r_vec_i, 
      _["rd"] = rd_vec_i,
      _["stringsAsFactors"] = false
    );
    
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
          c = sqrt(rd2_it(p) + rd2_it(q) + pow(beta, 2.0));
          gamma = sqrt(rd2_it(p)) / c;
          
          team1(idx) = unique_team_i(p);
          team2(idx) = unique_team_i(q);
          Y(idx) = calc_s(rank_vec_it(p), rank_vec_it(q));
          P(idx) = exp(r_it(p) / c) / 
            (exp(r_it(p) / c) +  exp(r_it(q) / c));
          
          omega(p) = omega(p) +
            rd2_it(p) / c *
            (Y(idx) - P(idx));
          
          delta(p) = 
            delta(p) +
            gamma *
            pow(sqrt(rd2_it(p)) / c, 2.0) *
            (P(idx) * (1 - P(idx)));
          
          idx += 1;   
        }
      }
    }
    
    // update player ratings
    for (int t = 0; t < k; t++) {
      team_t = unique_team_i(t);
      idx_it = utils::find<std::string>(team_t, team_vec_i) + idx_i(0);
      
      player_vec_it = player[idx_it];
      idx_rating_it = Rcpp::match(player_vec_it, player_names) - 1;
      
      for (int p = 0; p < idx_rating_it.size(); p++) {
        idx_r = idx_rating_it(p);
        idx_df = idx_it(p);
        
        r(idx_r) = r(idx_r) + 
          omega(t) * 
          (pow(rd(idx_r), 2.0) / rd2_it(t)) *
          weight(idx_df) *
          share(idx_df);
        
        rd_update = (
          rd(idx_r) - 
            sqrt(pow(rd(idx_r), 2.0) * (1 - pow(rd(idx_r), 2.0) / sum(rd2_it) * delta(t)))
        ) * weight(idx_df) * share(idx_df);
        
        rd(idx_r) = ((rd(idx_r) - rd_update) <  (rd(idx_r) * kappa)) ?
        rd(idx_r) * kappa : 
          rd(idx_r) - rd_update;
      }
    }
    
    out_p_i = Rcpp::DataFrame::create(
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
    _["final_r"] = r,
    _["final_rd"] = rd,
    _["r"] = out_r,
    _["p"] = out_p
  );
} 
