#include <Rcpp.h>
using namespace Rcpp;

double r2mu(double r){
  double mu;
  mu = ( r - 1500 ) / 173.7178;
  return(mu);
}
double rd2phi(double rd){
  double phi;
  phi =  rd / 173.7178;
  return(phi);
  
}
double calcGPhi( double phi ){
  double 
    pi = std::atan(1)*4,
    g_phi;
  
  g_phi = 1 / sqrt( 1 + 3 * pow(phi, 2) / pow(pi, 2) );
  return(g_phi);
}
double calcP( double g_phi_j, double mu_i, double mu_j){
    
  double P;
  P = 1/(1 + exp(-g_phi_j * (mu_i - mu_j)));
  return(P);
}
double funX( double X, double delta, double phi, double ni, double a, double tau) {
  double result;
  result = 
    ( exp(X) * ( pow(delta, 2) - pow(phi, 2) - ni - exp(X) ) ) / 
    ( 2 * pow( pow(phi, 2) + ni + exp(X),2) ) - 
    (X - a)/pow(tau, 2);
  
  return(result);
}
double optimSigma( double delta, double sig, double phi, double ni, double tau){
  double 
    a  = 0.0,
    A  = 0.0,
    B  = 0.0,
    C  = 0.0,
    fA = 0.0,
    fB = 0.0,
    fC = 0.0,
    k  = 0.0,
    e = 0.000001;
  
    A = a = log( pow( sig, 2 ) );
    if(delta > phi + ni) {
      B = log( pow(delta, 2) - pow(phi, 2) - ni);
    } else {
      k = 1;
      while( funX(a - k * tau,  delta, phi, ni, a, tau) < 0) 
        k ++;
      
      B = A - k * tau;
    }
    
    
    fA = funX(A, delta, phi, ni, a, tau);
    fB = funX(B, delta, phi, ni, a, tau);
    
    while( std::abs(B) - std::abs(A) > e & k < 20 ){
      // Rcpp::Rcout << "fA("<< k << ")" << fA << " A=" << A <<  std::endl;
      k++;
      C = A + (A- B) * fA / (fB - fA);
      fC = funX(C, delta, phi, ni, a, tau);
      
      if(fC * fB < 0)
        A = B, fA = fB;
      else 
        fA = fA/2;
      
      B = C, fB = fC;
    }
  
  return(A);
}
double updatePhi( double phi, double ni, double sig) {
  double
  prerating_phi,
  updated_phi;
  
  prerating_phi = sqrt( pow(phi, 2) + pow(sig, 2));
  updated_phi   = 1 / sqrt( 1/pow(prerating_phi,2) + 1/ni);
  
  return(updated_phi);
}
double updateMu( double mu, double phi, double d) {
  double updated_mu;
  updated_mu = mu + pow(phi,2) * d;
  return(updated_mu);
}
double mu2r(double mu){
  double r;
  r = mu * 173.7178 + 1500;
  return(r);
}
double phi2rd(double phi){
  double rd;
  rd =  phi * 173.7178;
  return(rd);
  
}

//' Glicko rating for single game
//' 
//' Calculates Glicko rating for single game input
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
//'glicko2(
//'  teams = c( "A", "B", "C", "D" ), 
//'  rank  = c( 3, 4, 1, 2 ), 
//'  days  = c( 0, 0, 0, 0),
//'  r     = c( 1500, 1400, 1550, 1700 ) , 
//'  rd    = c( 200,  30,   100,  300 ),
//'  sig   = c( .06, .06, .05, .07),
//'  tau   = .5,
//'  init_r  = 1500,
//'  init_rd = 100
//')
//' @export
// [[Rcpp::export]]
List 
  glicko2(
    CharacterVector teams, 
    std::vector<int> rank,
    NumericVector r, 
    NumericVector rd,
    NumericVector sig,
    NumericVector days = NumericVector::create(0),
    double tau = .5,
    double init_r  = 1500.00,
    double init_rd = 350.00
  ) {
    
    int n = teams.size();
    int d_size = days.size();
    double 
      d  = 0.0,
      delta = 0.0,
      ni = 0.0, 
      A  = 0.0;
    NumericVector mu(n);
    NumericVector phi(n);
    NumericVector g_phi(n);
    NumericVector ni_i(n);
    NumericVector d_i(n);
    NumericVector delta_i(n);
    NumericMatrix E_s(n, n);
    
    // precalculate 
    for(int i = 0; i < n; i++){
      if(d_size < i + 1) days.push_back(0);
      if( NumericVector::is_na(r[i]) ) 
        r[i] = init_r, rd[i] = init_rd;
      
      // rescale params to glicko2 scale
      mu[i]    = r2mu( r[i] );
      phi[i]   = rd2phi( rd[i] );
      g_phi[i] = calcGPhi( phi[i] );
      
    }
    
    // Sum deviations from expectations  
    for(int i = 0; i < n; i++){
      ni = 0, d  = 0;
      for(int j = 0; j < n; j ++){
        if(j != i){
          E_s(i,j) = calcP(g_phi[j], mu[i], mu[j]);
          ni +=  pow(g_phi[j],2) * E_s(i,j) * ( 1 - E_s(i,j) ) ;
          if( rank[i] < rank[j] ) {
            d += g_phi[j] * ( 1 - E_s(i,j) );
          } else if(rank[i] == rank[j]) { 
            d += g_phi[j] * ( .5 - E_s(i,j) ); 
          } else {
            d += g_phi[j] * ( - E_s(i,j) ); 
          }
        }
      }
      delta = 1/ni * d;
      ni_i[i]  = 1/ni;
      d_i[i]   = d;
      delta_i[i] = delta;
    }
    
    
    // update params
    for(int i = 0; i < n; i++){
      //Rcpp::Rcout << "----- i =" << i <<  std::endl;
      A = optimSigma(delta_i[i], sig[i], phi[i], ni_i[i], tau);
      sig[i] = exp(A/2);
      
      phi[i] = updatePhi(phi[i], ni_i[i], sig[i]);
      mu[i]  = updateMu( mu[i], phi[i], d_i[i]);
      
      r[i]   = mu2r( mu[i] );
      rd[i]  = phi2rd( phi[i] ); 
    }
    
    
    Rcpp::List dimnms = Rcpp::List::create(teams, teams);
    E_s.attr("dimnames") = dimnms;
    r.names()  = teams;
    rd.names() = teams;
  
    return List::create(
      _["r"]    = r,
      _["rd"]   = rd,
      _["sigma"] = sig,
      _["E_s"] = E_s
    );  
  }

