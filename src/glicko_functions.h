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
double calcGRd( double rd){
  double 
  pi = std::atan(1)*4,
  q   = log(10)/400,
  g_rd;
  
  g_rd =   1/sqrt(1 + 3 * pow(q,2) * pow(rd, 2) / pow(pi,2) );
  return(g_rd);
}
double calcGPhi( double phi ){
  double 
  pi = std::atan(1)*4,
    g_phi;
  
  g_phi = 1 / sqrt( 1 + 3 * pow(phi, 2) / pow(pi, 2) );
  return(g_phi);
}
double calcPGlicko( double g_rd_j, double r_i, double r_j){
  double P;
  P = 1/(1 + pow(10, -g_rd_j * (r_i - r_j)/400));
  return(P);
}
double calcPGlicko2( double g_phi_j, double mu_i, double mu_j){
  
  double P;
  P = 1/(1 + exp(-g_phi_j * (mu_i - mu_j)));
  return(P);
}
double calcVar(double var, double g_rd_j, double E_s_ij){
  var += pow(g_rd_j,2) * E_s_ij * ( 1 - E_s_ij );
  return(var);
}
double calcErr( double err,  double g_rd_j, double E_s_ij,double rank_i, double rank_j){
  
  if( rank_i < rank_j ) {
    err += g_rd_j * ( 1 - E_s_ij );
  } else if( rank_i == rank_j ) { 
    err += g_rd_j * ( .5 - E_s_ij ); 
  } else {
    err += g_rd_j * ( - E_s_ij ); 
  }
  
  return(err);
}

double calcZ( double rank_i, double rank_j){
  
  if( rank_i < rank_j ) {
    return(1);
  } else if( rank_i == rank_j ) { 
    return(.5); 
  } else {
    return(0); 
  }
}

double funX( double X, double delta, double phi, double var, double a, double tau) {
  double result;
  result = 
    ( exp(X) * ( pow(delta, 2) - pow(phi, 2) - var - exp(X) ) ) / 
    ( 2 * pow( pow(phi, 2) + var + exp(X),2) ) - 
    (X - a)/pow(tau, 2);
  
  return(result);
}
double optimSigma( double delta, double sig, double phi, double var, double tau){
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
  if(delta > phi + var) {
    B = log( pow(delta, 2) - pow(phi, 2) - var);
  } else {
    k = 1;
    while( funX(a - k * tau,  delta, phi, var, a, tau) < 0) 
      k ++;
    
    B = A - k * tau;
  }
  
  
  fA = funX(A, delta, phi, var, a, tau);
  fB = funX(B, delta, phi, var, a, tau);
  
  while( std::abs(B) - std::abs(A) > e & k < 20 ){
    //Rcpp::Rcout << "fA("<< k << ")" << fA << " A=" << A <<  std::endl;
    k++;
    C = A + (A- B) * fA / (fB - fA);
    fC = funX(C, delta, phi, var, a, tau);
    
    if(fC * fB < 0)
      A = B, fA = fB;
    else 
      fA = fA/2;
    
    B = C, fB = fC;
  }
  
  return(A);
}
double updatePhi( double phi, double var, double sig) {
  double
  prerating_phi,
  updated_phi;
  
  prerating_phi = sqrt( pow(phi, 2) + pow(sig, 2));
  updated_phi   = 1 / sqrt( 1/pow(prerating_phi,2) + 1/var);
  
  return(updated_phi);
}
double updateMu( double mu, double phi, double err) {
  double updated_mu;
  updated_mu = mu + pow(phi,2) * err;
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