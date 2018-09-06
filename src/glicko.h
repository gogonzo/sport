double r2mu(double r){
  return ( r - 1500 ) / 173.7178;
}
double rd2phi(double rd){
  return rd / 173.7178;
}
double calcGRd( double rd){
  double pi = std::atan(1)*4, q   = log(10.0)/400.0;
  return 1.0/sqrt(1.0 + 3.0 * pow(q,2.0) * pow(rd, 2.0) / pow(pi,2.0) );
}
double calcGPhi( double phi ){
  double pi = std::atan(1)*4.0;
  return 1 / sqrt( 1.0 + 3.0 * pow(phi, 2.0) / pow(pi, 2.0) );
}
double calcPGlicko( double g_rd_j, double r_i, double r_j){
  return  1/(1 + pow(10.0, -g_rd_j * (r_i - r_j)/400));
}
double calcPGlicko2( double g_phi_j, double mu_i, double mu_j){
  return 1/(1 + exp(-g_phi_j * (mu_i - mu_j)));
}
double calcVar(double var, double g_rd_j, double E_s_ij){
  return var + pow(g_rd_j,2) * E_s_ij * ( 1 - E_s_ij );
}
double calcErr( double err,  double g_rd_j, double E_s_ij,double rank_i, double rank_j){
  
  if( rank_i < rank_j ) {
    return err + g_rd_j * ( 1.0 - E_s_ij );
  } else if( rank_i == rank_j ) { 
    return err + g_rd_j * ( .5 - E_s_ij ); 
  } else {
    return err + g_rd_j * ( - E_s_ij ); 
  }
  
  return err;
}

double calcZ( double rank_i, double rank_j){
  
  if( rank_i < rank_j ) {
    return 1;
  } else if( rank_i == rank_j ) { 
    return .5; 
  } else {
    return 0; 
  }
}

double funX( double X, double delta, double phi, double var, double a, double tau) {
  return 
    ( exp(X) * ( pow(delta, 2.0) - pow(phi, 2.0) - var - exp(X) ) ) / 
    ( 2 * pow( pow(phi, 2.0) + var + exp(X),2.0) ) - 
    (X - a)/pow(tau, 2.0);
  
}
double optimSigma( double delta, double sigma, double phi, double var, double tau){
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
  
  A = a = log( pow( sigma, 2.0 ) );
  if(delta > phi + var) {
    B = log( pow(delta, 2.0) - pow(phi, 2.0 ) - var);
  } else {
    k = 1;
    while( funX(a - k * tau,  delta, phi, var, a, tau) < 0) 
      k ++;
    
    B = A - k * tau;
  }
  
  
  fA = funX(A, delta, phi, var, a, tau);
  fB = funX(B, delta, phi, var, a, tau);
  
  while( ( (std::abs(B) - std::abs(A) ) > e) & (k < 20) ){
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
  
  return A;
}

double updatePhi( double phi, double var, double sigma, double w) {
  double prerating_phi;
  prerating_phi = sqrt( pow(phi, 2.0) + pow(sigma, 2.0))  * w ;
  return 1 / sqrt( 1/pow(prerating_phi,2.0) + 1/var);
}

double updateMu( double mu, double phi, double err, double w) {
  return mu + pow(phi,2.0) * err * w;
}

double mu2r(double mu){
  return mu * 173.7178 + 1500;
}
double phi2rd(double phi){
  return phi * 173.7178;
}