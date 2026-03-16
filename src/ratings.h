// Precomputed constants
static const double CONST_PI = 3.14159265358979323846;
static const double CONST_Q  = 0.00575646273248511; // log(10) / 400
static const double CONST_3Q2_PI2 = 3.0 * CONST_Q * CONST_Q / (CONST_PI * CONST_PI);
static const double CONST_3_PI2 = 3.0 / (CONST_PI * CONST_PI);
static const double CONST_GLICKO2_SCALE = 173.7178;

double r2mu(double r){
  return ( r - 1500 ) / CONST_GLICKO2_SCALE;
}
double rd2phi(double rd){
  return rd / CONST_GLICKO2_SCALE;
}
double calcGRd( double rd){
  return 1.0 / sqrt(1.0 + CONST_3Q2_PI2 * rd * rd);
}
double calcGPhi( double phi ){
  return 1.0 / sqrt(1.0 + CONST_3_PI2 * phi * phi);
}
double calcPGlicko( double g_rd_j, double r_i, double r_j){
  return  1.0/(1.0 + pow(10.0, -g_rd_j * (r_i - r_j)/400.0));
}
double calcPGlicko2(double g_phi_j, double mu_i, double mu_j){
  return 1.0/(1.0 + exp(-g_phi_j * (mu_i - mu_j)));
}
double calcVar( double g_rd_j, double E_s_ij){
  return g_rd_j * g_rd_j * E_s_ij * ( 1.0 - E_s_ij );
}
double calcErr( double g_rd_j, double E_s_ij,double rank_i, double rank_j){

  if( rank_i < rank_j ) {
    return g_rd_j * ( 1.0 - E_s_ij );
  } else if( rank_i == rank_j ) {
    return g_rd_j * ( .5 - E_s_ij );
  } else {
    return g_rd_j * ( - E_s_ij );
  }

  return 0;
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
  double expX = exp(X);
  double phi2 = phi * phi;
  double denom = phi2 + var + expX;
  return
    ( expX * ( delta * delta - phi2 - var - expX ) ) /
    ( 2.0 * denom * denom ) -
    ( X - a ) / ( tau * tau );

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

  A = a = log( sigma * sigma );
  if(delta > phi + var) {
    B = log( delta * delta - phi * phi - var);
  } else {
    k = 1;
    while( funX(a - k * tau,  delta, phi, var, a, tau) < 0)
      k ++;

    B = A - k * tau;
  }


  fA = funX(A, delta, phi, var, a, tau);
  fB = funX(B, delta, phi, var, a, tau);

  while( ( (std::abs(B) - std::abs(A) ) > e) & (k < 20) ){
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

double updatePhi( double phi, double var, double sigma) {
  double prerating_phi2 = phi * phi + sigma * sigma;
  return 1.0 / sqrt( 1.0/prerating_phi2 + 1.0/var);
}

double dbl_calc_y(int rank_i, int rank_j){
  if(rank_i < rank_j ){
    return 1.0;
  } else if( rank_i==rank_j){
    return .5;
  } else {
    return 0.0;
  }
}


template <int rtype>
Rcpp::Matrix<rtype> subset_matrix(Rcpp::Matrix<rtype> input, Rcpp::NumericVector idx) {
  int n = idx.size();
  int k = input.ncol();
  Rcpp::Matrix<rtype> output(n, k);

  for (int i = 0; i < n; i++) {
    output(i,Rcpp::_) = input(idx(i),Rcpp::_);
  }

  return(output);
}

Rcpp::IntegerMatrix term_matrix_idx(Rcpp::StringMatrix term, Rcpp::StringVector param_names) {
  int n = term.nrow();
  int k = term.ncol();
  Rcpp::IntegerMatrix output(n, k);
  Rcpp::StringVector terms_i;

  for (int i = 0; i < n; i++) {
    terms_i = term(i, Rcpp::_);
    output(i,Rcpp::_) = Rcpp::match(terms_i, param_names) - 1;
  }

  return(output);
}

Rcpp::NumericMatrix term_matrix(Rcpp::IntegerMatrix Idx,
                                Rcpp::NumericVector r) {

  int n = Idx.nrow();
  int k = Idx.ncol();
  Rcpp::NumericMatrix term(n, k);

  for (int i = 0; i < n; i++) {
    for (int j = 0; j < k; j++) {
      term(i, j) = r(Idx(i, j));
    }
  }

  return(term);
}
