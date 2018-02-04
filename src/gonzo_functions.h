double calcLik( double P_iq,double rank_i, double rank_j){
  double Lik;
  if( rank_i < rank_j ) {
    Lik = ( P_iq );
  } else if( rank_i == rank_j ) { 
    Lik = 1- abs( .5 - P_iq )*2; 
  } else {
    Lik = ( 1 - P_iq ); 
  }
  
  return(Lik);
}