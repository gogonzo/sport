double calc_y(int rank_i, int rank_j){
  if(rank_i < rank_j ){
    return 1.0;
  } else if( rank_i==rank_j){
    return 0.5;
  } else {
    return 0.0;
  }
}


arma::mat x_vec2mat( arma::mat x, int k ){
  arma::mat x_new(2,k);
  arma::uvec t1_idx;
  arma::uvec t2_idx;
  
  
  
  
}