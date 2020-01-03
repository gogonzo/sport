double calc_s(int rank_i, int rank_j) {
  if(rank_i < rank_j ) {
    return 1.0;
  } else if (rank_i == rank_j) {
    return 0.5;
  } else {
    return 0.0;
  }
}