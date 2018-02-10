library(data.table)
library(psych)
library(sport)
df <- data.table(
  team = c( "A", "B", "C", "D" ),
  rank  = c( 3, 4, 1, 2 ), 
  days  = c( 0, 0, 0, 0),
  r     = c( 1500, 1400, 1550, 1700 ) , 
  rd    = c( 200,  30,   100,  300 ),
  sig   = c( .06, .06, .05, .07),
  tau   = .5
)

mi_ij <- matrix(c(1500, 1400, 1550, 1700),2,2)
sig_ij <- matrix(c( 200,  30,   100,  300 ),2,2)
sig2_ij <- sig_ij^2
rank_i <- c(1,2)


mi_ij <- matrix(c(1500, 1400, 1550, 2000),2)
sig_ij <- matrix(c( 200,  30,   100,  200 ),2)
rank_i <-  c( 1,2)
kappa <- 0.95

bbt(
  rank = rank_i, 
  sig_ij = sig_ij/60, 
  mi_ij = mi_ij/60
)

gonzo(
  rank  = c( 3, 4, 1, 2 ), 
  mi_i     = c( 1500, 1400, 1550, 1700 ) , 
  sig_i    = c( 200,  30,   100,  300 )
)

glicko(
  teams = c( "A", "B", "C", "D" ), 
  rank  = c( 3, 4, 1, 2 ), 
  days  = c( 0, 0, 0, 0),
  r     = c( 1500, 1400, 1550, 1700 ) , 
  rd    = c( 200,  30,   100,  300 ),
  init_r  = 1500,
  init_rd = 100
)

glicko2(
  teams = c( "A", "B", "C", "D" ), 
  rank  = c( 3, 4, 1, 2 ), 
  days  = c( 0, 0, 0, 0),
  r     = c( 1500, 1400, 1550, 1700 ) , 
  rd    = c( 200,  30,   100,  300 ),
  sig   = c( .06, .06, .05, .07),
  tau   = .5,
  init_r  = 1500,
  init_rd = 100
)
