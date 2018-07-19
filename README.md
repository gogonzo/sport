`sport` an R package for online update algorithms
================

[![Project Status](http://www.repostatus.org/badges/latest/active.svg)](http://www.repostatus.org/#active) [![License: MIT](https://img.shields.io/badge/License-MIT-yellow.svg)](https://opensource.org/licenses/MIT)

About
=====

Package contains functions calculating ratings for two-player or multi-player matchups. Methods included in package are able to estimate players ratings, estimates evolution in time and to predict output of any challange. Algorithms are based on Bayesian Approximation Method, and they don't involve any martix inversions nor likelihood estimation. Weights (parameters) are updated iteratively, so computation doesn't require any additional RAM to make estimation feasible. Additionaly, base of the package is writen in `C++` what makes `sport` computation even faster.

Theory
======

Problem of sport matchups falls into subject of paired comparison modeling and choice modeling. Estimating player skills is equivalent to estimating preferrence of choice between two alternatives. Just as one product is more preferred over another to buy, similarly better player is more preffered to win over worst.

![\\large R\_i^{'} \\leftarrow R\_i + K \* ( Y\_i - \\hat{Y\_i}  )](https://latex.codecogs.com/png.latex?%5Clarge%20R_i%5E%7B%27%7D%20%5Cleftarrow%20R_i%20%2B%20K%20%2A%20%28%20Y_i%20-%20%5Chat%7BY_i%7D%20%20%29 "\large R_i^{'} \leftarrow R_i + K * ( Y_i - \hat{Y_i}  )")

 Where:

![\\large \\hat{Y} = P(X\_i &gt; X\_j)](https://latex.codecogs.com/png.latex?%5Clarge%20%5Chat%7BY%7D%20%3D%20P%28X_i%20%3E%20X_j%29 "\large \hat{Y} = P(X_i > X_j)")

![K - learning rate](https://latex.codecogs.com/png.latex?K%20-%20learning%20rate "K - learning rate")

Probability function is based on [Bradley-Terry model](https://en.wikipedia.org/wiki/Bradley%E2%80%93Terry_model) designed to predict outcome of pairwise comparison. For multi-player matchups where output is a ranking, `sport` package uses the same data transformation as in [exploded logit](https://www.jstor.org/stable/270983) - ranking is then presented as combination all possible pairs competing within same event.

Glicko rating system
--------------------

Algorithms based on [Mark E. Glickman (1999)](http://www.glicko.net/research/glicko.pdf).

Update Rules:

![\\hat{Y\_{ij}} = P(X\_i&gt;X\_j) = \\frac{1}{ 1 + 10^{-g(RD\_{ij}) \* (R\_i-R\_j)/400}}](https://latex.codecogs.com/png.latex?%5Chat%7BY_%7Bij%7D%7D%20%3D%20P%28X_i%3EX_j%29%20%3D%20%5Cfrac%7B1%7D%7B%201%20%2B%2010%5E%7B-g%28RD_%7Bij%7D%29%20%2A%20%28R_i-R_j%29%2F400%7D%7D "\hat{Y_{ij}} = P(X_i>X_j) = \frac{1}{ 1 + 10^{-g(RD_{ij}) * (R_i-R_j)/400}}")

![{R'}\_i = R\_i +  \\frac{1}{\\frac{1}{{RD}^2\_{i}} + \\frac{1}{d^2\_i}} \* \\sum\_j{g(RD\_j) \* (Y\_{ij} - \\hat{Y\_{ij}})  }](https://latex.codecogs.com/png.latex?%7BR%27%7D_i%20%3D%20R_i%20%2B%20%20%5Cfrac%7B1%7D%7B%5Cfrac%7B1%7D%7B%7BRD%7D%5E2_%7Bi%7D%7D%20%2B%20%5Cfrac%7B1%7D%7Bd%5E2_i%7D%7D%20%2A%20%5Csum_j%7Bg%28RD_j%29%20%2A%20%28Y_%7Bij%7D%20-%20%5Chat%7BY_%7Bij%7D%7D%29%20%20%7D "{R'}_i = R_i +  \frac{1}{\frac{1}{{RD}^2_{i}} + \frac{1}{d^2_i}} * \sum_j{g(RD_j) * (Y_{ij} - \hat{Y_{ij}})  }")

![{RD'}\_i = \\sqrt{(\\frac{1}{{RD}^2\_{i}} + \\frac{1}{d^2\_i}})^{-1}](https://latex.codecogs.com/png.latex?%7BRD%27%7D_i%20%3D%20%5Csqrt%7B%28%5Cfrac%7B1%7D%7B%7BRD%7D%5E2_%7Bi%7D%7D%20%2B%20%5Cfrac%7B1%7D%7Bd%5E2_i%7D%7D%29%5E%7B-1%7D "{RD'}_i = \sqrt{(\frac{1}{{RD}^2_{i}} + \frac{1}{d^2_i}})^{-1}")

Glicko2 rating system
---------------------

Algorithm according to [Mark E. Glickman (2013)](http://www.glicko.net/glicko/glicko2.pdf)

![ \\hat{Y\_{ij}} = \\frac{1}{1 + e^{-g(\\phi\_{ij})\*(\\mu\_i  - \\mu\_j)} }](https://latex.codecogs.com/png.latex?%20%5Chat%7BY_%7Bij%7D%7D%20%3D%20%5Cfrac%7B1%7D%7B1%20%2B%20e%5E%7B-g%28%5Cphi_%7Bij%7D%29%2A%28%5Cmu_i%20%20-%20%5Cmu_j%29%7D%20%7D " \hat{Y_{ij}} = \frac{1}{1 + e^{-g(\phi_{ij})*(\mu_i  - \mu_j)} }")

![ {\\phi'}\_i = \\frac{1}{\\sqrt{ \\frac{1}{ { {\\phi\_i}^2 + {\\sigma'\_i}^2}} + \\frac{1}{v}  }}](https://latex.codecogs.com/png.latex?%20%7B%5Cphi%27%7D_i%20%3D%20%5Cfrac%7B1%7D%7B%5Csqrt%7B%20%5Cfrac%7B1%7D%7B%20%7B%20%7B%5Cphi_i%7D%5E2%20%2B%20%7B%5Csigma%27_i%7D%5E2%7D%7D%20%2B%20%5Cfrac%7B1%7D%7Bv%7D%20%20%7D%7D " {\phi'}_i = \frac{1}{\sqrt{ \frac{1}{ { {\phi_i}^2 + {\sigma'_i}^2}} + \frac{1}{v}  }}")

![ {\\mu'\_i} = \\mu\_i + {\\phi'}\_i \* \\sum\_j{g(\\phi\_j)\*(Y\_{ij} - \\hat{Y\_{ij}})} ](https://latex.codecogs.com/png.latex?%20%7B%5Cmu%27_i%7D%20%3D%20%5Cmu_i%20%2B%20%7B%5Cphi%27%7D_i%20%2A%20%5Csum_j%7Bg%28%5Cphi_j%29%2A%28Y_%7Bij%7D%20-%20%5Chat%7BY_%7Bij%7D%7D%29%7D%20 " {\mu'_i} = \mu_i + {\phi'}_i * \sum_j{g(\phi_j)*(Y_{ij} - \hat{Y_{ij}})} ")

Bayesian Bradley Terry
----------------------

Based on [Ruby C. Weng and Chih-Jen Lin (2011)](http://jmlr.csail.mit.edu/papers/volume12/weng11a/weng11a.pdf)

![\\hat{Y\_{ij}} = P(X\_i&gt;X\_j) = \\frac{e^{R\_i/c\_{i\_j}}}{e^{R\_i/c\_{ij}} + e^{R\_j/c\_{ij}}} ](https://latex.codecogs.com/png.latex?%5Chat%7BY_%7Bij%7D%7D%20%3D%20P%28X_i%3EX_j%29%20%3D%20%5Cfrac%7Be%5E%7BR_i%2Fc_%7Bi_j%7D%7D%7D%7Be%5E%7BR_i%2Fc_%7Bij%7D%7D%20%2B%20e%5E%7BR_j%2Fc_%7Bij%7D%7D%7D%20 "\hat{Y_{ij}} = P(X_i>X_j) = \frac{e^{R_i/c_{i_j}}}{e^{R_i/c_{ij}} + e^{R_j/c_{ij}}} ")

![{R'}\_i = R\_i + \\sum\_j{\\frac{RD\_i^2}{c\_{ij}}\*(Y\_{ij} - \\hat{Y\_{ij}})}](https://latex.codecogs.com/png.latex?%7BR%27%7D_i%20%3D%20R_i%20%2B%20%5Csum_j%7B%5Cfrac%7BRD_i%5E2%7D%7Bc_%7Bij%7D%7D%2A%28Y_%7Bij%7D%20-%20%5Chat%7BY_%7Bij%7D%7D%29%7D "{R'}_i = R_i + \sum_j{\frac{RD_i^2}{c_{ij}}*(Y_{ij} - \hat{Y_{ij}})}")

![{RD'}\_i = RD\_i \* \[ 1 - \\frac{RD\_{ij}^2}{RD\_i^2}\\sum\_j{ \\gamma\_j \* (\\frac{RD\_i}{c\_{ij}})^2\* \\hat{Y\_{ij}}\\hat{Y\_{ji}}   } \]](https://latex.codecogs.com/png.latex?%7BRD%27%7D_i%20%3D%20RD_i%20%2A%20%5B%201%20-%20%5Cfrac%7BRD_%7Bij%7D%5E2%7D%7BRD_i%5E2%7D%5Csum_j%7B%20%5Cgamma_j%20%2A%20%28%5Cfrac%7BRD_i%7D%7Bc_%7Bij%7D%7D%29%5E2%2A%20%5Chat%7BY_%7Bij%7D%7D%5Chat%7BY_%7Bji%7D%7D%20%20%20%7D%20%5D "{RD'}_i = RD_i * [ 1 - \frac{RD_{ij}^2}{RD_i^2}\sum_j{ \gamma_j * (\frac{RD_i}{c_{ij}})^2* \hat{Y_{ij}}\hat{Y_{ji}}   } ]")

Dynamic Logistic Regression
---------------------------

This algorithm differs from above in not basing on Bradley Terry model. Dynamic Logistic Regression weights are updated using extended Kalman Filter, which means that it's possible to estimate multiple parameters per individual.

*William D. Penny and Stephen J. Roberts (1999): Dynamic Logistic Regression, Departament of Electrical and Electronic Engineering, Imperial College*

![w\_t = {w\_{t-1}} + \\eta\_t](https://latex.codecogs.com/png.latex?w_t%20%3D%20%7Bw_%7Bt-1%7D%7D%20%2B%20%5Ceta_t "w_t = {w_{t-1}} + \eta_t")

![Y\_t = g(w\_t^Tx\_t)](https://latex.codecogs.com/png.latex?Y_t%20%3D%20g%28w_t%5ETx_t%29 "Y_t = g(w_t^Tx_t)")

![x\_t = x\_{it} - x\_{jt}](https://latex.codecogs.com/png.latex?x_t%20%3D%20x_%7Bit%7D%20-%20x_%7Bjt%7D "x_t = x_{it} - x_{jt}")

![w\_t = w\_{t-1} + {\\sum{\_t}} x\_t](https://latex.codecogs.com/png.latex?w_t%20%3D%20w_%7Bt-1%7D%20%2B%20%7B%5Csum%7B_t%7D%7D%20x_t "w_t = w_{t-1} + {\sum{_t}} x_t")

Package Usage
=============

Installation
------------

Install package from github.

``` r
# devtools::install_github("gogonzo/sport")
library(sport)
```

Available Data
--------------

Package contains data from Speedway Grand-Prix. There are two data.frames: 1. `gpheats` - results of each race in all SGP events. Column `rank` is a numeric version of column `position` - rider position in race. 2. `gpsquads` - summarized results of the events, with sum of point and final position.

``` r
str(gpheats)
```

    ## 'data.frame':    20374 obs. of  11 variables:
    ##  $ id      : num  1 1 1 1 2 2 2 2 3 3 ...
    ##  $ season  : int  1995 1995 1995 1995 1995 1995 1995 1995 1995 1995 ...
    ##  $ date    : POSIXct, format: "1995-05-20 19:00:00" "1995-05-20 19:00:00" ...
    ##  $ round   : int  1 1 1 1 1 1 1 1 1 1 ...
    ##  $ name    : chr  "Speedway Grand Prix of Poland" "Speedway Grand Prix of Poland" "Speedway Grand Prix of Poland" "Speedway Grand Prix of Poland" ...
    ##  $ heat    : int  1 1 1 1 2 2 2 2 3 3 ...
    ##  $ field   : int  1 2 3 4 1 2 3 4 1 2 ...
    ##  $ rider   : chr  "Tomasz Gollob" "Gary Havelock" "Chris Louis" "Tony Rickardsson" ...
    ##  $ points  : int  2 0 3 1 3 0 1 2 0 2 ...
    ##  $ position: chr  "2" "4" "1" "3" ...
    ##  $ rank    : num  2 4 1 3 1 4 3 2 4 2 ...

Estimate dynamic ratings
------------------------

To compute ratings using each algorithms one has to specify formula. For example `formula = rank|id ~ rider` estimates `rider` abilities, with observed outputs `rank` nested within particular event/experiment `id`. This formula can be

Glicko uses only one parameter per `rider` to describe his overall abilities. Ouput is a ranking within specified event (`rank|id`). One can also specify initial parameters based on prior knowledge. `glicko` estimates `r` and `rd` but glicko2 has additional `sig` parameter, measuring volitality. If not specified, by default `r=1500`, `rd=300`, `sig=0.05`.

``` r
# initial estimates default
list_glicko  <- glicko_run(  formula = rank|id ~ rider , data = gpheats )
list_glicko2 <- glicko2_run( formula = rank|id ~ rider , data = gpheats )
```

``` r
library(magrittr)
unique_riders <- unique(gpheats$rider)
r   <- rep(1500, length(unique_riders) ) %>% setNames(unique_riders)
rd  <- rep(1500, length(unique_riders) ) %>% setNames(unique_riders)
sig <- rep(0.05, length(unique_riders) ) %>% setNames(unique_riders)

list_glicko  <- glicko_run( 
  formula = rank|id ~ rider , 
  r = r, rd = rd, 
  data = gpheats)

list_glicko2 <- glicko2_run( 
  formula = rank|id ~ rider , 
  r = r, rd = rd, sig = sig ,
  data = gpheats)
```

``` r
list_bbt <- bbt_run( formula = rank|id~rider,  data = gpheats )
```

``` r
list_dlr1 <- dlr_run( formula = rank|id ~ rider, data = gpheats )
```

``` r
library(tidyverse);library(magrittr)
riders <- unique(gpheats$rider)
stadiums <- unique(gpheats$place)
set.seed(1)
r <- 
  c(
    rep(0, length(riders)) %>% setNames(paste("rider:", riders)),
    seq(0.6,0.1, length.out = 6) %>% setNames(paste("field:",1:6))
  ) %>% as.matrix
rd <- 
  c(
    rep(1, length(riders)) %>% setNames(paste("rider:", riders)),
    seq(0.6,0.1, length.out = 6) %>% setNames(paste("field:",1:6))
  ) %>% as.matrix

list_dlr <- dlr_run(
    rank|id ~ rider + field,
    r = r,
    rd = rd, 
    data = gpheats
  )
```

Join ratings
------------

``` r
ratings_glicko  <- list_glicko$r %>% rename(r_glicko = r, rd_glicko = rd, rider = names )
ratings_glicko2 <- list_glicko2$r %>% rename(r_glicko2 = r, rd_glicko2 = rd, rider = names )
ratings_bbt     <- list_bbt$r %>% rename(r_bbt = r, rd_bbt = rd, rider = names )
ratings_dlr     <- list_dlr$r %>% rename(r_dlr = r, rd_dlr = rd, rider = names )

gpheats %<>%
  mutate( id = as.character(id)) %>%
  left_join( ratings_glicko ) %>%
  left_join( ratings_glicko2 ) %>%
  left_join( ratings_bbt ) %>%
  left_join( ratings_dlr )
```

Join pairs
----------

``` r
pairs_glicko  <- list_glicko[[2]]  %>% rename(P_glicko = P)
pairs_glicko2 <- list_glicko2[[2]] %>% rename(P_glicko2 = P)
pairs_bbt     <- list_bbt[[2]]     %>% rename(P_bbt = P)
pairs_dlr1    <- list_dlr1[[1]]    %>% rename(P_dlr1 = P)
pairs_dlr     <- list_dlr[[1]]     %>% rename(P_dlr = P)

pairs <-
  pairs_glicko %>%
  left_join(pairs_glicko2) %>%
  left_join(pairs_bbt) %>%
  left_join(pairs_dlr1) %>%
  left_join(pairs_dlr) %>%
  rename(rider = team1, opponent = team2) %>%
  filter(Y!=0.5) %>%
  arrange(id, sample(1:n()))


# leave only unique pairs
pairs$uniq_pair <-
  pairs %>% 
  select(rider,opponent) %>% 
  apply(1,function(x)paste(sort(x), collapse=" - ")) 

pairs %<>% filter(!duplicated(paste(id, uniq_pair)))
```
