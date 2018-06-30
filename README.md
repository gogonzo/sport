`sport` an R package for bayesian dynamic rating system
================

[![Project Status](http://www.repostatus.org/badges/latest/active.svg)](http://www.repostatus.org/#active) [![License: MIT](https://img.shields.io/badge/License-MIT-yellow.svg)](https://opensource.org/licenses/MIT)

About
=====

Package contains functions calculating rating for two-player or multi-player matchups. Methods are based on Bayesian Approximation Method, and their idea can be summarized by:

![\\large R\_i^{'} \\leftarrow R\_i + K \* ( Y\_i - \\hat{Y\_i}  )](https://latex.codecogs.com/png.latex?%5Clarge%20R_i%5E%7B%27%7D%20%5Cleftarrow%20R_i%20%2B%20K%20%2A%20%28%20Y_i%20-%20%5Chat%7BY_i%7D%20%20%29 "\large R_i^{'} \leftarrow R_i + K * ( Y_i - \hat{Y_i}  )")

 Where:

![\\large \\hat{Y} = P(X\_i &gt; X\_j)](https://latex.codecogs.com/png.latex?%5Clarge%20%5Chat%7BY%7D%20%3D%20P%28X_i%20%3E%20X_j%29 "\large \hat{Y} = P(X_i > X_j)")

![K - learning rate](https://latex.codecogs.com/png.latex?K%20-%20learning%20rate "K - learning rate")

Installation
============

``` r
# devtools::install_github("gogonzo/sport")
library(sport)
```

Elo rating system
=================

`elo` function uses following formula.

![E\_a = \\frac{ 10^{ \\frac{r\_a}{400} } }{ 10^{ \\frac{r\_a}{400} } + 10^{ \\frac{r\_b}{400} } }](https://latex.codecogs.com/png.latex?E_a%20%3D%20%5Cfrac%7B%2010%5E%7B%20%5Cfrac%7Br_a%7D%7B400%7D%20%7D%20%7D%7B%2010%5E%7B%20%5Cfrac%7Br_a%7D%7B400%7D%20%7D%20%2B%2010%5E%7B%20%5Cfrac%7Br_b%7D%7B400%7D%20%7D%20%7D "E_a = \frac{ 10^{ \frac{r_a}{400} } }{ 10^{ \frac{r_a}{400} } + 10^{ \frac{r_b}{400} } }")

Glicko rating system
====================

Update Rules:

![\\hat{Y\_{ij}} = P(X\_i&gt;X\_j) = \\frac{1}{ 1 + 10^{-g(RD\_{ij}) \* (R\_i-R\_j)/400}}](https://latex.codecogs.com/png.latex?%5Chat%7BY_%7Bij%7D%7D%20%3D%20P%28X_i%3EX_j%29%20%3D%20%5Cfrac%7B1%7D%7B%201%20%2B%2010%5E%7B-g%28RD_%7Bij%7D%29%20%2A%20%28R_i-R_j%29%2F400%7D%7D "\hat{Y_{ij}} = P(X_i>X_j) = \frac{1}{ 1 + 10^{-g(RD_{ij}) * (R_i-R_j)/400}}")

![{R'}\_i = R\_i +  \\frac{1}{\\frac{1}{{RD}^2\_{i}} + \\frac{1}{d^2\_i}} \* \\sum\_j{g(RD\_j) \* (Y\_{ij} - \\hat{Y\_{ij}})  }](https://latex.codecogs.com/png.latex?%7BR%27%7D_i%20%3D%20R_i%20%2B%20%20%5Cfrac%7B1%7D%7B%5Cfrac%7B1%7D%7B%7BRD%7D%5E2_%7Bi%7D%7D%20%2B%20%5Cfrac%7B1%7D%7Bd%5E2_i%7D%7D%20%2A%20%5Csum_j%7Bg%28RD_j%29%20%2A%20%28Y_%7Bij%7D%20-%20%5Chat%7BY_%7Bij%7D%7D%29%20%20%7D "{R'}_i = R_i +  \frac{1}{\frac{1}{{RD}^2_{i}} + \frac{1}{d^2_i}} * \sum_j{g(RD_j) * (Y_{ij} - \hat{Y_{ij}})  }")

![{RD'}\_i = \\sqrt{(\\frac{1}{{RD}^2\_{i}} + \\frac{1}{d^2\_i}})^{-1}](https://latex.codecogs.com/png.latex?%7BRD%27%7D_i%20%3D%20%5Csqrt%7B%28%5Cfrac%7B1%7D%7B%7BRD%7D%5E2_%7Bi%7D%7D%20%2B%20%5Cfrac%7B1%7D%7Bd%5E2_i%7D%7D%29%5E%7B-1%7D "{RD'}_i = \sqrt{(\frac{1}{{RD}^2_{i}} + \frac{1}{d^2_i}})^{-1}")

``` r
list_glicko <- glicko_run( formula = rank|id ~ rider_name , data = gpheats)
```

Glicko2 rating system
=====================

![ \\hat{Y\_{ij}} = \\frac{1}{1 + e^{-g(\\phi\_{ij})\*(\\mu\_i  - \\mu\_j)} }](https://latex.codecogs.com/png.latex?%20%5Chat%7BY_%7Bij%7D%7D%20%3D%20%5Cfrac%7B1%7D%7B1%20%2B%20e%5E%7B-g%28%5Cphi_%7Bij%7D%29%2A%28%5Cmu_i%20%20-%20%5Cmu_j%29%7D%20%7D " \hat{Y_{ij}} = \frac{1}{1 + e^{-g(\phi_{ij})*(\mu_i  - \mu_j)} }")

![ {\\phi'}\_i = \\frac{1}{\\sqrt{ \\frac{1}{ { {\\phi\_i}^2 + {\\sigma'\_i}^2}} + \\frac{1}{v}  }}](https://latex.codecogs.com/png.latex?%20%7B%5Cphi%27%7D_i%20%3D%20%5Cfrac%7B1%7D%7B%5Csqrt%7B%20%5Cfrac%7B1%7D%7B%20%7B%20%7B%5Cphi_i%7D%5E2%20%2B%20%7B%5Csigma%27_i%7D%5E2%7D%7D%20%2B%20%5Cfrac%7B1%7D%7Bv%7D%20%20%7D%7D " {\phi'}_i = \frac{1}{\sqrt{ \frac{1}{ { {\phi_i}^2 + {\sigma'_i}^2}} + \frac{1}{v}  }}")

![ {\\mu'\_i} = \\mu\_i + {\\phi'}\_i \* \\sum\_j{g(\\phi\_j)\*(Y\_{ij} - \\hat{Y\_{ij}})} ](https://latex.codecogs.com/png.latex?%20%7B%5Cmu%27_i%7D%20%3D%20%5Cmu_i%20%2B%20%7B%5Cphi%27%7D_i%20%2A%20%5Csum_j%7Bg%28%5Cphi_j%29%2A%28Y_%7Bij%7D%20-%20%5Chat%7BY_%7Bij%7D%7D%29%7D%20 " {\mu'_i} = \mu_i + {\phi'}_i * \sum_j{g(\phi_j)*(Y_{ij} - \hat{Y_{ij}})} ")

``` r
list_glicko2 <- glicko2_run( formula = rank|id ~ rider_name , data = gpheats)
```

Dynamic Bradley Terry
=====================

Algorithm based on 'A Bayesian Approximation Method for Online Ranking' by Ruby C. Weng and Chih-Jen Lin

![\\hat{Y\_{ij}} = P(X\_i&gt;X\_j) = \\frac{e^{R\_i/c\_{i\_j}}}{e^{R\_i/c\_{ij}} + e^{R\_j/c\_{ij}}} ](https://latex.codecogs.com/png.latex?%5Chat%7BY_%7Bij%7D%7D%20%3D%20P%28X_i%3EX_j%29%20%3D%20%5Cfrac%7Be%5E%7BR_i%2Fc_%7Bi_j%7D%7D%7D%7Be%5E%7BR_i%2Fc_%7Bij%7D%7D%20%2B%20e%5E%7BR_j%2Fc_%7Bij%7D%7D%7D%20 "\hat{Y_{ij}} = P(X_i>X_j) = \frac{e^{R_i/c_{i_j}}}{e^{R_i/c_{ij}} + e^{R_j/c_{ij}}} ")

![{R'}\_i = R\_i + \\sum\_j{\\frac{RD\_i^2}{c\_{ij}}\*(Y\_{ij} - \\hat{Y\_{ij}})}](https://latex.codecogs.com/png.latex?%7BR%27%7D_i%20%3D%20R_i%20%2B%20%5Csum_j%7B%5Cfrac%7BRD_i%5E2%7D%7Bc_%7Bij%7D%7D%2A%28Y_%7Bij%7D%20-%20%5Chat%7BY_%7Bij%7D%7D%29%7D "{R'}_i = R_i + \sum_j{\frac{RD_i^2}{c_{ij}}*(Y_{ij} - \hat{Y_{ij}})}")

![{RD'}\_i = RD\_i \* \[ 1 - \\frac{RD\_{ij}^2}{RD\_i^2}\\sum\_j{ \\gamma\_j \* (\\frac{RD\_i}{c\_{ij}})^2\* \\hat{Y\_{ij}}\\hat{Y\_{ji}}   } \]](https://latex.codecogs.com/png.latex?%7BRD%27%7D_i%20%3D%20RD_i%20%2A%20%5B%201%20-%20%5Cfrac%7BRD_%7Bij%7D%5E2%7D%7BRD_i%5E2%7D%5Csum_j%7B%20%5Cgamma_j%20%2A%20%28%5Cfrac%7BRD_i%7D%7Bc_%7Bij%7D%7D%29%5E2%2A%20%5Chat%7BY_%7Bij%7D%7D%5Chat%7BY_%7Bji%7D%7D%20%20%20%7D%20%5D "{RD'}_i = RD_i * [ 1 - \frac{RD_{ij}^2}{RD_i^2}\sum_j{ \gamma_j * (\frac{RD_i}{c_{ij}})^2* \hat{Y_{ij}}\hat{Y_{ji}}   } ]")

``` r
list_bbt <- bbt_run( formula = rank|id~rider_name,  data = gpheats )
```

Dynamic Logistic Regression
===========================

![w\_t = {w\_{t-1}} + \\eta\_t](https://latex.codecogs.com/png.latex?w_t%20%3D%20%7Bw_%7Bt-1%7D%7D%20%2B%20%5Ceta_t "w_t = {w_{t-1}} + \eta_t")

![Y\_t = g(w\_t^Tx\_t)](https://latex.codecogs.com/png.latex?Y_t%20%3D%20g%28w_t%5ETx_t%29 "Y_t = g(w_t^Tx_t)")

![x\_t = x\_{it} - x\_{jt}](https://latex.codecogs.com/png.latex?x_t%20%3D%20x_%7Bit%7D%20-%20x_%7Bjt%7D "x_t = x_{it} - x_{jt}")

![w\_t = w\_{t-1} + {\\sum{\_t}} x\_t](https://latex.codecogs.com/png.latex?w_t%20%3D%20w_%7Bt-1%7D%20%2B%20%7B%5Csum%7B_t%7D%7D%20x_t "w_t = w_{t-1} + {\sum{_t}} x_t")

``` r
list_bdl <- bdl_run( formula = rank|id ~ rider_name, data = gpheats )
```

``` r
library(dplyr);library(magrittr)
riders <- unique(gpheats$rider_name)
stadiums <- unique(gpheats$place)
set.seed(1)
r <- 
  c(
    rep(0, length(riders)) %>% setNames(paste("rider_name:", riders)),
    seq(0.6,0.1, length.out = 6) %>% setNames(paste("field:",1:6))
  ) %>% as.matrix
rd <- 
  c(
    rep(1, length(riders)) %>% setNames(paste("rider_name:", riders)),
    seq(0.6,0.1, length.out = 6) %>% setNames(paste("field:",1:6))
  ) %>% as.matrix

list_bdl2 <- bdl_run(
    rank|id ~ rider_name + field,
    r = r,
    rd = rd, 
    data = gpheats
  )
```

Join ratings
============

``` r
ratings_glicko  <- list_glicko$r %>% rename(r_glicko = r, rd_glicko = rd, rider_name = names )
ratings_glicko2 <- list_glicko2$r %>% rename(r_glicko2 = r, rd_glicko2 = rd, rider_name = names )
ratings_bbt     <- list_bbt$r %>% rename(r_bbt = r, rd_bbt = rd, rider_name = names )
ratings_bdl     <- list_bdl$r %>% rename(r_bdl = r, rd_bdl = rd, rider_name = names )

gpheats %<>%
  mutate( id = as.character(id)) %>%
  left_join( ratings_glicko ) %>%
  left_join( ratings_glicko2 ) %>%
  left_join( ratings_bbt ) %>%
  left_join( ratings_bdl )
```
