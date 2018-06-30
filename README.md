<!-- rmarkdown v1 -->
---
title: "Untitled"
output: github_document
  pandoc_args: --webtex
---

<head>
 <script type="text/javascript" src="http://cdn.mathjax.org/mathjax/latest/MathJax.js?config=default"></script>
</head>

# `sport` R package for bayesian dynamic rating system.
[![Project Status](http://www.repostatus.org/badges/latest/active.svg)](http://www.repostatus.org/#active)
[![MIT License](https://badges.frapsoft.com/os/mit/mit.svg)](https://opensource.org/licenses/mit-license.php)
# About
Package contains functions calculating rating for two-player or multi-player matchups. Methods are based on Bayesian Approximation Method, and their idea can be summarized by:

$$\large R_i^{'} \leftarrow R_i + K * ( Y_i - \hat{Y_i}  )$$
Where: 
    $$\large \hat{Y} = P(X_i > X_j)$$ 
    $$K - learning rate$$

# Installation

```r
# devtools::install_github("gogonzo/sport")
library(sport)
```


# Elo rating system
`elo` function uses following formula.

$E_a = \frac{ 10^{ \frac{r_a}{400} } }{ 10^{ \frac{r_a}{400} } + 10^{ \frac{r_b}{400} } }$

# Glicko rating system
Update Rules:

$$\hat{Y_{ij}} = P(X_i>X_j) = \frac{1}{ 1 + 10^{-g(RD_{ij}) * (R_i-R_j)/400}}$$

$${R'}_i = R_i +  \frac{1}{\frac{1}{{RD}^2_{i}} + \frac{1}{d^2_i}} * \sum_j{g(RD_j) * (Y_{ij} - \hat{Y_{ij}})  }$$

$${RD'}_i = \sqrt{(\frac{1}{{RD}^2_{i}} + \frac{1}{d^2_i}})^{-1}$$



```r
list_glicko <- glicko_run( formula = rank|id ~ rider_name , data = gpheats)
```

# Glicko2 rating system

$$ \hat{Y_{ij}} = \frac{1}{1 + e^{-g(\phi_{ij})*(\mu_i  - \mu_j)} }$$

$$ {\phi'}_i = \frac{1}{\sqrt{ \frac{1}{ { {\phi_i}^2 + {\sigma'_i}^2}} + \frac{1}{v}  }}$$

$$ {\mu'_i} = \mu_i + {\phi'}_i * \sum_j{g(\phi_j)*(Y_{ij} - \hat{Y_{ij}})} $$


```r
list_glicko2 <- glicko2_run( formula = rank|id ~ rider_name , data = gpheats)
```


# Dynamic Bradley Terry
Algorithm based on 'A Bayesian Approximation Method for Online Ranking' by Ruby C. Weng and Chih-Jen Lin

$$\hat{Y_{ij}} = P(X_i>X_j) = \frac{e^{R_i/c_{i_j}}}{e^{R_i/c_{ij}} + e^{R_j/c_{ij}}} $$

$${R'}_i = R_i + \sum_j{\frac{RD_i^2}{c_{ij}}*(Y_{ij} - \hat{Y_{ij}})}$$

$${RD'}_i = RD_i * [ 1 - \frac{RD_{ij}^2}{RD_i^2}\sum_j{ \gamma_j * (\frac{RD_i}{c_{ij}})^2* \hat{Y_{ij}}\hat{Y_{ji}}   } ]$$


```r
list_bbt <- bbt_run( formula = rank|id~rider_name,  data = gpheats )
```


# Dynamic Logistic Regression

$$w_t = {w_{t-1}} + \eta_t$$

$$Y_t = g(w_t^Tx_t)$$

$$x_t = x_{it} - x_{jt}$$

$$w_t = w_{t-1} + {\sum{_t}} x_t$$



```r
list_bdl <- bdl_run( formula = rank|id ~ rider_name, data = gpheats )
```



```r
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


# Join ratings

```r
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


