`sport` an R package for online update algorithms
================

[![Travis-CI Build Status](https://travis-ci.org/gogonzo/sport.svg?branch=master)](https://travis-ci.org/gogonzo/sport) [![Project Status](http://www.repostatus.org/badges/latest/active.svg)](http://www.repostatus.org/#active) [![License: MIT](https://img.shields.io/badge/License-MIT-yellow.svg)](https://opensource.org/licenses/MIT) [![Coverage status](https://codecov.io/gh/gogonzo/sport/branch/master/graph/badge.svg)](https://codecov.io/github/gogonzo/sport?branch=master)

About
=====

Package contains functions calculating ratings for two-player or multi-player matchups. Methods included in package are able to estimate ratings (players strengths) and their evolution in time, also able to predict output of challange. Algorithms are based on Bayesian Approximation Method, and they don't involve any martix inversions nor likelihood estimation. Weights (parameters) are updated sequentionaly, and computation doesn't require any additional RAM to make estimation feasible. Additionaly, base of the package is writen in `C++` what makes `sport` computation even faster.

Theory
======

Problem of sport matchups falls into subject of paired comparison modeling and choice modeling. Estimating player skills is equivalent to estimating preferrence of choice between two alternatives. Just as one product is more preferred over another to buy, similarly better player is more preffered to win over worst. As player/event and alternative/experiment can be used interchangeably, for ease of use sport nomenclature is adapted (player/event).

Algorithms implemented in a `sport` package works similarly, as all using Bayesian Approximation Method. Algorithms works as follows: At the moment player `i` competes with player `j` while both have initial ![R\_i](https://latex.codecogs.com/png.latex?R_i "R_i") and ![R\_j](https://latex.codecogs.com/png.latex?R_j "R_j") ratings. Prior to event, probability that player `i` win over player `j` is ![\\hat{Y\_i}](https://latex.codecogs.com/png.latex?%5Chat%7BY_i%7D "\hat{Y_i}"). After event is finished when true result ![Y\_{ij}](https://latex.codecogs.com/png.latex?Y_%7Bij%7D "Y_{ij}") is observed, initial believe about rating is changed ![R\_i^{'} \\leftarrow R\_i](https://latex.codecogs.com/png.latex?R_i%5E%7B%27%7D%20%5Cleftarrow%20R_i "R_i^{'} \leftarrow R_i") according to the prediction error ![( Y\_{ij} - \\hat{Y\_{ij}} )](https://latex.codecogs.com/png.latex?%28%20Y_%7Bij%7D%20-%20%5Chat%7BY_%7Bij%7D%7D%20%29 "( Y_{ij} - \hat{Y_{ij}} )") and some constant ![K](https://latex.codecogs.com/png.latex?K "K"). Updates are summed as player can compete with more than one player in particular event.

![\\large R\_i^{'} \\leftarrow R\_i + \\sum\_{j \\neq i}{ K \* ( Y\_{ij} - \\hat{Y\_{ij}}}  )](https://latex.codecogs.com/png.latex?%5Clarge%20R_i%5E%7B%27%7D%20%5Cleftarrow%20R_i%20%2B%20%5Csum_%7Bj%20%5Cneq%20i%7D%7B%20K%20%2A%20%28%20Y_%7Bij%7D%20-%20%5Chat%7BY_%7Bij%7D%7D%7D%20%20%29 "\large R_i^{'} \leftarrow R_i + \sum_{j \neq i}{ K * ( Y_{ij} - \hat{Y_{ij}}}  )")

 Where:

![\\large \\hat{Y} = P(X\_i &gt; X\_j)](https://latex.codecogs.com/png.latex?%5Clarge%20%5Chat%7BY%7D%20%3D%20P%28X_i%20%3E%20X_j%29 "\large \hat{Y} = P(X_i > X_j)")

![K - learning rate](https://latex.codecogs.com/png.latex?K%20-%20learning%20rate "K - learning rate")

Outcome probability function is based on [Bradley-Terry model](https://en.wikipedia.org/wiki/Bradley%E2%80%93Terry_model) designed to predict outcome of pairwise comparison. For multi-player matchups where output is a ranking, `sport` package uses the same data transformation as in [exploded logit](https://www.jstor.org/stable/270983) - ranking is then presented as combination of all possible pairs competing within same event.

Glicko rating system
--------------------

Glicko is the first bayesian online update algorithm incorporating rating volatility to rating and outcome computation. Glicko system is not balanced, and sum of rating rewards of all players are not zero. In one 2-players event, reward of player `i` differs from reward of player `q` as it depends on their individual ratings deviation. Rating values oscillates around `r=1500` with max deviation `rd<=350`.

For deeper knowledge read [Mark E. Glickman (1999)](http://www.glicko.net/research/glicko.pdf).

Update Rules:

![\\hat{Y\_{ij}} = P(X\_i&gt;X\_j) = \\frac{1}{ 1 + 10^{-g(RD\_{ij}) \* (R\_i-R\_j)/400}}](https://latex.codecogs.com/png.latex?%5Chat%7BY_%7Bij%7D%7D%20%3D%20P%28X_i%3EX_j%29%20%3D%20%5Cfrac%7B1%7D%7B%201%20%2B%2010%5E%7B-g%28RD_%7Bij%7D%29%20%2A%20%28R_i-R_j%29%2F400%7D%7D "\hat{Y_{ij}} = P(X_i>X_j) = \frac{1}{ 1 + 10^{-g(RD_{ij}) * (R_i-R_j)/400}}")

![{R'}\_i = R\_i +  \\frac{1}{\\frac{1}{{RD}^2\_{i}} + \\frac{1}{d^2\_i}} \* \\sum\_j{g(RD\_j) \* (Y\_{ij} - \\hat{Y\_{ij}})  }](https://latex.codecogs.com/png.latex?%7BR%27%7D_i%20%3D%20R_i%20%2B%20%20%5Cfrac%7B1%7D%7B%5Cfrac%7B1%7D%7B%7BRD%7D%5E2_%7Bi%7D%7D%20%2B%20%5Cfrac%7B1%7D%7Bd%5E2_i%7D%7D%20%2A%20%5Csum_j%7Bg%28RD_j%29%20%2A%20%28Y_%7Bij%7D%20-%20%5Chat%7BY_%7Bij%7D%7D%29%20%20%7D "{R'}_i = R_i +  \frac{1}{\frac{1}{{RD}^2_{i}} + \frac{1}{d^2_i}} * \sum_j{g(RD_j) * (Y_{ij} - \hat{Y_{ij}})  }")

![{RD'}\_i = \\sqrt{(\\frac{1}{{RD}^2\_{i}} + \\frac{1}{d^2\_i}})^{-1}](https://latex.codecogs.com/png.latex?%7BRD%27%7D_i%20%3D%20%5Csqrt%7B%28%5Cfrac%7B1%7D%7B%7BRD%7D%5E2_%7Bi%7D%7D%20%2B%20%5Cfrac%7B1%7D%7Bd%5E2_i%7D%7D%29%5E%7B-1%7D "{RD'}_i = \sqrt{(\frac{1}{{RD}^2_{i}} + \frac{1}{d^2_i}})^{-1}")

Glicko2 rating system
---------------------

Glicko2 improved predecessor by adding volatile parameter ![\\sigma\_i](https://latex.codecogs.com/png.latex?%5Csigma_i "\sigma_i") which increase/decrease rating deviation in periods when player performance differs from expected. Sigma is estimated iteratively using Illinois algorithm, which converges quickly not affecting computation time. Rating values oscillates around `r=1500` with max deviation `rd<=350`.

For further knowledge read [Mark E. Glickman (2013)](http://www.glicko.net/glicko/glicko2.pdf)

![ \\hat{Y\_{ij}} = \\frac{1}{1 + e^{-g(\\phi\_{ij})\*(\\mu\_i  - \\mu\_j)} }](https://latex.codecogs.com/png.latex?%20%5Chat%7BY_%7Bij%7D%7D%20%3D%20%5Cfrac%7B1%7D%7B1%20%2B%20e%5E%7B-g%28%5Cphi_%7Bij%7D%29%2A%28%5Cmu_i%20%20-%20%5Cmu_j%29%7D%20%7D " \hat{Y_{ij}} = \frac{1}{1 + e^{-g(\phi_{ij})*(\mu_i  - \mu_j)} }")

![ {\\phi'}\_i = \\frac{1}{\\sqrt{ \\frac{1}{ { {\\phi\_i}^2 + {\\sigma'\_i}^2}} + \\frac{1}{v}  }}](https://latex.codecogs.com/png.latex?%20%7B%5Cphi%27%7D_i%20%3D%20%5Cfrac%7B1%7D%7B%5Csqrt%7B%20%5Cfrac%7B1%7D%7B%20%7B%20%7B%5Cphi_i%7D%5E2%20%2B%20%7B%5Csigma%27_i%7D%5E2%7D%7D%20%2B%20%5Cfrac%7B1%7D%7Bv%7D%20%20%7D%7D " {\phi'}_i = \frac{1}{\sqrt{ \frac{1}{ { {\phi_i}^2 + {\sigma'_i}^2}} + \frac{1}{v}  }}")

![ {\\mu'\_i} = \\mu\_i + {\\phi'}\_i \* \\sum\_j{g(\\phi\_j)\*(Y\_{ij} - \\hat{Y\_{ij}})} ](https://latex.codecogs.com/png.latex?%20%7B%5Cmu%27_i%7D%20%3D%20%5Cmu_i%20%2B%20%7B%5Cphi%27%7D_i%20%2A%20%5Csum_j%7Bg%28%5Cphi_j%29%2A%28Y_%7Bij%7D%20-%20%5Chat%7BY_%7Bij%7D%7D%29%7D%20 " {\mu'_i} = \mu_i + {\phi'}_i * \sum_j{g(\phi_j)*(Y_{ij} - \hat{Y_{ij}})} ")

Bayesian Bradley Terry
----------------------

The fastest algorithm with simple formula. Original BT formula lacks variance parameter, and this method incorporates rating deviation into model. BBT also prevents against fast `rd` decline to zero using `gamma` and `kappa`.

For further knowledge read [Ruby C. Weng and Chih-Jen Lin (2011)](http://jmlr.csail.mit.edu/papers/volume12/weng11a/weng11a.pdf)

![\\hat{Y\_{ij}} = P(X\_i&gt;X\_j) = \\frac{e^{R\_i/c\_{i\_j}}}{e^{R\_i/c\_{ij}} + e^{R\_j/c\_{ij}}} ](https://latex.codecogs.com/png.latex?%5Chat%7BY_%7Bij%7D%7D%20%3D%20P%28X_i%3EX_j%29%20%3D%20%5Cfrac%7Be%5E%7BR_i%2Fc_%7Bi_j%7D%7D%7D%7Be%5E%7BR_i%2Fc_%7Bij%7D%7D%20%2B%20e%5E%7BR_j%2Fc_%7Bij%7D%7D%7D%20 "\hat{Y_{ij}} = P(X_i>X_j) = \frac{e^{R_i/c_{i_j}}}{e^{R_i/c_{ij}} + e^{R_j/c_{ij}}} ")

![{R'}\_i = R\_i + \\sum\_j{\\frac{RD\_i^2}{c\_{ij}}\*(Y\_{ij} - \\hat{Y\_{ij}})}](https://latex.codecogs.com/png.latex?%7BR%27%7D_i%20%3D%20R_i%20%2B%20%5Csum_j%7B%5Cfrac%7BRD_i%5E2%7D%7Bc_%7Bij%7D%7D%2A%28Y_%7Bij%7D%20-%20%5Chat%7BY_%7Bij%7D%7D%29%7D "{R'}_i = R_i + \sum_j{\frac{RD_i^2}{c_{ij}}*(Y_{ij} - \hat{Y_{ij}})}")

![{RD'}\_i = RD\_i \* \[ 1 - \\frac{RD\_{ij}^2}{RD\_i^2}\\sum\_j{ \\gamma\_j \* (\\frac{RD\_i}{c\_{ij}})^2\* \\hat{Y\_{ij}}\\hat{Y\_{ji}}   } \]](https://latex.codecogs.com/png.latex?%7BRD%27%7D_i%20%3D%20RD_i%20%2A%20%5B%201%20-%20%5Cfrac%7BRD_%7Bij%7D%5E2%7D%7BRD_i%5E2%7D%5Csum_j%7B%20%5Cgamma_j%20%2A%20%28%5Cfrac%7BRD_i%7D%7Bc_%7Bij%7D%7D%29%5E2%2A%20%5Chat%7BY_%7Bij%7D%7D%5Chat%7BY_%7Bji%7D%7D%20%20%20%7D%20%5D "{RD'}_i = RD_i * [ 1 - \frac{RD_{ij}^2}{RD_i^2}\sum_j{ \gamma_j * (\frac{RD_i}{c_{ij}})^2* \hat{Y_{ij}}\hat{Y_{ji}}   } ]")

Dynamic Bayesian Logit
----------------------

Following algorithm gives some advantages over mentioned rating systems, adding other important factors to estimation process making final ratings unbiased. Algorithm perform better in disciples where other variables can make a difference in result eg. home field advantage. DBL implements Extended Kalman Filter learning rule, and allows to estimate multiple parameters in addition to player ratings. DBL is a Dynamic Logit extended to usage in pairwise comparisons by modeling differences in players characteristics. Classic Bradley-Terry model is enriched by moderation element ![K(s\_t)](https://latex.codecogs.com/png.latex?K%28s_t%29 "K(s_t)") which adds prior uncertainty to output prediction.

![\\hat{Y\_{ij}} = \\frac{ e^{-K(s\_t)w \_t^T(x\_{it}-x\_{jt})} }{1+e^{-K(s\_t)w \_t^T(x\_{it}-x\_{jt})}}](https://latex.codecogs.com/png.latex?%5Chat%7BY_%7Bij%7D%7D%20%3D%20%5Cfrac%7B%20e%5E%7B-K%28s_t%29w%20_t%5ET%28x_%7Bit%7D-x_%7Bjt%7D%29%7D%20%7D%7B1%2Be%5E%7B-K%28s_t%29w%20_t%5ET%28x_%7Bit%7D-x_%7Bjt%7D%29%7D%7D "\hat{Y_{ij}} = \frac{ e^{-K(s_t)w _t^T(x_{it}-x_{jt})} }{1+e^{-K(s_t)w _t^T(x_{it}-x_{jt})}}")

 Parameters for player `i` competing with player `j` are estimated using EKF update rule.

![\\hat{\\omega}\_{it} = \\hat{\\omega}\_{i(t-1)} + \\frac{RD^2\_{i(t-1)}}{1+\\hat{Y\_{ij}}  (1-\\hat{Y\_{ij}})}  x\_t (Y\_{ij} - \\hat{Y\_{ij}})](https://latex.codecogs.com/png.latex?%5Chat%7B%5Comega%7D_%7Bit%7D%20%3D%20%5Chat%7B%5Comega%7D_%7Bi%28t-1%29%7D%20%2B%20%5Cfrac%7BRD%5E2_%7Bi%28t-1%29%7D%7D%7B1%2B%5Chat%7BY_%7Bij%7D%7D%20%20%281-%5Chat%7BY_%7Bij%7D%7D%29%7D%20%20x_t%20%28Y_%7Bij%7D%20-%20%5Chat%7BY_%7Bij%7D%7D%29 "\hat{\omega}_{it} = \hat{\omega}_{i(t-1)} + \frac{RD^2_{i(t-1)}}{1+\hat{Y_{ij}}  (1-\hat{Y_{ij}})}  x_t (Y_{ij} - \hat{Y_{ij}})")

![RD^2\_{i t} = RD^2\_{i(t-1)} - \\frac{\\hat{Y\_{ij}}(1-\\hat{Y\_{ij}})}{1+\\hat{Y\_{ij}}  (1-\\hat{Y\_{ij}})s\_t^2}(RD^2\_{i(t-1)}x\_i)(RD^2\_{i(t-1)}x\_i)^T](https://latex.codecogs.com/png.latex?RD%5E2_%7Bi%20t%7D%20%3D%20RD%5E2_%7Bi%28t-1%29%7D%20-%20%5Cfrac%7B%5Chat%7BY_%7Bij%7D%7D%281-%5Chat%7BY_%7Bij%7D%7D%29%7D%7B1%2B%5Chat%7BY_%7Bij%7D%7D%20%20%281-%5Chat%7BY_%7Bij%7D%7D%29s_t%5E2%7D%28RD%5E2_%7Bi%28t-1%29%7Dx_i%29%28RD%5E2_%7Bi%28t-1%29%7Dx_i%29%5ET "RD^2_{i t} = RD^2_{i(t-1)} - \frac{\hat{Y_{ij}}(1-\hat{Y_{ij}})}{1+\hat{Y_{ij}}  (1-\hat{Y_{ij}})s_t^2}(RD^2_{i(t-1)}x_i)(RD^2_{i(t-1)}x_i)^T")

For further knowledge read [Stephen J. Roberts, William Penny (2011)](https://www.researchgate.net/publication/2465226_Dynamic_Logistic_Regression)

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

Package contains actual data from Speedway Grand-Prix. There are two data.frames:

1.  `gpheats` - results SGP heats. Column `rank` is a numeric version of column `position` - rider position in race.
2.  `gpsquads` - summarized results of the events, with sum of point and final position.

``` r
str(gpheats)
```

    ## 'data.frame':    20649 obs. of  11 variables:
    ##  $ id      : num  1 1 1 1 2 2 2 2 3 3 ...
    ##  $ season  : int  1995 1995 1995 1995 1995 1995 1995 1995 1995 1995 ...
    ##  $ date    : POSIXct, format: "1995-05-20 21:00:00" "1995-05-20 21:00:00" ...
    ##  $ round   : int  1 1 1 1 1 1 1 1 1 1 ...
    ##  $ name    : chr  "Speedway Grand Prix of Poland" "Speedway Grand Prix of Poland" "Speedway Grand Prix of Poland" "Speedway Grand Prix of Poland" ...
    ##  $ heat    : int  1 1 1 1 2 2 2 2 3 3 ...
    ##  $ field   : int  1 2 3 4 1 2 3 4 1 2 ...
    ##  $ rider   : chr  "Tomasz Gollob" "Gary Havelock" "Chris Louis" "Tony Rickardsson" ...
    ##  $ points  : int  2 0 3 1 3 0 1 2 0 2 ...
    ##  $ position: chr  "2" "4" "1" "3" ...
    ##  $ rank    : num  2 4 1 3 1 4 3 2 4 2 ...

Data used in `sport` package must be in so called long format. Typicaly data.frame contains at least `id`, `name` and `rank`, with one row for one player within specific match. Package allows for any number of players within event and allows ties also. For all games, *output needs to be a rank/position in event*. Don't mix up rank output with typical 1-win, 0-lost. In `sport` package output for two player game is 1-winner 2-looser. Below example of two matches with 4 players each.

    ##   id             rider rank
    ## 1  1     Tomasz Gollob    2
    ## 2  1     Gary Havelock    4
    ## 3  1       Chris Louis    1
    ## 4  1  Tony Rickardsson    3
    ## 5  2     Sam Ermolenko    1
    ## 6  2    Jan Staechmann    4
    ## 7  2     Tommy Knudsen    3
    ## 8  2 Henrik Gustafsson    2

Estimate dynamic ratings
------------------------

To compute ratings using each algorithms one has to specify formula. Following manner is required, which estimates `name` (of a player) abilities, with observed outputs `rank` nested within particular event `id`. Variable names in formula are unrestricted, but model structure remains the same. All methods are named `method_run`. `formula = rank|id ~ name`

Output objects are of class `sport` have their own `print` and `summary` which provides most important informations. `print.sport` shows condensed informations about model performance like accuracy and consistency of model predictions with observed probabilities. More profound summarization are given by `summary` by showing ratings, ratings deviations and comparing model win probabilities with observed.

``` r
print(glicko)
```

    ## 
    ## Call: rank | id ~ rider
    ## 
    ## Number of unique pairs: 1500
    ## 
    ## Accuracy of the model: 0.63
    ## 
    ## True probabilities and Accuracy in predicted intervals:
    ##      Interval Model probability True probability  Accuracy   n
    ##  1:   [0,0.1]        0.06574276        0.1956522 0.8043478  92
    ##  2: (0.1,0.2]        0.15201026        0.3045267 0.6954733 243
    ##  3: (0.2,0.3]        0.25075788        0.2943144 0.7056856 299
    ##  4: (0.3,0.4]        0.34996827        0.4242788 0.5745192 416
    ##  5: (0.4,0.5]        0.45374289        0.4480249 0.5488565 481
    ##  6: (0.5,0.6]        0.55310184        0.5596659 0.5560859 419
    ##  7: (0.6,0.7]        0.65003173        0.5757212 0.5745192 416
    ##  8: (0.7,0.8]        0.74924212        0.7056856 0.7056856 299
    ##  9: (0.8,0.9]        0.84798974        0.6954733 0.6954733 243
    ## 10:   (0.9,1]        0.93425724        0.8043478 0.8043478  92

``` r
summary(glicko)
```

    ## $formula
    ## rank | id ~ rider
    ## 
    ## $method
    ## [1] "glicko"
    ## 
    ## $`Overall Accuracy`
    ## [1] 0.6276667
    ## 
    ## $`Number of pairs`
    ## [1] 3000
    ## 
    ## $r
    ##                  name        r        rd Model probability
    ##  1:     Tomasz Gollob 1562.326  31.96461         0.4981824
    ##  2:     Gary Havelock 1535.574  36.07222         0.3876544
    ##  3:       Chris Louis 1575.082  28.31770         0.5392178
    ##  4:  Tony Rickardsson 1704.619  29.73239         0.6623543
    ##  5:     Sam Ermolenko 1573.429  28.21369         0.5512214
    ##  6:    Jan Staechmann 1246.821  55.56176         0.2441970
    ##  7:     Tommy Knudsen 1704.262  37.44257         0.6662263
    ##  8: Henrik Gustafsson 1625.992  28.20670         0.5260341
    ##  9:   Mikael Karlsson 1245.190  61.99332         0.1694434
    ## 10:      Hans Nielsen 1781.379  33.81743         0.7905322
    ## 11:        Andy Smith 1389.424  31.63832         0.3353383
    ## 12:        Mark Loram 1514.087  28.19171         0.4837638
    ## 13:      Greg Hancock 1643.684  28.29307         0.5114290
    ## 14:        Marvyn Cox 1404.498  29.48348         0.3853776
    ## 15:     Dariusz Śledź 1459.486 110.21924         0.4066321
    ## 16:       Craig Boyce 1479.722  30.07608         0.4123869
    ## 17:      Billy Hamill 1696.884  29.76935         0.6170499
    ## 18:    Peter Karlsson 1585.646  36.59011         0.4850580
    ## 19:     Franz Leitner 1406.367  99.02141         0.3846200
    ## 20:         Gerd Riss 1615.527  64.31768         0.6407864
    ## 21:       Josh Larsen 1121.785 106.05383         0.1999891
    ## 22:    Lars Gunnestad 1445.625  89.65285         0.3996265
    ## 23:       Jason Crump 1467.898  43.82568         0.4602013
    ## 24:       Leigh Adams 1512.791  48.44582         0.3942940
    ## 25:        Joe Screen 1455.754  42.99367         0.3516820
    ## 26:   Stefano Alfonso 1424.647  85.49851         0.5690259
    ##                  name        r        rd Model probability
    ##     True probability  Accuracy pairings
    ##  1:       0.50657895 0.5986842      152
    ##  2:       0.47619048 0.6349206      126
    ##  3:       0.52925532 0.5585106      188
    ##  4:       0.66137566 0.6349206      189
    ##  5:       0.52688172 0.5215054      186
    ##  6:       0.22222222 0.7777778       72
    ##  7:       0.65447154 0.6097561      123
    ##  8:       0.58064516 0.6559140      186
    ##  9:       0.19444444 0.7916667       72
    ## 10:       0.75668449 0.7326203      187
    ## 11:       0.31578947 0.6374269      171
    ## 12:       0.44444444 0.6243386      189
    ## 13:       0.59259259 0.5925926      189
    ## 14:       0.33333333 0.6888889      180
    ## 15:       0.46666667 0.6666667       15
    ## 16:       0.41470588 0.6176471      170
    ## 17:       0.64804469 0.6145251      179
    ## 18:       0.50980392 0.5588235      102
    ## 19:       0.38888889 0.5555556       18
    ## 20:       0.55555556 0.5277778       36
    ## 21:       0.05555556 0.8888889       18
    ## 22:       0.38888889 0.6666667       18
    ## 23:       0.38461538 0.5641026       78
    ## 24:       0.43333333 0.6166667       60
    ## 25:       0.35897436 0.6923077       78
    ## 26:       0.33333333 0.3333333       18
    ##     True probability  Accuracy pairings
