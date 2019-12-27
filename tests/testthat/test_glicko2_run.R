context("glicko2_run")
data  <- data.frame( id = 1,name = c( "A", "B", "C", "D" ), rank  = c( 3, 4, 1, 2 ), weight=1.05, date=c("a","b","c","d"))
sigma <- setNames( rep(1,4), c("A","B","C","D"))
rd    <- setNames( rep(350,4), c("A","B","C","D") )

test_that("Error with NA parameters",{
  gpheats$weight   <- 1.1
  gpheats$weight[17] <- NaN
  expect_error(
    glicko2_run( rank|id~rider,data=gpheats[17:21,] , weight = "weight"  ),
    paste0("Parameters error after evaluating id=", gpheats$id[17])
  )
})

test_that("init r passed",{
  expect_equal(
    sum(glicko2_run(formula = rank | id ~ name, data=data, init_r = 1000)$final_r),
    4000
  )
})

test_that("init rd passed",{
  expect_true( 
    all(glicko2_run(formula = rank | id ~ name, data=data, init_rd = 100)$final_rd<100)
  )
})

test_that("higher rating change for higher deviation",{
  expect_true( all(
    abs( 1500 - glicko2_run(formula = rank|id~name, data=data, rd = setNames( rep(350,4), c("A","B","C","D")))$final_r ) >
    abs( 1500 - glicko2_run(formula = rank|id~name, data=data, rd = setNames( rep(349,4), c("A","B","C","D")))$final_r )
  ))
})

test_that("R and RD exacltly proportional to weight",{
  r  <- setNames(c( 1500, 1400, 1550, 1700 ), c("A","B","C","D"))
  rd <- setNames(c( 200,  30,   100,  300 ), c("A","B","C","D"))
  
  model1 <- glicko2_run( rank | id ~ name, data = data, r = r ,rd = rd, weight="weight" )
  model2 <- glicko2_run( rank | id ~ name, data = data, r = r ,rd = rd )
  
  expect_true( all(
    round((r - model1$final_r)/(r-model2$final_r),10)==1.05
  ))
  
  expect_true( all(
    round((rd - model1$final_rd)/(rd-model2$final_rd),10)==1.05
  ))
  
})

test_that("higher deviation for higher sigma",{
  expect_true( all(
    glicko2_run(formula = rank|id~name, data=data, sigma = setNames( rep(0.11,4), c("A","B","C","D")))$final_rd >
    glicko2_run(formula = rank|id~name, data=data, sigma = setNames( rep(0.1,4), c("A","B","C","D")))$final_rd
  ))
})

test_that("kappa is working",{
  expect_true(all( 
    glicko2_run(rank|id~name, data=data, kappa=.99)$final_rd == 350*.99
  ))
})

test_that("identifier passed succesfuly",{
  expect_equal(
    as.character(data$date),
    attr(glicko2_run(formula = rank|id~name, data=data, idlab = "date")$r,"identifier")
  )
})

