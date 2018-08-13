data <- data.frame( id = 1,name = c( "A", "B", "C", "D" ), rank  = c( 3, 4, 1, 2 ), weight=1.01, date=c("a","b","c","d"))
sig  <- setNames( rep(1,4), c("A","B","C","D"))
rd    <- setNames( rep(350,4), c("A","B","C","D") )

test_that("higher rating change for higher deviation",{
  expect_true( all(
    abs( 1500 - glicko2_run(formula = rank|id~name, data=data, rd = setNames( rep(350,4), c("A","B","C","D")))$final_r ) -
    abs( 1500 - glicko2_run(formula = rank|id~name, data=data, rd = setNames( rep(349,4), c("A","B","C","D")))$final_r )
  ))
})

test_that("higher rating change for higher weight",{
  expect_true( all(
    abs( 1500 - glicko2_run(formula = rank|id~name, data=data, weight="weight")$final_r ) >
    abs( 1500 - glicko2_run(formula = rank|id~name, data=data)$final_r )
  ))
})

test_that("higher deviation for higher sigma",{
  expect_true( all(
    glicko2_run(formula = rank|id~name, data=data, sig = setNames( rep(0.11,4), c("A","B","C","D")))$final_rd >
    glicko2_run(formula = rank|id~name, data=data, sig = setNames( rep(0.1,4), c("A","B","C","D")))$final_rd
  ))
})

#test_that("identifier passed succesfuly",{
#  expect_identical(
#    data$date,
#    attr(glicko2_run(formula = rank|id~name, data=data, date="date")$r,"identifier")
#  )
#})

