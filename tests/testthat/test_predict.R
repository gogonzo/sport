context("predict")
data <- data.frame( 
  id = 1,
  name = c( "A", "B", "C", "D" ), 
  rank  = c( 3, 4, 1, 2 ),
  field = 1:4,
  date = seq(Sys.Date()-3, Sys.Date(), by="1 day"),
  sigma=rep(0,4), 
  weight=rep(1.0,4), date=c("a","b","c","d"))
sigma  <- setNames( rep(1,4), c("A","B","C","D"))
rd    <- setNames( rep(350,4), c("A","B","C","D") )
r     <- setNames( rep(1500,4), c("A","B","C","D") )


test_that("valid glicko predict computation",{
  r  <- setNames(c( 1500, 1400, 1550, 1700 ), c("A","B","C","D"))
  rd <- setNames(c( 200,  30,   100,  300 ), c("A","B","C","D"))
  
  expect_identical(
    round(predict(
      glicko_run( rank | id ~ name, data = data, r = r, rd = rd),
      data)$P,7),
    c(0.5873752, 0.3287548, 0.2759733, 0.4126248, 0.2390720, 0.2015118, 0.6712452, 0.7609280, 0.4186856, 0.7240267, 0.7984882, 0.5813144)
  )
})

test_that("valid glicko2 predict computation",{
  r  <- setNames(c( 1500, 1400, 1550, 1700 ), c("A","B","C","D"))
  rd <- setNames(c( 200,  30,   100,  300 ), c("A","B","C","D"))
  
  expect_identical(
    round(predict(
      glicko2_run( rank | id ~ name, data = data, r = r, rd    = rd),
      data)$P,7),
    c(0.6356254, 0.2616612, 0.2816442, 0.3643746, 0.1592030, 0.1777754, 0.7383388, 0.8407970, 0.5089747, 0.7183558, 0.8222246, 0.4910253)
  )
})

test_that("valid bbt predict computation",{
  expect_identical(
    round(predict(
      bbt_run( rank | id ~ name, data = data),
      data)$P,7),
    c(0.6215515, 0.2704624, 0.3784485, 0.3784485, 0.1841595, 0.2704624, 0.7295376, 0.8158405, 0.6215515, 0.6215515, 0.7295376, 0.3784485)
  )
})

test_that("valid bbt predict computation",{
  expect_identical(
    round(predict(
      dbl_run( rank | id ~ name, data = data),
      data)$P,7),
    c(0.6267486, 0.2618092, 0.3732514, 0.3732514, 0.1743826, 0.2618092, 0.7381908, 0.8256174, 0.6267486, 0.6267486, 0.7381908, 0.3732514)
  )
})
