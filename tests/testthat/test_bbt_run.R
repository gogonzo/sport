context("bbt")
data <- data.frame( name = c( "A", "B", "C", "D" ), rank  = c( 3, 4, 1, 2 ), sig=rep(1.1,4), weight=rep(1.01,4), date=c("a","b","c","d"))
sig  <- setNames( rep(1,4), c("A","B","C","D"))
rd    <- setNames( rep(350,4), c("A","B","C","D") )

test_that("valid bbt computation",{
  expect_equal(
    c(1499.921841, 1399.680003, 1550.401084, 1699.997072),
    bbt_run( rank ~ name, data = data, r = c( 1500, 1400, 1550, 1700 ) , rd    = c( 200,  30,   100,  300 ) )$final_r
  )
})

test_that("init r passed",{
  expect_true( all(
    sum(bbt_run(formula = rank ~ name, data=data, init_r = 1000)$final_r)==4000
  ))
})

test_that("init rd passed",{
  expect_true(all(bbt_run(formula = rank ~ name, data=data, init_rd = 100)$final_rd<100))
})

test_that("bigger rating change for higher deviation",{
  expect_true( all(
    abs( 25 - bbt_run(formula = rank~name, data=data  ,gamma=1.001, rd = setNames( rep(26/6,4), c("A","B","C","D")))$final_r ) >
    abs( 25 - bbt_run(formula = rank~name, data=data, gamma=1.001,   rd = setNames( rep(25/6,4), c("A","B","C","D")))$final_r )
  ))
})

test_that("bigger rating change for higher sigma",{
  expect_true( all(
    abs( 25 - bbt_run(formula = rank ~ name, data=data, rd = rep(3,4), sig="sig")$final_r ) >
    abs( 25 - bbt_run(formula = rank ~ name, data=data, rd = rep(3,4)           )$final_r )
  ))
})

test_that("bigger rating change for higher weight",{
  expect_true( all(
    abs( 25 - bbt_run(formula = rank ~ name, data=data, weight="weight")$final_r ) >
      abs( 25 - bbt_run(formula = rank ~ name, data=data                 )$final_r )
  ))
})

test_that("smaller rating change for higher beta",{
  expect_true( all(
    abs( 25 - bbt_run(formula = rank ~ name, beta = 25/3, data=data)$final_r ) <
      abs( 25 - bbt_run(formula = rank ~ name, beta = 25/4,data=data)$final_r )
  ))
})

test_that("bigger rd change for higher gamma",{
  expect_true( all(
    bbt_run(formula = rank ~ name,   gamma = 1.1, data=data)$final_rd  <
    bbt_run(formula = rank ~ name, gamma = 1,data=data)$final_rd 
  ))
})

test_that("identifier passed succesfuly",{
  expect_equal(
    as.character(data$date),
    attr(bbt_run(formula = rank~name, data=data, idlab = "date")$r,"identifier")
  )
})

test_that("valid glicko output names",{
  expect_identical(
    c("final_r","final_rd","r","pairs"),
    names( bbt_run( rank ~ name, data = data) )
  )
})

test_that("valid glicko attr names",{
  expect_identical(
    list(names = c("final_r","final_rd","r","pairs"),
         class = "sport", method = "bbt",formula = rank~name),
    attributes( bbt_run( rank ~ name, data = data, weight="weight", sig = "sig", init_r=1000, init_rd=200) )
  )
})

test_that("r object has date labels attribute",{
  expect_identical(
    list(names = c("id","name","r","rd"), row.names=1:4, class=c("data.table","data.frame"),identifier=as.character(c(1,1,1,1))),
    attributes(glicko_run( rank ~ name, data = data, weight="weight", sig = "sig", init_r=1000, init_rd=200)$r)[-3]
  )
})