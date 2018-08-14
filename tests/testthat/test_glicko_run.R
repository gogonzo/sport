context("glicko_run")
data <- data.frame( name = c( "A", "B", "C", "D" ), rank  = c( 3, 4, 1, 2 ), sig=rep(2,4), weight=rep(1.01,4), date=c("a","b","c","d"))
sig  <- setNames( rep(1,4), c("A","B","C","D"))
rd    <- setNames( rep(350,4), c("A","B","C","D") )

test_that("valid glicko computation",{
  expect_identical(
    c(1464.2973571629759135, 1396.0386597041820096, 1606.5214821882570959, 1674.8363617318223078),
    glicko_run( rank ~ name, data = data, r = c( 1500, 1400, 1550, 1700 ) , rd    = c( 200,  30,   100,  300 ) )$final_r
  )
})

test_that("init r passed",{
  expect_true( all(
    sum(glicko_run(formula = rank ~ name, data=data, init_r = 1000)$final_r)==4000
  ))
})

test_that("init rd passed",{
  expect_true(all(glicko_run(formula = rank ~ name, data=data, init_rd = 100)$final_rd<100))
})

test_that("higher rating change for higher deviation",{
  expect_true( all(
    abs( 1500 - glicko_run(formula = rank~name, data=data, rd = setNames( rep(301,4), c("A","B","C","D")))$final_r ) >
    abs( 1500 - glicko_run(formula = rank~name, data=data, rd = setNames( rep(300,4), c("A","B","C","D")))$final_r )
  ))
})

test_that("higher rating change for higher weight",{
  expect_true( all(
    abs( 1500 - glicko_run(formula = rank ~ name, data=data, weight="weight")$final_r ) >
    abs( 1500 - glicko_run(formula = rank ~ name, data=data)$final_r )
  ))
})

test_that("higher rating change for higher sigma",{
  expect_true( all(
    abs( 1500 - glicko_run(formula = rank ~ name, r = c( 1500, 1400, 1550, 1700 ) , rd    = c( 200,  30,   100,  300 ),data=data, sig="sig")$final_r ) >
    abs( 1500 - glicko_run(formula = rank ~ name, r = c( 1500, 1400, 1550, 1700 ) , rd    = c( 200,  30,   100,  300 ),data=data)$final_r )
  ))
})

test_that("identifier passed succesfuly",{
  expect_equal(
    as.character(data$date),
    attr(glicko_run(formula = rank~name, data=data, idlab = "date")$r,"identifier")
  )
})

test_that("valid glicko output names",{
  expect_identical(
    c("final_r","final_rd","r","pairs"),
    names( glicko_run( rank ~ name, data = data) )
  )
})

test_that("valid glicko attr names",{
  expect_identical(
    list(names = c("final_r","final_rd","r","pairs"),
         class = "sport", method = "glicko",formula = rank~name),
    attributes( glicko_run( rank ~ name, data = data, weight="weight", sig = "sig", init_r=1000, init_rd=200) )
  )
})

test_that("r object has date labels attribute",{
  expect_identical(
    list(names = c("id","name","r","rd"), row.names=1:4, class=c("data.table","data.frame"),identifier=as.character(c(1,1,1,1))),
    attributes(glicko_run( rank ~ name, data = data, weight="weight", sig = "sig", init_r=1000, init_rd=200)$r)[-3]
  )
})