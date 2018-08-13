context("glicko_run")
data1 <- data.frame( name = c( "A", "B", "C", "D" ), rank  = c( 3, 4, 1, 2 ), weight = 1, sig = 1 )

test_that("valid glicko computation",{
  expect_identical(
    c(1464.2973571629759135, 1396.0386597041820096, 1606.5214821882570959, 1674.8363617318223078),
    glicko_run( rank ~ name, data = data1, r = c( 1500, 1400, 1550, 1700 ) , rd    = c( 200,  30,   100,  300 ) )$final_r
  )
})

test_that("valid glicko output names",{
  expect_identical(
    c("final_r","final_rd","r","pairs"),
    names( glicko_run( rank ~ name, data = data1) )
  )
})

test_that("valid glicko attr names",{
  expect_identical(
    list(names = c("final_r","final_rd","r","pairs"),
         class = "sport", method = "glicko",formula = rank~name),
    attributes( glicko_run( rank ~ name, data = data1, weight="weight", sig = "sig", init_r=1000, init_rd=200) )
  )
})

test_that("r object has date labels attribute",{
  expect_identical(
    list(names = c("id","names","r","rd"), row.names=1:4, class="data.frame",identifier=c(1,1,1,1)),
    attributes(glicko_run( rank ~ name, data = data1, weight="weight", sig = "sig", init_r=1000, init_rd=200)$r)
  )
})