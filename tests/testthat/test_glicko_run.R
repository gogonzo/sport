context("glicko_run")
data <- data.frame( 
  id = 1,
  name = c( "A", "B", "C", "D" ), 
  rank  = c( 3, 4, 1, 2 ),
  field = 1:4,
  date = seq(Sys.Date()-3, Sys.Date(), by="1 day"),
  sigma=rep(2,4), 
  weight=rep(1.05,4),
  weight2= rep(0.9, 4),
  date=c("a","b","c","d"))
sigma  <- setNames( rep(1,4), c("A","B","C","D"))
rd    <- setNames( rep(350,4), c("A","B","C","D") )
r     <- setNames( rep(1500,4), c("A","B","C","D") )

test_that("missing formula error",{
  expect_error( glicko_run(data),"Formula is not specified")
})

test_that("missing data error",{
  expect_error( glicko_run(formula=rank|id~name),"Data is not provided")
})

test_that("lhs formula message",{
  expect_warning( 
    glicko_run(rank~name, data=data, r=r, rd=rd),
    "LHS of formula doesn't contain `| id` element. It will be assummed that all belongs to the same event id")
})

test_that("lhs formula message",{
  expect_error( 
    glicko_run(rank + id ~ name, data=data, r=r, rd=rd),
    "LHS of formula must be seperated by `|` operator eg. `rank | id ~ .`")
})

test_that("lhs formula error ",{
  expect_error( 
    glicko_run(formula=rank|id + elo~name, data=data),
    "LHS must contain 1 or 2 variables")
})

test_that("rhs formula error ",{
  expect_error( 
    glicko_run(formula=rank|id~name+field, data=data),
    "glicko_run expects only one variable which is `~ name`")
})

test_that("missing rd message",{
  expect_message( 
    glicko_run(rank|id~name, r=r,data=data),
    "rd is missing and will set to default=350")
})

test_that("missing r message",{
  expect_message( 
    glicko_run(rank|id~name, rd=rd,data=data),
    "r is missing and will set to default=1500")
})

test_that("Error with NA parameters",{
  gpheats$weight   <- 1.1
  gpheats$weight[17] <- NaN
  expect_error(
    glicko_run( rank|id~rider,data=gpheats[17:21,] , weight = "weight"  ),
    paste0("Parameters error after evaluating id=", gpheats$id[17])
  )
})

test_that("valid glicko computation",{

  expect_identical(
    setNames(c(1464.297, 1396.039, 1606.521, 1674.836), c("A","B","C","D")),
    round(
      glicko_run( 
        rank | id ~ name, 
        data = data, 
        r = setNames(c(1500, 1400, 1550, 1700), c("A","B","C","D")), 
        rd = setNames(c( 200,  30,   100,  300 ), c("A","B","C","D")) 
      )$final_r,
      3
    )
  )
})

test_that("init r passed",{
  expect_true( 
    all(
      sum(
        glicko_run(formula = rank | id ~ name, data = data, init_r = 1000)$final_r
      ) == 4000
  ))
})

test_that("init rd passed",{
  expect_true(all(glicko_run(formula = rank | id ~ name, data=data, init_rd = 100)$final_rd<100))
})

test_that("higher rating change for higher deviation",{
  expect_true( all(
    abs( 1500 - glicko_run(formula = rank | id ~name, data=data, rd = setNames( rep(301,4), c("A","B","C","D")))$final_r ) >
    abs( 1500 - glicko_run(formula = rank | id ~name, data=data, rd = setNames( rep(300,4), c("A","B","C","D")))$final_r )
  ))
})

test_that("R and RD exacltly proportional to weight",{
  r  <- setNames(c( 1500, 1400, 1550, 1700 ), c("A","B","C","D"))
  rd <- setNames(c( 200,  30,   100,  300 ), c("A","B","C","D"))
  model1 <- glicko_run( rank | id ~ name, data = data, r = r ,rd = rd, weight="weight" )
  model2 <- glicko_run( rank | id ~ name, data = data, r = r ,rd = rd )
  
  expect_true( all(
    round((r - model1$final_r)/(r-model2$final_r),10)==1.05
  ))
  
  expect_true( all(
    round((rd - model1$final_rd)/(rd-model2$final_rd),10)==1.05
  ))
  
})

test_that("higher rating change for higher sigma",{
  r  <- setNames(c( 1500, 1400, 1550, 1700 ), c("A","B","C","D"))
  rd <- setNames(c( 200,  30,   100,  300 ), c("A","B","C","D"))
  
  expect_true( all(
    abs( 1500 - glicko_run(formula = rank | id ~ name, r = r, rd = rd, data=data, sigma="sigma")$final_r ) >
    abs( 1500 - glicko_run(formula = rank | id ~ name, r = r , rd    = rd, data=data)$final_r )
  ))
})

test_that("kappa is working",{
  expect_true(all( 
    glicko_run(rank|id~name, data=data, kappa=.99)$final_rd == 350*.99
  ))
})

test_that("identifier passed succesfuly",{
  expect_equal(
    as.character(data$date),
    attr(glicko_run(formula = rank | id ~name, data=data, idlab = "date")$r,"identifier")
  )
})

test_that("valid glicko output names",{
  expect_identical(
    c("final_r","final_rd","r","pairs"),
    names( glicko_run( rank | id ~ name, data = data) )
  )
})

test_that("valid glicko attr names",{
  expect_equal(
    list(names = c("final_r","final_rd","r","pairs"),
         class = "rating", method = "glicko",formula = rank | id ~name,
         settings = list(init_r = 1000, init_rd = 200, sigma="sigma", weight="weight",kappa=0.5, idlab="id")),
    attributes( glicko_run( rank | id ~ name, data = data, weight="weight", sigma = "sigma", init_r=1000, init_rd=200) )
  )
})

test_that("r object has date labels attribute",{
  expect_identical(
    list(names = c("id","name","r","rd","p_win"), class=c("data.table","data.frame"), row.names=1:4,identifier=as.character(c(1,1,1,1))),
    attributes( glicko_run( rank | id ~ name, data = data, weight="weight", sigma = "sigma", init_r=1000, init_rd=200)$r )[-4]
  )
})
