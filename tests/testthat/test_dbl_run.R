context("dbl_run")
gpheats$field_f <- as.factor(gpheats$field)
gpheats$beta   <- 100
gpheats$weight   <- .9
gpheats$weight2  <- 1.0

formula      <- rank|id ~ rider + field + field_f
r  <- dbl_run( formula, data = gpheats[1:16,])$final_r
rd <- dbl_run( formula, data = gpheats[1:16,])$final_rd

test_that("final parameters list match all variables.levels ",{
  expect_identical(
    sport:::allLevelsList( rank|id ~ rider + field + field:name+rider:name + beta:weight , gpheats[1:32,]),
    names(        dbl_run( rank|id ~ rider + field + field:name+rider:name + beta:weight , data = gpheats[1:32,])$final_r)
  )
})

test_that("only factor, character, numeric and integer allowed",{
  expect_error(
    sport:::allLevelsList( rank|id~rider+date , gpheats[1:32,]),
    "Variables can be only of following classes: factor, character, numeric, integer."
  )
  
  expect_error(
    sport:::allLevelsList( rank|id~rider:date , gpheats[1:32,]),
    "Variables can be only of following classes: factor, character, numeric, integer."
  )
  
  expect_error(
    sport:::createTermMatrix( rank|id~rider+date , gpheats[1:32,]),
    "Variables can be only of following classes: factor, character, numeric, integer."
  )
  
  expect_error(
    sport:::createTermMatrix( rank|id~rider:date , gpheats[1:32,]),
    "Variables can be only of following classes: factor, character, numeric, integer."
  )
  
})


test_that("Error with NA parameters",{
  gpheats$weight[17] <- NaN
  expect_error(
    dbl_run( formula,data=gpheats[17:21,] , r=r, rd=rd, weight = "weight"  ),
    paste0("Parameters error after evaluating id=", gpheats$id[17])
  )
})

test_that("passing prior estimates",{
  expect_true(
    all( !is.na(dbl_run( formula, gpheats[17:32,], r=r, rd=rd  )$final_r) )
  )
})

test_that("beta changes calculation",{
  expect_true(all(
    abs(r-dbl_run( formula, gpheats[17:32,], r=r, rd=rd, beta = "beta"  )$final_r) !=
    abs(r-dbl_run( formula, gpheats[17:32,], r=r, rd=rd )$final_r)
  ))
})

test_that("R and RD exacltly proportional to weight",{
  model1 <- dbl_run( formula, gpheats[1:4,], weight = "weight"  )
  model2 <- dbl_run( formula, gpheats[1:4,], weight = "weight2" )
  
  expect_true( all(
    round(abs(0-model1$final_r)/abs(0-model2$final_r),2) == .9
  ))
  
  expect_true( all(
    round(abs(1-model1$final_rd)/abs(1-model2$final_rd),2) == .9
  ))
  
})

test_that("identifier passed succesfuly",{
  expect_identical(
    as.character(gpheats$date[1:32]),
    attr( dbl_run(formula = rank|id~rider+field+field_f,r=r,rd=rd, data=gpheats[1:32,], idlab = "date")$r,"identifier")
  )
})

test_that("valid glicko output names",{
  expect_identical(
    c("final_r","final_rd","r","pairs"),
    names( dbl_run( rank|id ~ rider, data = gpheats[1:32,]) )
  )
})

test_that("valid dbl attr names",{
  expect_equal(
    list(names = c("final_r","final_rd","r","pairs"),
         class = "rating", method = "dbl",formula = rank|id~rider,
         settings=list(init_r=0, init_rd=1,beta="beta",weight="weight",kappa=0.5,idlab="id")),
    attributes( dbl_run( rank|id ~ rider, data = gpheats[1:32,], weight="weight", beta = "beta") )
  )
})