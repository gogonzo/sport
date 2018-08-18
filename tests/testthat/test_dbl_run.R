context("dbl")
gpheats$field_f <- as.factor(gpheats$field)
gpheats$beta   <- 100
gpheats$weight  <- 1.01

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
})
test_that("only factor, character, numeric and integer allowed",{
  expect_error(
    sport:::allLevelsList( rank|id~rider:date , gpheats[1:32,]),
    "Variables can be only of following classes: factor, character, numeric, integer."
  )
})

test_that("only factor, character, numeric and integer allowed",{
  expect_error(
    sport:::createTermMatrix( rank|id~rider+date , gpheats[1:32,]),
    "Variables can be only of following classes: factor, character, numeric, integer."
  )
})
test_that("only factor, character, numeric and integer allowed",{
  expect_error(
    sport:::createTermMatrix( rank|id~rider:date , gpheats[1:32,]),
    "Variables can be only of following classes: factor, character, numeric, integer."
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


test_that("bigger rating change for higher weight",{
  expect_true( all(
    abs(r-dbl_run( formula, gpheats[17:32,], r=r, rd=rd, weight = "weight"  )$final_r) >
    abs(r-dbl_run( formula, gpheats[17:32,], r=r, rd=rd )$final_r)
  ))
})

test_that("identifier passed succesfuly",{
  expect_identical(
    as.character(gpheats$date[1:32]),
    attr( dbl_run(formula = rank|id~rider,r=r,rd=rd, data=gpheats[1:32,], idlab = "date")$r,"identifier")
  )
})

test_that("valid glicko output names",{
  expect_identical(
    c("final_r","final_rd","r","pairs"),
    names( dbl_run( rank|id ~ rider, data = gpheats[1:32,]) )
  )
})


test_that("valid dbl attr names",{
  expect_identical(
    list(names = c("final_r","final_rd","r","pairs"),
         class = "sport", method = "dbl",formula = rank|id~rider),
    attributes( dbl_run( rank|id ~ rider, data = gpheats[1:32,], weight="weight", beta = "beta") )
  )
})