context("dbl")
data <- gpheats[1:32,]
data$field_f <- as.factor(data$field)
data$sigma   <- 3
data$weight  <- 1.01

formula      <- rank|id ~ rider + field + field_f
r  <- dbl_run( formula, data = data[1:16,])$final_r
rd <- dbl_run( formula, data = data[1:16,])$final_rd

test_that("final parameters list match all variables.levels ",{
  expect_identical(
    sport:::allLevelsList( rank|id ~ rider + field + field:name+rider:name,data),
    names(        dbl_run(rank|id ~ rider + field + field:name+rider:name, data = data)$final_r)
  )
})

test_that("passing prior estimates",{
  expect_true(
    all( !is.na(dbl_run( formula, data[17:32,], r=r, rd=rd  )$final_r) )
  )
})

test_that("sigma changes calculation",{
  expect_true(all(
    abs(r-dbl_run( formula, data[17:32,], r=r, rd=rd, sig = "sigma"  )$final_r) !=
    abs(r-dbl_run( formula, data[17:32,], r=r, rd=rd )$final_r)
  ))
})


test_that("bigger rating change for higher weight",{
  expect_true( all(
    abs(r-dbl_run( formula, data[17:32,], r=r, rd=rd, weight = "weight"  )$final_r) >
    abs(r-dbl_run( formula, data[17:32,], r=r, rd=rd )$final_r)
  ))
})

test_that("identifier passed succesfuly",{
  expect_identical(
    as.character(data$date),
    attr( dbl_run(formula = rank|id~rider,r=r,rd=rd, data=data, idlab = "date")$r,"identifier")
  )
})

test_that("valid glicko output names",{
  expect_identical(
    c("final_r","final_rd","r","pairs"),
    names( dbl_run( rank|id ~ rider, data = data) )
  )
})


test_that("valid dbl attr names",{
  expect_identical(
    list(names = c("final_r","final_rd","r","pairs"),
         class = "sport", method = "dbl",formula = rank|id~rider),
    attributes( dbl_run( rank|id ~ rider, data = data, weight="weight", sig = "sigma") )
  )
})