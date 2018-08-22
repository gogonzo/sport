context("print, summary")
glicko <- glicko_run(rank|id ~ rider, data = gpheats[1:32,] )
dbl    <- dbl_run(rank|id ~ rider, data = gpheats[1:32,] )

test_that("summary",{
  expect_identical(
    c("formula","method","Overall Accuracy","Number of pairs","r"),
    names(summary(dbl)),
    names(summary(glicko))
  )
})

test_that("",{
  expect_output(
    print(glicko)
  )
})
