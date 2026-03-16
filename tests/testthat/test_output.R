glicko <- glicko_run(rank | id ~ player(rider), data = gpheats[1:32, ])
dbl <- dbl_run(rank | id ~ player(rider), data = gpheats[1:32, ])

testthat::test_that("summary", {
  testthat::expect_identical(
    c("formula", "method", "Overall Accuracy", "Number of pairs", "r"),
    names(summary(dbl))
  )
  testthat::expect_identical(
    c("formula", "method", "Overall Accuracy", "Number of pairs", "r"),
    names(summary(glicko))
  )
})

testthat::test_that("Plot layers match", {
  testthat::expect_output(
    print(glicko)
  )
})

testthat::test_that("Plot output matches snapshot", {
  vdiffr::expect_doppelganger("dbl_plot", plot(dbl))
  vdiffr::expect_doppelganger("glicko_plot", plot(glicko))
})
