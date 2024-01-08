context("print, summary")
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

testthat::test_that("Plot layers match expectations", {
  p <- plot(dbl)
  testthat::expect_identical(class(p$layers[[1]]$geom), c("GeomLinerange", "Geom", "ggproto", "gg"))
  testthat::expect_identical(class(p$layers[[2]]$geom), c("GeomPoint", "Geom", "ggproto", "gg"))
})

testthat::test_that("Scale is labelled 'r'", {
  p <- plot(dbl)
  testthat::expect_identical(p$labels$y, "r")
})

testthat::test_that("Scale range is NULL", {
  p <- plot(dbl)
  testthat::expect_null(p$scales$scales[[1]]$range$range)
})
