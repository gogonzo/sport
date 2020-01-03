context("print, summary")
glicko <- glicko_run(rank | id ~ player(rider), data = gpheats[1:32, ])
dbl <- dbl_run(rank | id ~ player(rider), data = gpheats[1:32, ])

test_that("summary", {
  expect_identical(
    c("formula", "method", "Overall Accuracy", "Number of pairs", "r"),
    names(summary(dbl))
  )
  expect_identical(
    c("formula", "method", "Overall Accuracy", "Number of pairs", "r"),
    names(summary(glicko))
  )

})

test_that("Plot layers match", {
  expect_output(
    print(glicko)
  )
})

test_that("Plot layers match expectations", {
  p <- plot(dbl)
  expect_identical(class(p$layers[[1]]$geom), c("GeomLinerange", "Geom", "ggproto", "gg"))
  expect_identical(class(p$layers[[2]]$geom), c("GeomPoint", "Geom", "ggproto", "gg"))
})

test_that("Scale is labelled 'r'", {
  p <- plot(dbl)
  expect_identical(p$labels$y, "r")
})

test_that("Scale range is NULL", {
  p <- plot(dbl)
  expect_null(p$scales$scales[[1]]$range$range)
})

