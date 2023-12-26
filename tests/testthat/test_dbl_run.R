testthat::context("dbl_run")

testthat::test_that("formula supports rank_var | id_var ~ player(var) syntax", {
  testthat::expect_silent(
    dbl_run(
      formula <- rank | id ~ player(rider),
      data = gpheats[1:8, ]
    )
  )
})

testthat::test_that("formula syntax supports additional (except player) independent variables", {
  testthat::expect_silent(
    dbl_run(
      formula <- rank | id ~ player(rider) + field,
      data = gpheats[1:8, ]
    )
  )
})

testthat::test_that("formula syntax supports interactions", {
  testthat::expect_silent(
    dbl_run(
      formula <- rank | id ~ player(rider) + field * heat + heat:rider,
      data = gpheats[1:8, ]
    )
  )
})


testthat::test_that("formula syntax method doesn't allow players nested in teams", {
  testthat::expect_error(
    dbl_run(
      formula <- rank | id ~ player(rider | team),
      data = gpheats[1:8, ]
    ),
    "Please specify only one variable inside of the player"
  )
})

testthat::test_that("LHS of the formula requires rank | id", {
  testthat::expect_error(
    dbl_run(
      formula <- rank + id ~ player(rider),
      data = gpheats[1:8, ]
    ),
    "LHS"
  )
})


testthat::test_that("formula syntax method requires player", {
  testthat::expect_error(
    dbl_run(
      formula <- rank | id ~ rider,
      data = gpheats[1:8, ]
    ),
    "Formula requires specifying player"
  )
})

testthat::test_that("formula syntax method requires rank grouped by event", {
  testthat::expect_warning(
    dbl_run(
      formula <- rank ~ player(rider),
      data = gpheats[1:8, ]
    ),
    "LHS"
  )
})

testthat::test_that("formula requires variable to be present in data", {
  testthat::expect_error(
    dbl_run(
      formula <- rank | idd ~ rider,
      data = gpheats[1:8, ]
    ),
    "idd specified in formula are not present in data"
  )
})


testthat::test_that("initial", {
  testthat::expect_warning(
    dbl_run(
      formula <- rank | id ~ player(rider),
      data = gpheats[1:8, ],
      r = setNames(.5, "rider=Tomasz Gollob")
    ),
    "Missing parameters will be added with init_r"
  )

  testthat::expect_warning(
    dbl_run(
      formula <- rank | id ~ player(rider),
      data = gpheats[1:8, ],
      rd = setNames(.5, "rider=Tomasz Gollob")
    ),
    "Missing parameters will be added with init_rd"
  )
})

testthat::test_that("output", {
  out1 <- dbl_run(
    formula = formula <- rank | id ~ player(rider),
    data = gpheats[1:4, ],
    lambda = 1,
    weight = 1,
    init_r = 0,
    init_rd = 1
  )

  testthat::expect_identical(
    out1$final_r,
    c(
      "rider=Chris Louis" = 1,
      "rider=Gary Havelock" = -1,
      "rider=Tomasz Gollob" = 1 / 3,
      "rider=Tony Rickardsson" = -1 / 3
    )
  )

  testthat::expect_true(all(out1$r$R == 0))
  testthat::expect_true(all(out1$r$RD == 1))
  testthat::expect_true(all(out1$pairs$P == 0.5))


  out2 <- dbl_run(
    formula = formula <- rank | id ~ player(rider),
    data = gpheats[1:4, ],
    lambda = 1,
    weight = 1,
    init_r = 1,
    init_rd = 2
  )

  testthat::expect_true(all(out2$r$R == 1))
  testthat::expect_true(all(out2$r$RD == 2))
  testthat::expect_true(all(out2$pairs$P == 0.5))
})
