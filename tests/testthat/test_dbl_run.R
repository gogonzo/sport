context("dbl_run")

test_that("wrong formula", {
  expect_error(
    dbl_run(
      formula <- rank | id ~ player(rider|team),
      data = gpheats[1:8, ]
    ),
    "Please specify only one variable inside of the player"
  )
  
  expect_error(
    dbl_run(
      formula <- rank + id ~ player(rider),
      data = gpheats[1:8, ]
    ),
    "LHS"
  )
  
  expect_error(
    dbl_run(
      formula <- rank | id ~ rider,
      data = gpheats[1:8, ]
    ),
    "Formula requires specifying player"
  )
  
  expect_warning(
    dbl_run(
      formula <- rank ~ player(rider),
      data = gpheats[1:8, ]
    ),
    "LHS"
  )
  
  expect_error(
    dbl_run(
      formula <- rank | idd ~ rider,
      data = gpheats[1:8, ]
    ),
    "idd specified in formula are not present in data"
  )
  
  expect_silent(
    dbl_run(
      formula <- rank | id ~ player(rider),
      data = gpheats[1:8, ]
    )
  )
  
  
  expect_silent(
    dbl_run(
      formula <- rank | id ~ player(rider) + field,
      data = gpheats[1:8, ]
    )
  )
  
  expect_silent(
    dbl_run(
      formula <- rank | id ~ player(rider) + field*heat + heat:rider,
      data = gpheats[1:8, ]
    )
  )
})

test_that("initial",{
  expect_warning(
    dbl_run(
      formula <- rank | id ~ player(rider),
      data = gpheats[1:8, ],
      r = setNames(.5, "rider=Tomasz GOLLOB")),
    "Missing parameters will be added with init_r"
  )
  
  expect_warning(
    dbl_run(
      formula <- rank | id ~ player(rider),
      data = gpheats[1:8, ],
      rd = setNames(.5, "rider=Tomasz GOLLOB")),
    "Missing parameters will be added with init_rd"
  )

  
})

test_that("output", {
  out1 <- dbl_run(
    formula = formula <- rank | id ~ player(rider),
    data = gpheats[1:4, ],
    lambda = 1,
    weight = 1,
    init_r = 0,
    init_rd = 1
  )
  
  expect_identical(
    out1$final_r,
    setNames(
      c(1/3, -1, 1, -1/3),
      c("rider=Tomasz GOLLOB", "rider=Gary HAVELOCK", "rider=Chris LOUIS", "rider=Tony RICKARDSSON")
    )
  )
  
  expect_true(all(out1$r$R == 0))
  expect_true(all(out1$r$RD == 1))
  expect_true(all(out1$pairs$P == 0.5))
  
  
  out2 <- dbl_run(
    formula = formula <- rank | id ~ player(rider),
    data = gpheats[1:4, ],
    lambda = 1,
    weight = 1,
    init_r = 1,
    init_rd = 2
  )
  
  expect_true(all(out2$r$R == 1))
  expect_true(all(out2$r$RD == 2))
  expect_true(all(out2$pairs$P == 0.5))
})

