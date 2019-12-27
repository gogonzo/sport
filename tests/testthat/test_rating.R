context("rating")
df <- data.frame(
  id = rep(1L, 8),
  rank = rep(c(3, 4, 1, 2), each = 2),
  team = c("A", "A", "B", "B", "C", "C", "D", "D"),
  player = sample(letters, 8, replace = FALSE),
  lambda = 1,
  share = 1,
  weight = 1,
  stringsAsFactors = FALSE
)


test_that("check rating default arguments", {
  expect_silent(
    g1 <- rating_run(
      method = "glicko",
      data = df,
      formula = rank | id ~ nest(player | team),
      r = NULL,
      rd = NULL,
      init_r = 1500,
      init_rd = 350,
      lambda = "lambda",
      share = "share",
      weight = "weight",
      kappa = 0.5,
      gamma = 1
    )
  )

  expect_silent(
    g2 <- rating_run(
      method = "glicko",
      data = df,
      formula = rank | id ~ nest(player | team),
      r = NULL,
      rd = NULL,
      init_r = 1500,
      init_rd = 350,
      lambda = NULL,
      share = NULL,
      weight = NULL,
      kappa = 0.5,
      gamma = 1
    )
  )

  expect_silent(
    g3 <- rating_run(
      method = "glicko",
      data = df,
      formula = rank | id ~ nest(player | team),
      init_r = 1500,
      init_rd = 350,
      kappa = 0.5,
      gamma = 1
    )
  )

  expect_silent(
    g4 <- glicko_run(
      data = df,
      formula = rank | id ~ nest(player | team)
    )
  )

  expect_identical(g1, g2)
  expect_identical(g2, g3)
  expect_identical(g3$final_r, g4$final_r)
  expect_identical(g3$final_rd, g4$final_rd)

  expect_silent(
    g5 <- rating_run(
      method = "glicko",
      data = df,
      formula = rank | id ~ nest(player | team),
      init_r = 1500,
      init_rd = 350,
      kappa = 0.5
    )
  )

  expect_silent(
    g6 <- rating_run(
      method = "glicko",
      data = df,
      formula = rank | id ~ nest(player | team),
      init_r = 1500,
      init_rd = 350
    )
  )

  expect_identical(g5, g6)

  expect_silent(
    g7 <- rating_run(
      method = "glicko",
      data = df,
      formula = rank | id ~ nest(player | team),
      init_r = 1500,
      init_rd = 350,
      gamma = 1
    )
  )

  expect_silent(
    g8 <- rating_run(
      method = "glicko",
      data = df,
      formula = rank | id ~ nest(player | team),
      init_r = 1500,
      init_rd = 350
    )
  )

  expect_identical(g7, g8)

  expect_silent(
    g9 <- rating_run(
      method = "glicko2",
      data = df,
      formula = rank | id ~ nest(player | team),
      init_r = 1500,
      init_rd = 350,
      init_sigma = 0.05,
      kappa = 0.5,
      tau = 0.5
    )
  )

  expect_silent(
    g10 <- glicko2_run(
      data = df,
      formula = rank | id ~ nest(player | team)
    )
  )

  expect_identical(g9$final_r, g10$final_r)
  expect_identical(g9$final_rd, g10$final_rd)
  expect_identical(g9$final_sigma, g10$final_sigma)


  expect_silent(
    g11 <- rating_run(
      method = "bbt",
      data = df,
      formula = rank | id ~ nest(player | team),
      init_r = 25.0,
      init_rd = 25 / 3,
      beta = 25 / 6,
      kappa = 0.5
    )
  )

  expect_silent(
    g12 <- bbt_run(
      data = df,
      formula = rank | id ~ nest(player | team)
    )
  )

  expect_identical(g11$final_r, g12$final_r)
  expect_identical(g11$final_rd, g12$final_rd)
})

test_that("rating errors", {
  expect_error(
    glicko_run(formula = rank ~ player),
    "Data is not provided"
  )

  # check formula errors
  expect_error(
    glicko_run(data = df, formula = rank:id ~ player),
    "LHS"
  )

  expect_error(
    glicko_run(data = df, formula = rank | id + test ~ player),
    "LHS"
  )

  expect_error(glicko_run(data = df, formula = rank | id ~ .))

  expect_error(glicko_run(data = df, formula = rank | id ~ nest(team)))

  expect_silent(glicko_run(data = df, formula = rank | id ~ nest(player | team)))
})

test_that("glicko result", {
  expected_r <- setNames(c(1464.297, 1396.039, 1606.521, 1674.836), c("A", "B", "C", "D"))
  expected_rd <- setNames(c(150.847, 29.800, 92.544, 186.326), c("A", "B", "C", "D"))

  cpp_glicko <- sport:::glicko(
    unique_id = 1L,
    id = c(1, 1, 1, 1),
    rank = c(3, 4, 1, 2),
    team = c("a", "b", "c", "d"),
    player = c("A", "B", "C", "D"),
    r = setNames(c(1500.0, 1400.0, 1550.0, 1700.0), c("A", "B", "C", "D")),
    rd = setNames(c(200.0, 30.0, 100.0, 300.0), c("A", "B", "C", "D")),
    sigma = numeric(0),
    share = c(1, 1, 1, 1),
    lambda = c(1, 1, 1, 1),
    weight = c(1, 1, 1, 1),
    init_r = 1500.0,
    init_rd = 350.0,
    init_sigma = 0,
    beta = 25 / 6,
    gamma = 1.0,
    kappa = 0.5,
    tau = 0.5
  )

  expect_warning(
    r_glicko <- glicko_run(
      data = data.frame(
        id = c(1, 1, 1, 1),
        rank = c(3, 4, 1, 2),
        team = c("a", "b", "c", "d"),
        player = c("A", "B", "C", "D"),
        stringsAsFactors = FALSE
      ),
      rank ~ player,
      r = setNames(c(1500.0, 1400.0, 1550.0, 1700.0), c("A", "B", "C", "D")),
      rd = setNames(c(200.0, 30.0, 100.0, 300.0), c("A", "B", "C", "D"))
    )
  )


  expect_identical(expected_r, round(cpp_glicko$final_r, 3))
  expect_identical(expected_r, round(r_glicko$final_r, 3))

  expect_identical(expected_rd, round(cpp_glicko$final_rd, 3))
  expect_identical(expected_rd, round(r_glicko$final_rd, 3))
})

test_that("glicko2 result", {
  cpp_glicko2 <- sport:::glicko2(
    unique_id = 1L,
    id = c(1, 1, 1, 1),
    rank = c(3, 4, 1, 2),
    team = c("a", "b", "c", "d"),
    player = c("A", "B", "C", "D"),
    r = setNames(c(1500.0, 1400.0, 1550.0, 1700.0), c("A", "B", "C", "D")),
    rd = setNames(c(200.0, 30.0, 100.0, 300.0), c("A", "B", "C", "D")),
    sigma = setNames(c(0.05, 0.05, 0.05, 0.05), c("A", "B", "C", "D")),
    share = c(1, 1, 1, 1),
    lambda = c(1, 1, 1, 1),
    weight = c(1, 1, 1, 1),
    init_r = 1500.0,
    init_rd = 350.0,
    init_sigma = 0.05,
    beta = 25 / 6,
    gamma = 1.0,
    kappa = 0.5,
    tau = 0.5
  )

  r_glicko2 <- glicko2_run(
    formula = rank | id ~ nest(player | team),
    data = data.frame(
      id = c(1, 1, 1, 1),
      rank = c(3, 4, 1, 2),
      team = c("a", "b", "c", "d"),
      player = c("A", "B", "C", "D"),
      share = c(1, 1, 1, 1),
      lambda = c(1, 1, 1, 1),
      weight = c(1, 1, 1, 1)
    ),
    r = setNames(c(1500.0, 1400.0, 1550.0, 1700.0), c("A", "B", "C", "D")),
    rd = setNames(c(200.0, 30.0, 100.0, 300.0), c("A", "B", "C", "D")),
    sigma = setNames(c(0.05, 0.05, 0.05, 0.05), c("A", "B", "C", "D")),
    init_r = 1500.0,
    init_rd = 350.0,
    init_sigma = 0.05,
    kappa = 0.5,
    tau = 0.5
  )

  expect_identical(
    setNames(c(1469, 1397, 1606, 1601), c("A", "B", "C", "D")),
    round(cpp_glicko2$final_r)
  )

  expect_identical(
    setNames(c(154, 31, 94, 204), c("A", "B", "C", "D")),
    round(cpp_glicko2$final_rd)
  )

  expect_identical(
    setNames(c(0.05, 0.05, 0.05, 0.05), c("A", "B", "C", "D")),
    round(cpp_glicko2$final_sigma, 2)
  )

  expect_identical(
    cpp_glicko2$final_r,
    r_glicko2$final_r
  )

  expect_identical(
    cpp_glicko2$final_rd,
    r_glicko2$final_rd
  )

  expect_identical(
    cpp_glicko2$final_sigma,
    r_glicko2$final_sigma
  )
})

test_that("bbt result", {
  cpp_bbt <- sport:::bbt(
    unique_id = 1,
    id = c(1, 1, 1, 1),
    rank = c(3, 4, 1, 2),
    team = c("a", "b", "c", "d"),
    player = c("A", "B", "C", "D"),

    r = setNames(c(25.0, 20.0, 15.0, 30.0), c("A", "B", "C", "D")),
    rd = setNames(c(6.0, 7.0, 5.0, 20.0), c("A", "B", "C", "D")),
    sigma = numeric(0),

    lambda = c(1, 1, 1, 1),
    weight = c(1, 1, 1, 1),
    share = c(1, 1, 1, 1),

    init_r = 25,
    init_rd = 25 / 3,
    init_sigma = 0,

    beta = 25 / 6,
    gamma = 1.0,
    kappa = 0.5,
    tau = 0.5
  )

  r_bbt <- bbt_run(
    formula = rank | id ~ nest(player | team),
    data = data.frame(
      id = c(1, 1, 1, 1),
      rank = c(3, 4, 1, 2),
      team = c("a", "b", "c", "d"),
      player = c("A", "B", "C", "D"),
      lambda = c(1, 1, 1, 1),
      weight = c(1, 1, 1, 1),
      share = c(1, 1, 1, 1)
    ),
    r = setNames(c(25.0, 20.0, 15.0, 30.0), c("A", "B", "C", "D")),
    rd = setNames(c(6.0, 7.0, 5.0, 20.0), c("A", "B", "C", "D"))
  )

  expect_identical(
    setNames(c(22.53, 14.07, 19.57, 32.69), c("A", "B", "C", "D")),
    round(cpp_bbt$final_r, 2)
  )

  expect_identical(
    setNames(c(5.98, 6.94, 4.99, 14.74), c("A", "B", "C", "D")),
    round(cpp_bbt$final_rd, 2)
  )

  expect_identical(
    cpp_bbt$final_r,
    r_bbt$final_r
  )

  expect_identical(
    cpp_bbt$final_rd,
    r_bbt$final_rd
  )
})
