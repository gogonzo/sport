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
      formula = rank | id ~ player(player | team),
      r = NULL,
      rd = NULL,
      init_r = 1500,
      init_rd = 350,
      lambda = "lambda",
      share = "share",
      weight = "weight",
      kappa = 0.5
    )
  )
  
  expect_silent(
    g2 <- rating_run(
      method = "glicko",
      data = df,
      formula = rank | id ~ player(player | team),
      r = NULL,
      rd = NULL,
      init_r = 1500,
      init_rd = 350,
      lambda = NULL,
      share = NULL,
      weight = NULL,
      kappa = 0.5
    )
  )
  
  expect_silent(
    g3 <- rating_run(
      method = "glicko",
      data = df,
      formula = rank | id ~ player(player | team),
      init_r = 1500,
      init_rd = 350,
      kappa = 0.5
    )
  )
  
  expect_silent(
    g4 <- glicko_run(
      data = df,
      formula = rank | id ~ player(player | team)
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
      formula = rank | id ~ player(player | team),
      init_r = 1500,
      init_rd = 350,
      kappa = 0.5
    )
  )
  
  expect_silent(
    g6 <- rating_run(
      method = "glicko",
      data = df,
      formula = rank | id ~ player(player | team),
      init_r = 1500,
      init_rd = 350
    )
  )
  
  expect_identical(g5, g6)
  
  expect_silent(
    g7 <- rating_run(
      method = "glicko",
      data = df,
      formula = rank | id ~ player(player | team),
      init_r = 1500,
      init_rd = 350
    )
  )
  
  expect_silent(
    g8 <- rating_run(
      method = "glicko",
      data = df,
      formula = rank | id ~ player(player | team),
      init_r = 1500,
      init_rd = 350
    )
  )
  
  expect_identical(g7, g8)
  
  expect_silent(
    g9 <- rating_run(
      method = "glicko2",
      data = df,
      formula = rank | id ~ player(player | team),
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
      formula = rank | id ~ player(player | team)
    )
  )
  
  expect_identical(g9$final_r, g10$final_r)
  expect_identical(g9$final_rd, g10$final_rd)
  expect_identical(g9$final_sigma, g10$final_sigma)
  
  
  expect_silent(
    g11 <- rating_run(
      method = "bbt",
      data = df,
      formula = rank | id ~ player(player | team),
      init_r = 25.0,
      init_rd = 25 / 3,
      kappa = 0.5
    )
  )
  
  expect_silent(
    g12 <- bbt_run(
      data = df,
      formula = rank | id ~ player(player | team)
    )
  )
  
  expect_identical(g11$final_r, g12$final_r)
  expect_identical(g11$final_rd, g12$final_rd)
  
  
  expect_warning(
    glicko_run(
      data = df,
      formula = rank | id ~ player(player | team),
      r = setNames(.5, "c")
    ),
    "Missing parameters will be added with init_r"
  )
  
  expect_warning(
    glicko_run(
      data = df,
      formula = rank | id ~ player(player | team),
      rd = setNames(.5, "c")
    ),
    "Missing parameters will be added with init_rd"
  )
  
  expect_warning(
    glicko2_run(
      data = df,
      formula = rank | id ~ player(player | team),
      sigma = setNames(.5, "c")
    ),
    "Missing parameters will be added with init_sigma"
  )
  
  expect_warning(
    glicko2_run(
      data = df,
      formula = rank | id ~ player(player | team),
      sigma = setNames(.5, "unknown")
    ),
    "Missing parameters will be added with init_sigma"
  )
  
  expect_warning(
    glicko_run(
      data = df,
      formula = rank | id ~ player(player | team),
      r = setNames(.5, "c"),
      rd = setNames(0, "f")
    ),
    "init_rd"
  )
  
  expect_warning(
    glicko <- glicko_run(
      data = df,
      formula = rank | id ~ player(player | team),
      r = setNames(.5, "c"),
      rd = setNames(0, "ff")
    ),
    "init_r"
  )
 
   
  expect_true(
    !"ff" %in% glicko$final_rd
  )
  
  
  glicko <- glicko_run(rank | id ~ player(rider), data = gpheats[1:4,])
  expect_warning(
    glicko_run(
      formula = rank | id ~ player(rider),
      data = gpheats[5:8,],
      r = glicko$final_r,
      rd = glicko$final_rd
    ),
    "Missing parameters will be added with init"
  )
})

test_that("rating (argument) errors", {
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
  
  expect_error(
    glicko_run(data = df, formula = rank | id ~ player),
    "Formula requires specifying player\\(...\\) term"
  )
  
  # stats::terms need also data if `.` specified
  expect_error(glicko_run(data = df, formula = rank | id ~ .),
               "in formula and no 'data' argument") 
  
  expect_error(glicko_run(data = df, formula = rank | id ~ 1),
               "Formula requires specifying player\\(...\\) term")
  
  expect_error(glicko_run(data = df, formula = rank | id ~ player(wrong)),
               "Variable\\(s\\) wrong specified in formula not present in data")
  
  expect_silent(glicko_run(data = df, formula = rank | id ~ player(player)))
  expect_silent(glicko_run(data = df, formula = rank | id ~ player(player | team)))
  expect_error(dbl_run(data = df, formula = rank | id ~ player(player | team)),
               "Please specify only one variable inside of the player")
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
      rank ~ player(player),
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
    kappa = 0.5,
    tau = 0.5
  )
  
  r_glicko2 <- glicko2_run(
    formula = rank | id ~ player(player | team),
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
    
    kappa = 0.5,
    tau = 0.5
  )
  
  r_bbt <- bbt_run(
    formula = rank | id ~ player(player | team),
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
    setNames(c(22.50, 14.03, 19.61, 32.68), c("A", "B", "C", "D")),
    round(cpp_bbt$final_r, 2)
  )
  
  expect_identical(
    setNames(c(5.98, 6.94, 4.99, 14.71), c("A", "B", "C", "D")),
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

test_that("dbl result", {
  r_dbl <- dbl_run(
    formula = rank | id ~ player(player) + gate:factor,
    data = data.frame(
      id = c(1, 1, 1, 1),
      rank = c(3, 4, 1, 2),
      team = c("a", "b", "c", "d"),
      gate = c(1, 2, 3, 4),
      factor = c("a", "a", "b", "b"),
      factor2 = c("a", "b", "a", "b"),
      player = c("A", "B", "C", "D"),
      lambda = c(1, 1, 1, 1),
      weight = c(1, 1, 1, 1),
      share = c(1, 1, 1, 1)
    )
  )
  
  expect_identical(
    round(r_dbl$final_r, 2),
    setNames(
      c(-0.02, -0.18,  0.15,  0.05, -0.38,  0.64),
      c("player=A", "player=B", "player=C", "player=D", "factor=a:gate", "factor=b:gate")
    )
  )
  
  expect_identical(
    round(r_dbl$final_rd, 2),
    setNames(
      c(0.95, 0.95, 0.95, 0.95, 0.90, 0.90),
      c("player=A", "player=B", "player=C", "player=D", "factor=a:gate", "factor=b:gate")
    )
  )
})

test_that("Reasonable estimates", {
  glicko <- glicko_run(rank | id ~ player(rider), data = gpheats[1:10000,])
  glicko2 <- glicko2_run(rank | id ~ player(rider), data = gpheats[1:10000,])
  bbt <- bbt_run(rank | id ~ player(rider), data = gpheats[1:10000,])
  dbl <- dbl_run(rank | id ~ player(rider), data = gpheats[1:10000,])

  library(dplyr)
  by_rank <- gpheats %>%
    head(10000) %>%
    group_by(rider) %>%
    summarize(mean_rank = mean(rank, na.rm = TRUE)) %>%
    ungroup %>%
    arrange(mean_rank)
  
  worst <- unique(c(
    names(sort(glicko$final_r)[1:10]),
    names(sort(glicko2$final_r)[1:10]),
    names(sort(bbt$final_r)[1:10]),
    gsub("rider\\=", "", x = names(sort(dbl$final_r)[1:10])),
    tail(by_rank$rider, 10)
  ))
  
  best <- unique(c(
    names(sort(glicko$final_r, decreasing = TRUE)[1:10]),
    names(sort(glicko2$final_r, decreasing = TRUE)[1:10]),
    names(sort(bbt$final_r, decreasing = TRUE)[1:10]),
    gsub("rider\\=", "", x = names(sort(dbl$final_r, decreasing = TRUE)[1:10]))),
    head(by_rank$rider, 10)
  )
  

  expect_true(all(!worst %in% best))
  expect_true(all(!best %in% worst))
})

test_that("Weighting", {
  data <- data.frame(
    id = rep(1L, 8),
    rank = rep(c(3, 4, 1, 2), each = 2),
    team = c("A", "A", "B", "B", "C", "C", "D", "D"),
    player = sample(letters, 8, replace = FALSE),
    lambda = 1,
    share = 1,
    weight = 2,
    stringsAsFactors = FALSE
  )

  bbt1 <- bbt_run(rank | id ~ player(player),
                  data = data)  
  
  bbt2 <- bbt_run(rank | id ~ player(player),
                  data = data,
                  weight = "weight")
  
  expect_equal(
    abs(bbt2$final_r - 25),   
    abs(bbt1$final_r - 25) * 2
  )
  
  expect_equal(
    abs(bbt2$final_rd - 25/3),   
    abs(bbt1$final_rd - 25/3) * 2
  )
})

test_that("Lambda", {
  data <- data.frame(
    id = rep(1L, 8),
    rank = rep(c(3, 4, 1, 2), each = 2),
    team = c("A", "A", "B", "B", "C", "C", "D", "D"),
    player = sample(letters, 8, replace = FALSE),
    lambda = 2,
    share = 1,
    weight = 2,
    stringsAsFactors = FALSE
  )
  
  glicko1 <- glicko_run(rank | id ~ player(player),
                  data = data)  
  
  glicko2 <- glicko_run(rank | id ~ player(player),
                  data = data,
                  lambda = "lambda")
  
  expect_true(
    all(
      abs(glicko2$final_r - 1500) <    
      abs(glicko1$final_r - 1500)
    )
  )
  
  expect_true(
    all(
      abs(glicko2$final_rd - 350) <    
        abs(glicko1$final_rd - 350)
    )
  )
})

test_that("kappa", {
  glicko1 <- glicko2_run(rank | id ~ player(player), 
                         data = df,
                         kappa = 1)  

  expect_true(all(
    glicko1$final_rd == 350    
    )
  )
  
  
  glicko2 <- glicko_run(rank | id ~ player(player),
                        data = df,
                        kappa = 0.99)
  

  expect_true(all(
    glicko2$final_rd == (350 * 0.99)    
    )
  )
  
  glicko3 <- bbt_run(rank | id ~ player(player),
                     data = df,
                     kappa = 0.98)
  
  expect_true(all(
    glicko3$final_rd ==  (25 / 3 * 0.98)
  )
  )
})


test_that("share", {
  df <- data.frame(
    id = rep(1L, 8),
    rank = rep(c(3, 4, 1, 2), each = 2),
    team = c("A", "A", "B", "B", "C", "C", "D", "D"),
    player = letters[1:8],
    contribution = c(0.9, 0.1,
              0.1, 0.9,
              0.5, 0.5,
              1, 0),
    stringsAsFactors = FALSE
  )

  x <- glicko_run(rank | id ~ player(player|team),
                  data = df,
                  share = "contribution")  
  
  expect_true(
    all(
      (x$final_r[1:4] - 1500) < 0
    )
  )
  
  expect_true(
    all(
      (x$final_r[5:8] - 1500) >= 0
    )
  )
  
  
  expect_equal(
    (1500 - x$final_r[1:2]) / sum((1500 - x$final_r[1:2])),
    setNames(c(0.9, 0.1), c("a", "b"))
  )
  
  expect_equal(
    (1500 - x$final_r[3:4]) / sum((1500 - x$final_r[3:4])),
    setNames(c(0.1, 0.9), c("c", "d"))
  )
  
  expect_equal(
    (x$final_r[5:6] - 1500) / sum((x$final_r[5:6] - 1500)),
    setNames(c(0.5, 0.5), c("e", "f"))
  )
})
