context("predict")

data <- data.frame(
  id = 1,
  name = c("A", "B", "C", "D"),
  rank = c(3, 4, 1, 2),
  field = 1:4,
  date = seq(Sys.Date() - 3, Sys.Date(), by = "1 day"),
  sigma = rep(0, 4),
  weight = rep(1.0, 4), date = c("a", "b", "c", "d")
)


test_that("valid glicko predict computation", {
  r <- setNames(rep(1500, 4), c("A", "B", "C", "D"))
  rd <- setNames(rep(350, 4), c("A", "B", "C", "D"))
  sigma <- setNames(rep(1, 4), c("A", "B", "C", "D"))
  data$rank <- 1
  
  glicko <- glicko_run(rank | id ~ player(name), 
                       data = data, 
                       r = r, 
                       rd = rd)
  
  expect_identical(glicko$final_r, r)
  expect_silent(pred <- predict(glicko, data))
  expect_true(all(pred$Y == .5))
  
  
  r  <- setNames(c(1500, 1400, 1550, 1700), c("A", "B", "C", "D"))
  rd <- setNames(c(200, 30, 100, 300), c("A", "B", "C", "D"))
  glicko <- glicko_run(rank | id ~ player(name), data = data, r = r, rd = rd)
  
  expect_identical(
    round(predict(glicko, data)$P, 3),
    c(0.634, 0.457, 0.412, 
      0.366, 0.315, 0.288, 
      0.543, 0.685, 0.449, 
      0.588, 0.712, 0.551)
  )
})

test_that("valid glicko2 predict computation", {
  r <- setNames(rep(1500, 4), c("A", "B", "C", "D"))
  rd <- setNames(rep(350, 4), c("A", "B", "C", "D"))
  sigma <- setNames(rep(1, 4), c("A", "B", "C", "D"))
  data$rank <- 1
  
  glicko2 <- glicko2_run(rank | id ~ player(name), 
                       data = data, 
                       r = r, 
                       rd = rd)
  
  expect_identical(glicko2$final_r, r)
  expect_silent(pred <- predict(glicko2, data))
  expect_true(all(pred$Y == .5))
  
  
  r  <- setNames(c(1500, 1400, 1550, 1700), c("A", "B", "C", "D"))
  rd <- setNames(c(200, 30, 100, 300), c("A", "B", "C", "D"))
  glicko2 <- glicko2_run(rank | id ~ player(name), data = data, r = r, rd = rd)
  
  expect_identical(
    round(predict(glicko2, data)$P, 3),
    c(0.703, 0.451, 0.536, 
      0.297, 0.251, 0.335,
      0.549, 0.749, 0.584, 
      0.464, 0.665, 0.416)
  )
})

test_that("valid bbt predict computation", {
  r <- setNames(rep(25.0, 4), c("A", "B", "C", "D"))
  rd <- setNames(rep(25 / 3, 4), c("A", "B", "C", "D"))
  sigma <- setNames(rep(1, 4), c("A", "B", "C", "D"))
  data$rank <- 1
  
  bbt <- bbt_run(rank | id ~ player(name), 
                data = data, 
                r = r, 
                rd = rd)
  
  expect_identical(bbt$final_r, r)
  expect_silent(pred <- predict(bbt, data))
  expect_true(all(pred$Y == .5))
  
  
  r = setNames(c(25.0, 20.0, 15.0, 30.0), c("A", "B", "C", "D"))
  rd = setNames(c(6.0, 7.0, 5.0, 20.0), c("A", "B", "C", "D"))
  bbt <- bbt_run(rank | id ~ player(name), data = data, r = r, rd = rd)
  
  expect_identical(
    round(predict(bbt, data)$P, 3),
    c(0.585, 0.698, 0.501, 
      0.415, 0.602, 0.450, 
      0.302, 0.398, 0.389, 
      0.499, 0.550, 0.611)
  )
})

test_that("valid dbl predict computation", {
  dbl <- dbl_run(rank | id ~ player(name), data = data)
  expect_silent(pred <- predict(dbl, data))
  expect_identical(
    round(pred$P, 3),
    c(0.624, 0.267, 0.376, 
      0.376, 0.180, 0.267, 
      0.733, 0.820, 0.624, 
      0.624, 0.733, 0.376)
  )
  
  data$rank <- 1
  dbl <- dbl_run(rank | id ~ player(name), 
                 data = data)
  
  expect_silent(pred <- predict(dbl, data))
  expect_true(all(pred$Y == .5))
})

test_that("New data consistent", {
  glicko <- glicko_run(rank | id ~ player(rider), data = gpheats[1:4,])
  expect_error(
    predict(glicko, newdata = data),
    "Variables \\(rider\\) are not present in newdata"
  )
  
  test_that("New data consistent", {
    glicko <- glicko_run(rank | id ~ player(rider), data = gpheats[1:4,])
    expect_warning(
      predict(glicko, newdata = gpheats[5:8,]),
      "Missing parameters will be added with init_rd"
    )
  })
  
})
