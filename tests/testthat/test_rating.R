context("rating")

test_that("check arguments", {
  # is data provided
  # check formula errors
  # check init_r, init_rd, init_sigma
  # check if r, rd, sigma are > 0
  # check for missings and allowed values
  # check r, rd, sigma pointers (should be different)
})

test_that("output structure", {
  
})

test_that("glicko result",{
  expected_r  <- setNames(c(1464.297, 1396.039, 1606.521, 1674.836), c("A", "B", "C", "D"))
  expected_rd <- setNames(c(150.847, 29.800, 92.544, 186.326), c("A", "B", "C", "D"))
  
  cpp_glicko <- sport:::glicko(
    unique_id = 1L,
    id = c(1, 1, 1, 1),
    rank = c(3, 4, 1, 2),
    team = c("a", "b", "c", "d"),
    player = c("A", "B", "C", "D"),
    r  = setNames(c(1500.0, 1400.0, 1550.0, 1700.0), c("A", "B", "C", "D")), 
    rd = setNames(c(200.0,  30.0,   100.0,  300.0), c("A", "B", "C", "D")),
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
    tau = 0.5)
  
  r_glicko <- glicko_run(
    data = data.frame(
      id = c(1, 1, 1, 1),
      rank = c(3, 4, 1, 2),
      team = c("a", "b", "c", "d"),
      player = c("A", "B", "C", "D"),
      stringsAsFactors = FALSE
    ),
    rank ~ player,
    r  = setNames(c(1500.0, 1400.0, 1550.0, 1700.0), c("A", "B", "C", "D")), 
    rd = setNames(c(200.0,  30.0,   100.0,  300.0), c("A", "B", "C", "D"))
  )
  
  
  expect_identical(expected_r, round(cpp_glicko$final_r, 3))
  expect_identical(expected_r, round(r_glicko$final_r, 3))
  
  expect_identical(expected_rd, round(cpp_glicko$final_rd, 3))
  expect_identical(expected_rd, round(r_glicko$final_rd, 3))
})


test_that("bbt result", {
  out <- sport:::bbt(
    unique_id = 1,
    id     = c(1, 1, 1, 1),
    rank   = c(3, 4, 1, 2),
    team   = c("a", "b", "c", "d"),
    player = c("A", "B", "C", "D"),
    
    r      = setNames(c(25, 20, 15, 30), c("A", "B", "C", "D")), 
    rd     = setNames(c(6,  7,  5,  20), c("A", "B", "C", "D")),
    sigma  = numeric(0),
    
    lambda = c(1, 1, 1, 1),
    weight = c(1, 1, 1, 1),
    share  = c(1, 1, 1, 1),
    
    init_r  = 25,
    init_rd = 25 / 3,
    init_sigma = 0,
    
    beta = 25 / 6,
    gamma = 1.0,
    kappa = 0.5,
    tau = 0.5
  )
  
  out <- sport:::bbt2(
    unique_id = 1,
    id     = c(1, 1, 1, 1),
    rank   = c(3, 4, 1, 2),
    team   = c("a", "b", "c", "d"),
    player = c("A", "B", "C", "D"),
    r_val  = setNames(c(25, 20, 15, 30), c("A", "B", "C", "D")), 
    rd_val = setNames(c(6,  7,  5,  20), c("A", "B", "C", "D")),
    lambda = c(1, 1, 1, 1),
    weight = c(1, 1, 1, 1),
    share  = c(1, 1, 1, 1)
  )
  
  expect_identical(
    setNames(c(22.53, 14.07, 19.57, 32.69), c("A", "B", "C", "D")),
    round(out$final_r, 2)
  )
  
  expect_identical(
    setNames(c(5.98, 6.94, 4.99, 14.74), c("A", "B", "C", "D")),
    round(out$final_rd, 2)
  )
})
