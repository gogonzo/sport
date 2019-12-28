context("terms")

test_that("get_type", {
  int <- as.integer(1:5)
  num <- as.numeric(0.5, 1.5, 2.5, 3.5)
  char <- letters
  fctr <- as.factor(letters)
  expect_identical(
    get_type(char),
    "character"
  )
  expect_identical(
    get_type(fctr),
    "character"
  )
  expect_identical(
    get_type(int),
    "numeric"
  )
  expect_identical(
    get_type(num),
    "numeric"
  )
  expect_null(
    get_type(as.complex(1))
  )
})

test_that("get terms", {
  term1 <- get_terms(
    gpheats,
    rank|id ~ round
  )
  expected1 <- list(c(round = "numeric"))
  expect_identical(term1, expected1)
  
  
  term2 <- get_terms(
    gpheats,
    rank|id ~ rider + round
  )
  expected2 <- list(c(rider = "character"), c(round = "numeric"))
  expect_identical(term2, expected2)
  
  
  term3 <- get_terms(
    gpheats,
    rank|id ~ rider + round + field:heat
  )
  expected3 <- list(c(rider = "character"),
                    c(round = "numeric"), 
                    c(field = "numeric", 
                      heat = "numeric"))
  expect_identical(term3, expected3)
  
  
  
  term <- get_terms(
    iris,
    rank|id ~ Sepal.Length:Sepal.Width + Petal.Length + Species + Species:Petal.Length
  )
  
  
})