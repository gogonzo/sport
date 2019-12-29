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
  expected2 <- list(c(rider = "character"), 
                    c(round = "numeric"))
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
  
  term4 <- get_terms(
    gpheats,
    rank|id ~ rider + round + field:heat + heat:field_f
  )
  expected4 <- list(c(rider = "character"),
                    c(round = "numeric"), 
                    c(field = "numeric", 
                      heat = "numeric"),
                    c(field_f = "character",
                      heat = "numeric"))
  expect_identical(term4, expected4)
  
  
  term5 <- get_terms(
    gpheats,
    rank|id ~ rider + round + field*heat
  )
  expected5 <- list(c(rider = "character"),
                    c(round = "numeric"),
                    c(field = "numeric"),
                    c(heat = "numeric"),
                    c(field = "numeric", 
                      heat = "numeric"))
  expect_identical(term5, expected5)
  
  
  expect_error(
    get_terms(
      gpheats,
      rank|id ~ rider + round + field:heat + heat:field_f + unknown
    ),
    "Variable\\(s\\) .+ specified in formula are missing in data"
  )
  
  expect_error(
    get_terms(
      gpheats,
      rank|id ~ rider + round + field:heat + heat:field_f:unknown
    ),
    "Variable\\(s\\) .+ specified in formula are missing in data"
  )
  
  expect_error(
    get_terms(
      gpheats,
      rank|id ~ team(rider) + round + field:heat + heat:field_f:unknown
    ),
    "Specifying team\\([^)]+\\) in dbl_run is not possible"
  )
  
})

test_that("get terms map", {
  term1 <- get_terms(
    gpheats,
    rank|id ~ round
  )
  terms_map1 <- get_terms_map(gpheats[1:6,], term1)
  expected1 <- as.matrix(
    data.frame(
      round = rep("round", 6)
    )
  )
  expect_identical(terms_map1, expected1)  
  
  
  term2 <- get_terms(
    gpheats,
    rank|id ~ rider + round
  )
  terms_map2 <- get_terms_map(gpheats[1:6,], term2)
  expected2 <- as.matrix(
    data.frame(
      rider = paste0("rider=", gpheats$rider[1:6]),
      round = rep("round", 6)
    )
  )
  expect_identical(terms_map2, expected2)  
  
  
  term4 <- get_terms(
    gpheats,
    rank|id ~ rider + round + field:heat + heat:rider
  )
  terms_map4 <- get_terms_map(gpheats[1:6,], term4)
  expected4 <- as.matrix(
    data.frame(
      rider = paste0("rider=", gpheats$rider[1:6]),
      round = rep("round", 6),
      `field*heat` = rep("field*heat", 6),
      `rider|heat` = paste0("rider=", gpheats$rider[1:6],":heat"),
      check.names = FALSE
    )
  )
  expect_identical(terms_map4, expected4)  
  
  
  term5 <- get_terms(
    gpheats,
    rank|id ~ rider + round + field:heat + name:rider
  )
  terms_map5 <- get_terms_map(gpheats[1:6,], term5)
  expected5 <- as.matrix(
    data.frame(
      rider = paste0("rider=", gpheats$rider[1:6]),
      round = rep("round", 6),
      `field*heat` = rep("field*heat", 6),
      `rider|name` = paste0("rider=", gpheats$rider[1:6],":name=", gpheats$name[1:6]),
      check.names = FALSE
    )
  )
  expect_identical(terms_map5, expected5)  
})

test_that("get terms mat", {
  term1 <- get_terms(
    gpheats,
    rank|id ~ round
  )
  terms_mat1 <- get_terms_mat(gpheats[1:6,], term1)
  expected1 <- as.matrix(
    data.frame(
      round = gpheats[1:6, "round"]
    )
  )
  expect_identical(terms_mat1, expected1)  
  
  
  term2 <- get_terms(
    gpheats,
    rank|id ~ rider + round
  )
  terms_mat2 <- get_terms_mat(gpheats[1:6,], term2)
  expected2 <- as.matrix(
    data.frame(
      rider = rep(1, 6),
      round = data[1:6, "round"]
    )
  )
  expect_identical(terms_mat2, expected2)  
  
  
  term4 <- get_terms(
    gpheats,
    rank|id ~ rider + round + field:heat + heat:rider
  )
  terms_mat4 <- get_terms_mat(gpheats[1:6,], term4)
  expected4 <- as.matrix(
    data.frame(
      rider = rep(1, 6),
      round = data[1:6, "round"],
      `field*heat` = data[1:6, "field"] * data[1:6, "heat"],
      `rider|heat` = data[1:6, "heat"],
      check.names = FALSE,
      row.names = 1:6
    )
  )
  expect_identical(terms_mat4, expected4)  
  
  
  term5 <- get_terms(
    gpheats,
    rank|id ~ rider + round + field:heat + name:rider
  )
  terms_mat5 <- get_terms_mat(gpheats[1:6,], term5)
  expected5 <- as.matrix(
    data.frame(
      rider = rep(1, 6),
      round = data[1:6, "round"],
      `field*heat` = data[1:6, "field"] * data[1:6, "heat"],
      `rider|name` = rep(1, 6),
      check.names = FALSE,
      row.names = 1:6
    )
  )
  expect_identical(terms_mat5, expected5)
})


test_that("get terms cls", {
  term1 <- get_terms(
    gpheats,
    rank|id ~ round
  )
  terms_cls1 <- get_terms_cls(gpheats[1:6,], term1)
  expected1 <- "numeric"
  expect_identical(terms_cls1, expected1)  
  
  
  term2 <- get_terms(
    gpheats,
    rank|id ~ rider + round
  )
  terms_cls2 <- get_terms_cls(gpheats[1:6,], term2)
  expected2 <- c("character", "numeric") 
  expect_identical(terms_cls2, expected2)  
  
  
  term4 <- get_terms(
    gpheats,
    rank|id ~ rider + round + field:heat + heat:rider
  )
  terms_cls4 <- get_terms_cls(gpheats[1:6,], term4)
  expected4 <- c("character", "numeric", "numeric", "character")
  expect_identical(terms_cls4, expected4)  
  
  
  term5 <- get_terms(
    gpheats,
    rank|id ~ rider + round + field:heat + name:rider
  )
  terms_cls5 <- get_terms_cls(gpheats[1:6,], term5)
  expected5 <- c("character", "numeric", "numeric", "character")
  expect_identical(terms_cls5, expected5)
})