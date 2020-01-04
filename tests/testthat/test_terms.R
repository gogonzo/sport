context("terms")

test_that("valid lhs formula", {
  expect_error(
    is_formula_missing(NULL),
    "Formula is not specified"
  )
  
  expect_error(
    is_formula_missing("y ~ x"),
    "Formula incorrectly specified"
  )
  
  expect_error(
    is_lhs_valid(formula = rank:id ~ field, gpheats),
    "LHS of formula must be seperated by `\\|` operator eg."
  )
  
  expect_error(
    is_lhs_valid(formula = rank + id ~ field, gpheats),
    "LHS of formula must be seperated by `\\|` operator eg."
  )
  
  expect_warning(
    is_lhs_valid(formula = rank ~ field, gpheats),
    "all belongs to the same event id"
  )
  
  expect_error(
    is_lhs_valid(formula = rank | wrong ~ field, gpheats),
    "Variable\\(s\\) wrong specified in formula are not present in data"
  )
  
  expect_silent(is_lhs_valid(formula = rank | field ~ rider, gpheats))
  
  expect_silent(is_lhs_valid(formula = rank | id ~ player(rider | name), gpheats))
})

test_that("check team term", {
  expect_identical(
    extract_team_terms(formula = rank + id ~ player(rider)),
    "rider"
  )
  
  expect_identical(
    extract_team_terms(formula = rank + id ~ player(rider|name)),
    c("rider", "name")
  )
  
  expect_error(
    extract_team_terms(formula = rank + id ~ player(rider|name|elo)),
    "Only one or two variables are allowed within player"
  )
  
})

test_that("valid rhs", {
  expect_error(
    is_rhs_valid(1 ~ 1, gpheats, only_team_term = TRUE, single = FALSE),
    "Formula requires specifying player"
  )
  
  expect_error(
    is_rhs_valid(1 ~ ., gpheats, only_team_term = TRUE, single = FALSE),
    "'.' in formula and no 'data' argument"
  )
  
  expect_error(
    is_rhs_valid(1 ~ rider, gpheats, only_team_term = TRUE, single = FALSE),
    "Formula requires specifying player"
  )
  
  expect_error(
    is_rhs_valid(1 ~ player() + field, gpheats, only_team_term = TRUE, single = FALSE),
    "This formula requires only one RHS term which is player"
  )
  
  expect_error(
    is_rhs_valid(1 ~ rider, gpheats, only_team_term = FALSE, single = TRUE),
    "Formula requires specifying player"
  )
  
  expect_silent(
    is_rhs_valid(1 ~ player(rider), gpheats, only_team_term = FALSE, single = TRUE)
  )
  
  expect_silent(
    is_rhs_valid(1 ~ player(rider), gpheats, only_team_term = FALSE, single = FALSE)
  )
  
  expect_error(
    is_rhs_valid(1 ~ player(rider | team), gpheats, only_team_term = FALSE, single = TRUE),
    "Please specify only one variable inside of the player"
  )
  
  expect_error(
    is_rhs_valid(1 ~ player(rider | team | elo), gpheats, only_team_term = FALSE, single = FALSE),
    "Only one or two variables are allowed within player"
  )
  
  expect_error(
    is_rhs_valid(1 ~ player(rider | team) + field, gpheats, only_team_term = TRUE, single = FALSE),
    "This formula requires only one RHS term which is player"
  )
  
  expect_error(
    is_rhs_valid(1 ~ player(rider | team) + field, gpheats, only_team_term = FALSE, single = FALSE),
    "team specified in formula not present in data"
  )
  
  expect_silent(
    is_rhs_valid(1 ~ player(rider | name) + field, gpheats, only_team_term = FALSE, single = FALSE)
  )
  
})

test_that("valid team term", {
  expect_silent(
    is_team_term_valid(formula = 1 ~ player(player), single = TRUE)
  )  
  
  expect_silent(
    is_team_term_valid(formula = 1 ~ player(player), single = FALSE)
  )
  
  expect_silent(
    is_team_term_valid(formula = 1 ~ player(player | team), single = FALSE)
  )
  
  
  
  expect_error(
    is_team_term_valid(formula = 1 ~ player(), single = TRUE),
    "Formula requires specifying player"      
  )
  
  expect_error(
    is_team_term_valid(formula = 1 ~ player(player | team), single = TRUE),
    "Please specify only one variable inside of the player"
  )
  
  expect_error(
    is_team_term_valid(formula = 1 ~ player(player | team | country), single = FALSE),
    "Only one or two variables are allowed within player"
  )
  
  expect_error(
    is_team_term_valid(formula = 1 ~ player(player) + player(team), single = FALSE),
    "Only one player\\(...\\) term is allowed with one or two variables."
  )
  
})

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
  
  gpheats$field_f <- as.factor(gpheats$field)
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
  
  term6 <- get_terms(
    gpheats,
    rank|id ~ player(rider) + round + field*heat
  )
  expected6 <- list(c(rider = "character"),
                    c(round = "numeric"),
                    c(field = "numeric"),
                    c(heat = "numeric"),
                    c(field = "numeric", 
                      heat = "numeric"))
  expect_identical(term6, expected6)
  
  
  term7 <- get_terms(
    gpheats,
    rank|id ~ round + field*heat + player(rider)
  )
  expected7 <- list(c(rider = "character"),
                    c(round = "numeric"),
                    c(field = "numeric"),
                    c(heat = "numeric"),
                    c(field = "numeric", 
                      heat = "numeric"))
  expect_identical(term7, expected7)
  
  
  term8 <- get_terms(
    gpheats,
    rank|id ~ round + field*heat + player(rider|name)
  )
  expected8 <- list(c(rider = "character"),
                    c(name = "character"),
                    c(round = "numeric"),
                    c(field = "numeric"),
                    c(heat = "numeric"),
                    c(field = "numeric", 
                      heat = "numeric"))
  expect_identical(term8, expected8)
  
  expect_error(
    get_terms(
      gpheats,
      rank|id ~ rider + round + field:heat + heat:field_f + unknown
    ),
    "Variable\\(s\\) .+ specified in formula not present in data"
  )
  
  expect_error(
    get_terms(
      gpheats,
      rank|id ~ rider + round + field:heat + heat:field_f:unknown
    ),
    "Variable\\(s\\) .+ specified in formula not present in data"
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
  
  term6 <- get_terms(
    gpheats,
    rank|id ~ rider + round + field:heat:name
  )
  
  expect_error(
    get_terms_map(gpheats[1:6,], term6),
    "Only two-variable interactions are possible"
  )
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
      round = gpheats[1:6, "round"]
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
      round = gpheats[1:6, "round"],
      `field*heat` = gpheats[1:6, "field"] * gpheats[1:6, "heat"],
      `rider|heat` = gpheats[1:6, "heat"],
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
      round = gpheats[1:6, "round"],
      `field*heat` = gpheats[1:6, "field"] * gpheats[1:6, "heat"],
      `rider|name` = rep(1, 6),
      check.names = FALSE,
      row.names = 1:6
    )
  )
  expect_identical(terms_mat5, expected5)
  
  
  term6 <- get_terms(
    gpheats,
    rank|id ~ rider + round + field:heat:name
  )
  
  expect_error(
    get_terms_mat(gpheats[1:6,], term6),
    "Only two-variable interactions are possible"
  )
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
  
  
  term6 <- get_terms(
    gpheats,
    rank|id ~ rider + round + field:heat:name
  )
  
  expect_error(
    get_terms_cls(gpheats[1:6,], term6),
    "Only two-variable interactions are possible"
  )
})

