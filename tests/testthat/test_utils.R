context("utils")

test_that("check single arguments", {
  expect_error(
    check_single_argument(c(1, 2), "test"),
    "test should be a single value"
  )
  expect_error(
    check_single_argument(c("A", "b"), "test"),
    "test should be a single value"
  )
  expect_error(
    check_single_argument(1, "test", min = 0, max = .99),
    "test should be lower than 0.99"
  )
  
  expect_error(
    check_single_argument(1, "test", min = 1.1),
    "test should be greater than 1.1"
  )
  
  expect_silent(check_single_argument("a", "test"))
  expect_silent(check_single_argument(1, "test"))
  expect_silent(check_single_argument(1, "test", min = 0.99))
  expect_silent(check_single_argument(1, "test", max = 1.1))
})

test_that("check vector arguments", {
  expect_error(
    check_numeric_argument("a", "test"),
    "test should be of type numeric"
  )
  
  expect_error(
    check_numeric_argument(c(1, NA_real_), "test"),
    "test contains non-finite values"
  )
  
  expect_error(
    check_numeric_argument(c(-1, 1), "test", min = 0),
    "test should be in range \\[0, Inf\\]"
  )
  
  expect_error(
    check_numeric_argument(c(1, 2, 3), "test", max = 2),
    "values in variable test should be in range \\[-Inf, 2\\]"
  )
  
  expect_error(
    check_numeric_argument(c(1, 2, 3), "test", min = 2),
    "values in variable test should be in range \\[2, Inf\\]"
  )
  
  expect_silent(check_numeric_argument(c(1, 2, 3), "test", min = 1, max = 3))
  
  
  
  expect_error(
    check_integer_argument("a", "test"),
    "test should be of type integer"
  )
  
  expect_error(
    check_integer_argument(c(1L, NA_integer_), "test"),
    "test contains non-finite values"
  )
  
  expect_error(
    check_integer_argument(c(-1L, 1L), "test", min = 0),
    "test should be in range \\[0, Inf\\]"
  )
  
  expect_error(
    check_integer_argument(c(1L, 2L, 3L), "test", max = 2),
    "values in variable test should be in range \\[-Inf, 2\\]"
  )
  
  expect_error(
    check_integer_argument(c(1L, 2L, 3L), "test", min = 2),
    "values in variable test should be in range \\[2, Inf\\]"
  )
  
  expect_silent(check_integer_argument(c(1L, 2L, 3L), "test", min = 1, max = 3))
  
  
  expect_error(
    check_string_argument(1, "test"),
    "test should be of type character"
  )
  
  expect_error(
    check_string_argument(c("A", NA_character_), "test"),
    "test contains non-finite values"
  )
  
})

test_that("check data", {
  expect_error(
    is_data_provided(),
    "Data is not provided"
  )
})

test_that("valid formula", {
  expect_error(
    is_formula_missing(), 
    "Formula is not specified"
  )
  
  expect_error(
    is_lhs_valid(rank:id ~ elo),
    "LHS of formula must be seperated by `\\|` operator eg."
  )
  
  expect_error(
    is_lhs_valid(rank + id ~ elo),
    "LHS of formula must be seperated by `\\|` operator eg."
  )
  
  expect_silent(is_lhs_valid(rank ~ elo))
  expect_silent(is_rhs_valid(rank ~ elo))
  expect_silent(is_lhs_valid(rank|test ~ elo))
  expect_silent(is_rhs_valid(rank ~ nest(player|team)))
  expect_silent(is_lhs_valid(rank|test ~ nest(player|team)))
})

test_that("check initial r", {
  expect_identical(
    setNames(1500, "A"),
    init_check_r(r = setNames(1500, "A"),
                 player = "player_Var",
                 init_r = 1500, 
                 unique_names = "A")
  )
  
  expect_identical(
    setNames(c(1500, 1500), c("A", "B")),
    init_check_r(r = setNames(c(1500, 1500), c("A", "B")),
                 player = "player_Var",
                 init_r = 1500, 
                 unique_names = c("A", "B"))
  )
  
  expect_identical(
    setNames(c(1500, -1), c("A", "B")),
    init_check_r(r = setNames(c(1500, -1), c("A", "B")),
                 player = "player_var",
                 init_r = 1500, 
                 unique_names = c("A", "B"))
  )
  
  
  
  expect_identical(
    setNames(c(1500, 1500), c("A", "B")),
    init_check_r(r = setNames(c(1500, 1500), c("A", "B")),
                 player = "player_var",
                 init_r = -1500, 
                 unique_names = c("A", "B"))
  )
  
  expect_identical(
    setNames(c(99, 99), c("A", "B")),
    init_check_r(r = NULL,
                 player = "player_var",
                 init_r = 99, 
                 unique_names = c("A", "B"))
  )
  
  expect_error(
    init_check_r(r = setNames(c(1500, 1500, 1500), c("A", "B", "C")),
                 player = "player_var",
                 init_r = 1500, 
                 unique_names = c("A", "B")),
    "All names in r should have a name which match player_var"
  )
  
  expect_error(
    init_check_r(r = setNames(c(1500, 1500, 1500), c("A", "B", "A")),
                 player = "player_var",
                 init_r = 1500, 
                 unique_names = c("A", "B")),
    "All names in r should be unique. Duplicated names not allowed"
  )
  
  expect_error(
    init_check_r(r = setNames(c(1500, NA_real_), c("A", "B")),
                 player = "player_var",
                 init_r = 1500, 
                 unique_names = c("A", "B")),
    "All values in r should be a finite number. NA's not allowed"
  )
  
  expect_error(
    init_check_r(r = NULL,
                 player = "player_var",
                 init_r = NA_real_, 
                 unique_names = c("A", "B")),
    "init_r should be a finite number"
  )
  
})

test_that("check initial rd", {
  expect_identical(
    setNames(1500, "A"),
    init_check_rd(rd = setNames(1500, "A"),
                  player = "player_Var",
                  init_rd = 1500, 
                  unique_names = "A")
  )
  
  expect_identical(
    setNames(c(1500, 1500), c("A", "B")),
    init_check_rd(rd = setNames(c(1500, 1500), c("A", "B")),
                  player = "player_Var",
                  init_rd = 1500, 
                  unique_names = c("A", "B"))
  )
  
  expect_identical(
    setNames(c(1500, 1500), c("A", "B")),
    init_check_rd(rd = setNames(c(1500, 1500), c("A", "B")),
                  player = "player_var",
                  init_rd = -1500, 
                  unique_names = c("A", "B"))
  )
  
  expect_identical(
    setNames(c(9999, 9999), c("A", "B")),
    init_check_rd(rd = NULL,
                  player = "player_var",
                  init_rd = 9999, 
                  unique_names = c("A", "B"))
  )
  
  expect_error(
    init_check_rd(rd = setNames(c(1500, 1500, 1500), c("A", "B", "C")),
                 player = "player_var",
                 init_rd = 1500, 
                 unique_names = c("A", "B")),
    "All names in rd should have a name which match player_var"
  )
  
  expect_error(
    init_check_rd(rd = setNames(c(1500, 1500, 1500), c("A", "B", "A")),
                 player = "player_var",
                 init_rd = 1500, 
                 unique_names = c("A", "B")),
    "All names in rd should be unique. Duplicated names not allowed"
  )
  
  expect_error(
    init_check_rd(rd = setNames(c(1500, NA_real_), c("A", "B")),
                 player = "player_var",
                 init_rd = 1500, 
                 unique_names = c("A", "B")),
    "All values in rd should be a finite number. NA's not allowed"
  )
  
  expect_error(
    init_check_rd(rd = NULL,
                 player = "player_var",
                 init_rd = NA_real_, 
                 unique_names = c("A", "B")),
    "init_rd value should be positive"
  )
  
  expect_error(
    init_check_rd(rd = NULL,
                  player = "player_var",
                  init_rd = 0, 
                  unique_names = c("A", "B")),
    "init_rd value should be positive"
  )
  
  
  expect_error(
    init_check_rd(rd = setNames(c(1500, -1), c("A", "B")),
                  player = "player_var",
                  init_rd = 1500, 
                  unique_names = c("A", "B")),
    "All values in rd should be positive"
  )
})

test_that("check initial sigma", {
  expect_identical(
    numeric(0),
    init_check_sigma(sigma = setNames(1500, "A"),
                     player = "player_Var",
                     init_sigma = 1500, 
                     unique_names = "A", 
                     method = "glicko")
  )
  
  expect_identical(
    setNames(1500, "A"),
    init_check_sigma(sigma = setNames(1500, "A"),
                  player = "player_Var",
                  init_sigma = 1500, 
                  unique_names = "A", 
                  method = "glicko2")
  )
  
  expect_identical(
    setNames(c(1500, 1500), c("A", "B")),
    init_check_sigma(sigma = setNames(c(1500, 1500), c("A", "B")),
                  player = "player_Var",
                  init_sigma = 1500, 
                  unique_names = c("A", "B"), 
                  method = "glicko2")
  )
  
  expect_identical(
    setNames(c(1500, 1500), c("A", "B")),
    init_check_sigma(sigma = setNames(c(1500, 1500), c("A", "B")),
                  player = "player_var",
                  init_sigma = -1500, 
                  unique_names = c("A", "B"), 
                  method = "glicko2")
  )
  
  expect_identical(
    setNames(c(9999, 9999), c("A", "B")),
    init_check_sigma(sigma = NULL,
                  player = "player_var",
                  init_sigma = 9999, 
                  unique_names = c("A", "B"), 
                  method = "glicko2")
  )
  
  expect_error(
    init_check_sigma(sigma = setNames(c(1500, 1500, 1500), c("A", "B", "C")),
                  player = "player_var",
                  init_sigma = 1500, 
                  unique_names = c("A", "B"),
                  method = "glicko2"),
    "All names in sigma should have a name which match player_var"
  )
  
  expect_error(
    init_check_sigma(sigma = setNames(c(1500, 1500, 1500), c("A", "B", "A")),
                  player = "player_var",
                  init_sigma = 1500, 
                  unique_names = c("A", "B"),
                  method = "glicko2"),
    "All names in sigma should be unique. Duplicated names not allowed"
  )
  
  expect_error(
    init_check_sigma(sigma = setNames(c(1500, NA_real_), c("A", "B")),
                  player = "player_var",
                  init_sigma = 1500, 
                  unique_names = c("A", "B"),
                  method = "glicko2"),
    "All values in sigma should be a finite number. NA's not allowed"
  )
  
  expect_error(
    init_check_sigma(sigma = NULL,
                  player = "player_var",
                  init_sigma = NA_real_, 
                  unique_names = c("A", "B"),
                  method = "glicko2"),
    "init_sigma value should be positive"
  )
  
  expect_error(
    init_check_sigma(sigma = NULL,
                     player = "player_var",
                  init_sigma = 0, 
                  unique_names = c("A", "B"), 
                  method = "glicko2"),
    "init_sigma value should be positive"
  )
  
  
  
  expect_error(
    init_check_sigma(sigma = setNames(c(1500, -1), c("A", "B")),
                  player = "player_var",
                  init_sigma = 1500, 
                  unique_names = c("A", "B"),
                  method = "glicko2"),
    "All values in sigma should be positive"
  )
})
