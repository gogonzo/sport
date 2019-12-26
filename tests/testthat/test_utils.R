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
