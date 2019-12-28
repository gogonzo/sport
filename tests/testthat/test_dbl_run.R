context("dbl_run")
gpheats$field_f <- as.factor(gpheats$field)
gpheats$beta <- 100
gpheats$weight <- .9
gpheats$weight2 <- 1.0

formula <- rank | id ~ rider + field + field_f
r <- dbl_run(formula, data = gpheats[1:16, ])$final_r
rd <- dbl_run(formula, data = gpheats[1:16, ])$final_rd


test_that("", {
  
  dbl_run(
    formula <- rank | id ~ rider + field + field_f,
    data = gpheats[1:8, ]
  )
  
  
  
  
})