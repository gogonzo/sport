d <- data.frame(
  event_id = rep(1:4, each = 4),
  place    = rep(c(3, 4, 1, 2), 4),
  rider    = rep(c("A", "B", "C", "D"), 4),
  weight   = rep(1, 16),
  stringsAsFactors = FALSE
)
d_next <- data.frame(
  event_id = 5,
  place    = c(2, 1, 4, 3),
  rider    = c("A", "B", "C", "D"),
  weight   = 1,
  stringsAsFactors = FALSE
)

testthat::test_that("as.ratings(list) preserves all settings across JSON round-trip", {
  for (method in c("glicko", "glicko2", "bbt")) {
    runner <- get(paste0(method, "_run"))
    m <- runner(
      data = d, formula = place | event_id ~ player(rider),
      weight = "weight", kappa = 0.97
    )

    js <- jsonlite::toJSON(as.list(m), auto_unbox = TRUE, digits = NA)
    back <- as.ratings(jsonlite::fromJSON(js, simplifyVector = TRUE))

    s_orig <- attr(m, "settings")
    s_back <- attr(back, "settings")

    # Every key from the original must come through unchanged.
    for (key in names(s_orig)) {
      testthat::expect_equal(
        s_back[[key]], s_orig[[key]],
        info = sprintf("%s$%s", method, key)
      )
    }
    testthat::expect_identical(attr(back, "method"), method)
    testthat::expect_equal(unname(back$r),  unname(m$r))
    testthat::expect_equal(unname(back$rd), unname(m$rd))
  }
})

testthat::test_that("update(as.ratings(json)) equals fresh _run() on combined data", {
  for (method in c("glicko", "glicko2", "bbt")) {
    runner <- get(paste0(method, "_run"))
    m <- runner(
      data = d, formula = place | event_id ~ player(rider),
      weight = "weight", kappa = 0.97
    )

    js <- jsonlite::toJSON(as.list(m), auto_unbox = TRUE, digits = NA)
    back <- as.ratings(jsonlite::fromJSON(js, simplifyVector = TRUE))
    m_updated <- update(back, newdata = d_next)

    m_fresh <- runner(
      data = rbind(d, d_next),
      formula = place | event_id ~ player(rider),
      weight = "weight", kappa = 0.97
    )

    testthat::expect_equal(
      m_updated$r, m_fresh$r,
      tolerance = 1e-6,
      info = paste(method, "r")
    )
    testthat::expect_equal(
      m_updated$rd, m_fresh$rd,
      tolerance = 1e-6,
      info = paste(method, "rd")
    )
  }
})
