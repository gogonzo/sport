testthat::test_that("positional returns valid probability matrix", {
  data <- data.frame(
    id = 1,
    name = c("A", "B", "C", "D"),
    rank = c(3, 4, 1, 2)
  )

  r <- setNames(c(1500, 1400, 1550, 1700), c("A", "B", "C", "D"))
  rd <- setNames(c(200, 30, 100, 300), c("A", "B", "C", "D"))

  glicko <- glicko_run(rank | id ~ player(name), data = data, r = r, rd = rd)
  pos <- positional(glicko, data)

  # Should have 4 rows (one per player)
  testthat::expect_equal(nrow(pos), 4)

  # Should have player column + 4 position columns
  testthat::expect_equal(ncol(pos), 5)
  testthat::expect_true(all(c("pos_1", "pos_2", "pos_3", "pos_4") %in% names(pos)))

  # Each row should sum to 1 (player must finish somewhere)
  row_sums <- unname(rowSums(pos[, 2:5]))
  testthat::expect_equal(row_sums, rep(1, 4), tolerance = 1e-10)

  # Each column should sum to 1 (exactly one player per position)
  col_sums <- unname(colSums(pos[, 2:5]))
  testthat::expect_equal(col_sums, rep(1, 4), tolerance = 1e-10)

  # All probabilities should be between 0 and 1
  testthat::expect_true(all(pos[, 2:5] >= 0))
  testthat::expect_true(all(pos[, 2:5] <= 1))

  # Player D (rating 1700) should have highest P(1st)
  testthat::expect_equal(pos$name[which.max(pos$pos_1)], "D")

  # Player B (rating 1400) should have highest P(4th)
  testthat::expect_equal(pos$name[which.max(pos$pos_4)], "B")
})

testthat::test_that("positional works with 2 players", {
  data <- data.frame(
    id = 1,
    name = c("A", "B"),
    rank = c(1, 2)
  )

  r <- setNames(c(1500, 1400), c("A", "B"))
  rd <- setNames(c(200, 30), c("A", "B"))

  glicko <- glicko_run(rank | id ~ player(name), data = data, r = r, rd = rd)
  pos <- positional(glicko, data)

  testthat::expect_equal(nrow(pos), 2)
  testthat::expect_equal(ncol(pos), 3)

  # P(A wins) + P(B wins) = 1
  testthat::expect_equal(sum(pos$pos_1), 1, tolerance = 1e-10)

  # P(A 1st) = P(B 2nd) for 2-player case
  testthat::expect_equal(pos$pos_1[pos$name == "A"], pos$pos_2[pos$name == "B"], tolerance = 1e-10)
})

testthat::test_that("positional works with glicko2 and bbt", {
  data <- data.frame(
    id = 1,
    name = c("A", "B", "C"),
    rank = c(1, 2, 3)
  )

  glicko2 <- glicko2_run(rank | id ~ player(name), data = data)
  pos2 <- positional(glicko2, data)
  testthat::expect_equal(nrow(pos2), 3)
  testthat::expect_equal(unname(rowSums(pos2[, 2:4])), rep(1, 3), tolerance = 1e-10)
  testthat::expect_equal(unname(colSums(pos2[, 2:4])), rep(1, 3), tolerance = 1e-10)

  bbt <- bbt_run(rank | id ~ player(name), data = data)
  pos_bbt <- positional(bbt, data)
  testthat::expect_equal(nrow(pos_bbt), 3)
  testthat::expect_equal(unname(rowSums(pos_bbt[, 2:4])), rep(1, 3), tolerance = 1e-10)
  testthat::expect_equal(unname(colSums(pos_bbt[, 2:4])), rep(1, 3), tolerance = 1e-10)
})

testthat::test_that("positional errors on < 2 players", {
  data <- data.frame(id = 1, name = "A", rank = 1)
  glicko <- glicko_run(rank | id ~ player(name),
    data = data.frame(id = 1, name = c("A", "B"), rank = c(1, 2))
  )
  testthat::expect_error(positional(glicko, data), "at least 2 players")
})

testthat::test_that("equal ratings give uniform positions", {
  data <- data.frame(
    id = 1,
    name = c("A", "B", "C", "D"),
    rank = 1
  )

  r <- setNames(rep(1500, 4), c("A", "B", "C", "D"))
  rd <- setNames(rep(350, 4), c("A", "B", "C", "D"))

  glicko <- glicko_run(rank | id ~ player(name), data = data, r = r, rd = rd)
  pos <- positional(glicko, data)

  # All players should have 0.25 probability for each position
  for (col in paste0("pos_", 1:4)) {
    testthat::expect_equal(pos[[col]], rep(0.25, 4), tolerance = 1e-10)
  }
})
