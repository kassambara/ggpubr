test_that("AUC equals the Mann-Whitney value (pinned fixture)", {
  # Fixed, RNG-free data: pos scores c(2,4,5), neg scores c(1,2,3).
  # AUC = (#pos>neg + 0.5*ties) / (n_pos*n_neg) = 7.5 / 9 = 0.8333333.
  d <- data.frame(
    y = factor(c("n", "n", "n", "p", "p", "p"), levels = c("n", "p")),
    x = c(1, 2, 3, 2, 4, 5)
  )
  roc <- .compute_roc(d, "y", "x", conf.level = 0.95)
  expect_equal(roc$summary$auc, 7.5 / 9, tolerance = 1e-9)
  # CI brackets the estimate and stays in [0, 1].
  expect_true(roc$summary$lo <= roc$summary$auc)
  expect_true(roc$summary$hi >= roc$summary$auc)
  expect_gte(roc$summary$lo, 0)
  expect_lte(roc$summary$hi, 1)
})

test_that("AUC matches pROC::auc() when it is available", {
  skip_if_not_installed("pROC")
  set.seed(1)
  n <- 40
  d <- data.frame(
    y = factor(rep(c("ctrl", "case"), each = n), levels = c("ctrl", "case")),
    x = c(stats::rnorm(n), stats::rnorm(n, 1))
  )
  ours <- .compute_roc(d, "y", "x", 0.95)$summary$auc
  pr <- pROC::auc(pROC::roc(d$y, d$x,
    levels = c("ctrl", "case"),
    direction = "<", quiet = TRUE
  ))
  expect_equal(ours, as.numeric(pr), tolerance = 1e-9)
})

test_that("the positive class is the second level; reversing it flips the AUC", {
  d <- data.frame(
    y = factor(c("n", "n", "n", "p", "p", "p"), levels = c("n", "p")),
    x = c(1, 2, 3, 2, 4, 5)
  )
  d2 <- d
  d2$y <- factor(d2$y, levels = c("p", "n")) # flip which class is positive
  a1 <- .compute_roc(d, "y", "x", 0.95)$summary$auc
  a2 <- .compute_roc(d2, "y", "x", 0.95)$summary$auc
  expect_equal(a1 + a2, 1, tolerance = 1e-9)
})

test_that("output is deterministic (no RNG, freeze-stable)", {
  set.seed(1)
  d <- data.frame(
    y = factor(rep(c("n", "p"), each = 30), levels = c("n", "p")),
    x = stats::rnorm(60)
  )
  expect_identical(
    .compute_roc(d, "y", "x", 0.95),
    .compute_roc(d, "y", "x", 0.95)
  )
})

test_that("ggrocplot() returns a ggplot for single and multiple predictors", {
  set.seed(1)
  n <- 30
  d <- data.frame(
    y = factor(rep(c("n", "p"), each = n), levels = c("n", "p")),
    x1 = c(stats::rnorm(n), stats::rnorm(n, 1)),
    x2 = c(stats::rnorm(n), stats::rnorm(n, 0.5))
  )
  p1 <- ggrocplot(d, "y", "x1")
  p2 <- ggrocplot(d, "y", c("x1", "x2"), palette = "jco")
  expect_s3_class(p1, "ggplot")
  expect_s3_class(p2, "ggplot")
  # The multi-predictor plot maps a color aesthetic; the single one does not.
  expect_false("colour" %in% names(p1$mapping))
  expect_true("colour" %in% names(p2$mapping))
})

test_that("invalid input is rejected with a clear message", {
  d <- data.frame(
    y = factor(c("a", "b", "c")),
    x = 1:3
  )
  expect_error(ggrocplot(d, "y", "x"), "two classes")
  expect_error(ggrocplot(d, "missing", "x"), "not found")
})

test_that("a predictor whose NAs remove a whole class errors, not NaN", {
  # y has both classes, but x is missing on every positive row.
  d <- data.frame(
    y = factor(rep(c("n", "p"), each = 5), levels = c("n", "p")),
    x = c(1, 2, 3, 4, 5, NA, NA, NA, NA, NA)
  )
  expect_error(ggrocplot(d, "y", "x"), "both outcome classes")
})

test_that("the ROC axes use collision-free breaks and the multi legend wraps", {
  set.seed(1)
  n <- 30
  d <- data.frame(
    y = factor(rep(c("n", "p"), each = n), levels = c("n", "p")),
    x1 = c(stats::rnorm(n), stats::rnorm(n, 1)),
    x2 = c(stats::rnorm(n), stats::rnorm(n, 0.5))
  )
  b <- ggplot2::ggplot_build(ggrocplot(d, "y", "x1"))
  expect_equal(b$layout$panel_params[[1]]$x$get_breaks(), c(0, 0.5, 1))
  expect_equal(b$layout$panel_params[[1]]$y$get_breaks(), c(0, 0.5, 1))
  # Multi-predictor legend labels place the AUC on a second line.
  p2 <- ggrocplot(d, "y", c("x1", "x2"))
  expect_true(all(grepl("\n", levels(ggplot2::ggplot_build(p2)$plot$data$.model))))
})
