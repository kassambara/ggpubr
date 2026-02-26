# Data preparation
df <- ToothGrowth
df$dose <- as.factor(df$dose)
df$group <- factor(rep(c("grp1", "grp2"), 30))


# Comparing two groups
.get_stat_test <- function(df, ...) {
  bxp <- ggboxplot(df, x = "supp", y = "len") +
    stat_compare_means(...)
  bxp_build <- ggplot2::ggplot_build(bxp)
  stat.test <- bxp_build$data[[2]]
  stat.test
}


# Pairwise comparison test
.get_pwc_test <- function(df, ...) {
  my_comparisons <- list(c("0.5", "1"), c("1", "2"), c("0.5", "2"))
  bxp <- ggboxplot(df, x = "dose", y = "len") +
    stat_compare_means(comparisons = my_comparisons, method = "t.test", ...)

  bxp_build <- ggplot2::ggplot_build(bxp)
  pwc_test <- bxp_build$data[[2]]

  pwc_test <- pwc_test %>%
    dplyr::mutate(
      x = as.numeric(x), xend = as.numeric(xend),
      annotation = as.character(annotation)
    )
  pwc_test
}

# two independent tests -----------------------------------
test_that("stat_compare_means works for two independent tests: Wilcoxon test", {
  stat.test <- .get_stat_test(df)
  label_coords_expected <- data.frame(
    stringsAsFactors = FALSE,
    x = 1,
    y = c(33.9),
    label = c("Wilcoxon, p = 0.064")
  )
  label_coords_observed <- stat.test[, c("x", "y", "label")]
  label_coords_observed$x <- as.numeric(label_coords_observed$x)
  expect_equal(label_coords_expected, label_coords_observed)
})

test_that("stat_compare_means works for two independent tests when method changed to t.test", {
  stat.test <- .get_stat_test(df, method = "t.test")
  label_coords_expected <- data.frame(
    stringsAsFactors = FALSE,
    x = 1,
    y = c(33.9),
    label = c("T-test, p = 0.061")
  )
  label_coords_observed <- stat.test[, c("x", "y", "label")]
  label_coords_observed$x <- as.numeric(label_coords_observed$x)
  expect_equal(label_coords_expected, label_coords_observed)
})


test_that("stat_compare_means works when label specified as label='p.signif'", {
  stat.test <- .get_stat_test(df, label = "p.signif")
  label_coords_expected <- data.frame(
    stringsAsFactors = FALSE,
    x = 1,
    y = c(33.9),
    label = c("ns")
  )
  label_coords_observed <- stat.test[, c("x", "y", "label")]
  label_coords_observed$x <- as.numeric(label_coords_observed$x)
  expect_equal(label_coords_expected, label_coords_observed)
})


test_that("stat_compare_means works when label specified as aes(label=..p.signif..)", {
  stat.test <- .get_stat_test(df, aes(label = ..p.signif..))
  label_coords_expected <- data.frame(
    stringsAsFactors = FALSE,
    x = 1,
    y = c(33.9),
    label = c("ns")
  )
  label_coords_observed <- stat.test[, c("x", "y", "label")]
  label_coords_observed$x <- as.numeric(label_coords_observed$x)
  expect_equal(label_coords_expected, label_coords_observed)
})


# Paired samples-----------------------------------
test_that("stat_compare_means works for paired samples comparison", {
  stat.test <- .get_stat_test(df, paired = TRUE)
  label_coords_observed <- stat.test[, c("x", "y", "label")]
  label_coords_observed$x <- as.numeric(label_coords_observed$x)

  # Accept either 0.0043 (exact=FALSE) or 0.0038 (R-devel exact conditional inference)
  expect_true(label_coords_observed$label %in% c("Wilcoxon, p = 0.0043", "Wilcoxon, p = 0.0038"),
    info = paste("Observed label =", label_coords_observed$label)
  )
  expect_equal(label_coords_observed$x, 1)
  expect_equal(label_coords_observed$y, 33.9)
})


# More than two groups-------------------------------
test_that("stat_compare_means works for pairwise comparisons with multiple groups", {
  pwc_test <- .get_pwc_test(df)
  pwc_observed <- pwc_test[, c("x", "xend", "y", "yend", "annotation")]
  pwc_observed$annotation <- as.character(pwc_observed$annotation)
  pwc_expected <- data.frame(
    x = c(1, 1, 2, 1, 1, 3, 2, 2, 3),
    xend = c(1, 2, 2, 1, 3, 3, 2, 3, 3),
    y = c(
      34.494, 35.385, 35.385, 41.622,
      42.513, 42.513, 38.058, 38.949, 38.949
    ),
    yend = c(
      35.385, 35.385, 34.494, 42.513,
      42.513, 41.622, 38.949, 38.949, 38.058
    ),
    annotation = c(
      "1.3e-07",
      "1.3e-07", "1.3e-07", "4.4e-14", "4.4e-14", "4.4e-14",
      "1.9e-05", "1.9e-05", "1.9e-05"
    ),
    stringsAsFactors = FALSE
  )
  expect_equal(pwc_expected, pwc_observed)
})


# More than two groups-------------------------------
test_that("stat_compare_means works for pairwise comparisons with multiple groups when specifying label as label='p.signif'", {
  pwc_test <- .get_pwc_test(df, label = "p.signif")
  pwc_observed <- pwc_test[, c("x", "xend", "y", "yend", "annotation")]
  pwc_observed$annotation <- as.character(pwc_observed$annotation)
  pwc_expected <- data.frame(
    x = c(1, 1, 2, 1, 1, 3, 2, 2, 3),
    xend = c(1, 2, 2, 1, 3, 3, 2, 3, 3),
    y = c(
      34.494, 35.385, 35.385, 41.622,
      42.513, 42.513, 38.058, 38.949, 38.949
    ),
    yend = c(
      35.385, 35.385, 34.494, 42.513,
      42.513, 41.622, 38.949, 38.949, 38.058
    ),
    annotation = c(
      "****", "****", "****", "****", "****", "****",
      "****", "****", "****"
    ),
    stringsAsFactors = FALSE
  )
  expect_equal(pwc_expected, pwc_observed)
})


test_that("stat_compare_means works for pairwise comparisons with multiple groups when specifying label as aes(label=after_stat(p.signif))", {
  pwc_test <- .get_pwc_test(df, aes(label = after_stat(p.signif)))
  pwc_observed <- pwc_test[, c("x", "xend", "y", "yend", "annotation")]
  pwc_observed$annotation <- as.character(pwc_observed$annotation)
  pwc_expected <- data.frame(
    x = c(1, 1, 2, 1, 1, 3, 2, 2, 3),
    xend = c(1, 2, 2, 1, 3, 3, 2, 3, 3),
    y = c(
      34.494, 35.385, 35.385, 41.622,
      42.513, 42.513, 38.058, 38.949, 38.949
    ),
    yend = c(
      35.385, 35.385, 34.494, 42.513,
      42.513, 41.622, 38.949, 38.949, 38.058
    ),
    annotation = c(
      "****", "****", "****", "****", "****", "****",
      "****", "****", "****"
    ),
    stringsAsFactors = FALSE
  )
  expect_equal(pwc_expected, pwc_observed)
})


# Issue #4: Test for p.format.signif label type
test_that("stat_compare_means works with label='p.format.signif' to show p-value with significance stars", {
  stat.test <- .get_stat_test(df, label = "p.format.signif")
  label_coords_observed <- stat.test[, c("x", "y", "label")]
  label_coords_observed$x <- as.numeric(label_coords_observed$x)

  # The p-value is ~0.064 which is "ns" (not significant)
  expect_equal(label_coords_observed$label, "p = 0.064 ns")
  expect_equal(label_coords_observed$x, 1)
  expect_equal(label_coords_observed$y, 33.9)
})

test_that("stat_compare_means with label='p.format.signif' shows stars for significant results", {
  # Use paired test which has significant p-value
  stat.test <- .get_stat_test(df, label = "p.format.signif", paired = TRUE)
  label_value <- stat.test$label

  # Should have format "p = X.XXXX **" (p ~ 0.004 is **)
  expect_true(grepl("^p = .* \\*\\*$", label_value),
    info = paste("Observed label =", label_value)
  )
})


# Issue #5: Test that p-values with threshold show "p <" not "p = <"
test_that("stat_compare_means with p.format and threshold shows 'p <' not 'p = <'", {
  # Create data with very significant difference to get p < 0.001
  set.seed(123)
  test_df <- data.frame(
    group = rep(c("A", "B"), each = 30),
    value = c(rnorm(30, mean = 0, sd = 1), rnorm(30, mean = 5, sd = 1))
  )

  bxp <- ggboxplot(test_df, x = "group", y = "value") +
    stat_compare_means(label = "p.format", p.format.style = "nejm")
  bxp_build <- ggplot2::ggplot_build(bxp)
  stat.test <- bxp_build$data[[2]]

  # Should show "p < 0.001" not "p = < 0.001"
  expect_true(grepl("^p < ", stat.test$label) || grepl("^p = 0\\.", stat.test$label),
    info = paste("Observed label =", stat.test$label)
  )
  expect_false(grepl("p = <", stat.test$label),
    info = paste("Label should not contain 'p = <', but got:", stat.test$label)
  )
})

test_that("stat_compare_means with p.format.signif and threshold shows 'p <' not 'p = <'", {
  # Create data with very significant difference
  set.seed(123)
  test_df <- data.frame(
    group = rep(c("A", "B"), each = 30),
    value = c(rnorm(30, mean = 0, sd = 1), rnorm(30, mean = 5, sd = 1))
  )

  bxp <- ggboxplot(test_df, x = "group", y = "value") +
    stat_compare_means(label = "p.format.signif", p.format.style = "nejm")
  bxp_build <- ggplot2::ggplot_build(bxp)
  stat.test <- bxp_build$data[[2]]

  # Should show "p < 0.001 ****" not "p = < 0.001 ****"
  expect_true(grepl("^p < ", stat.test$label),
    info = paste("Observed label =", stat.test$label)
  )
  expect_false(grepl("p = <", stat.test$label),
    info = paste("Label should not contain 'p = <', but got:", stat.test$label)
  )
  # Should have stars
  expect_true(grepl("\\*+$", stat.test$label),
    info = paste("Label should end with stars, but got:", stat.test$label)
  )
})


# Tests for custom significance cutoffs and symbols
test_that("stat_compare_means works with custom signif.cutoffs (3 levels)", {
  stat.test <- .get_stat_test(df,
    label = "p.signif",
    signif.cutoffs = c(0.10, 0.05, 0.01)
  )
  # p = 0.064 is between 0.05 and 0.10, so should get "*"
  expect_equal(stat.test$label, "*")
})

test_that("stat_compare_means works with custom signif.cutoffs (4 levels with use.four.stars)", {
  # Use paired test which has significant p-value (~0.004)
  stat.test <- .get_stat_test(df,
    label = "p.signif", paired = TRUE,
    signif.cutoffs = c(0.10, 0.05, 0.01, 0.001),
    use.four.stars = TRUE
  )
  # p ~ 0.004 is between 0.001 and 0.01, so should get "***"
  expect_equal(stat.test$label, "***")
})

test_that("stat_compare_means works with custom signif.symbols", {
  stat.test <- .get_stat_test(df,
    label = "p.signif",
    signif.cutoffs = c(0.10, 0.05, 0.01),
    signif.symbols = c("+", "++", "+++")
  )
  # p = 0.064 is between 0.05 and 0.10, so should get "+"
  expect_equal(stat.test$label, "+")
})

test_that("stat_compare_means works with custom ns.symbol", {
  # Create data with non-significant difference
  set.seed(456)
  test_df <- data.frame(
    group = rep(c("A", "B"), each = 10),
    value = rnorm(20, mean = 5, sd = 1)
  )
  bxp <- ggboxplot(test_df, x = "group", y = "value") +
    stat_compare_means(label = "p.signif", ns.symbol = "N.S.")
  bxp_build <- ggplot2::ggplot_build(bxp)
  stat.test <- bxp_build$data[[2]]

  expect_equal(stat.test$label, "N.S.")
})

test_that("stat_compare_means works with empty ns.symbol", {
  # Create data with non-significant difference
  set.seed(456)
  test_df <- data.frame(
    group = rep(c("A", "B"), each = 10),
    value = rnorm(20, mean = 5, sd = 1)
  )
  bxp <- ggboxplot(test_df, x = "group", y = "value") +
    stat_compare_means(label = "p.signif", ns.symbol = "")
  bxp_build <- ggplot2::ggplot_build(bxp)
  stat.test <- bxp_build$data[[2]]

  expect_equal(stat.test$label, "")
})

test_that("show.signif = FALSE falls back to p.format with warning", {
  expect_warning(
    bxp <- ggboxplot(df, x = "supp", y = "len") +
      stat_compare_means(label = "p.format.signif", show.signif = FALSE),
    "falling back"
  )
  bxp_build <- ggplot2::ggplot_build(bxp)
  stat.test <- bxp_build$data[[2]]
  # Should show p-value only, no stars
  expect_equal(stat.test$label, "p = 0.064")
})

test_that("stat_compare_means with p.format.signif uses custom signif.cutoffs", {
  stat.test <- .get_stat_test(df,
    label = "p.format.signif",
    signif.cutoffs = c(0.10, 0.05, 0.01)
  )
  # p = 0.064 is between 0.05 and 0.10, so should get "*"
  expect_equal(stat.test$label, "p = 0.064 *")
})
