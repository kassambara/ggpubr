# Data preparation
df <- ToothGrowth
df$dose <- as.factor(df$dose)
df$group <- factor(rep(c("grp1", "grp2"), 30))


# Comparing two groups
.get_stat_test <- function(df,  ...){
  bxp <- ggboxplot(df, x = "supp", y = "len") +
    stat_compare_means(...)
  bxp_build <- ggplot2::ggplot_build(bxp)
  stat.test <- bxp_build$data[[2]]
  stat.test
}


# Pairwise comparison test
.get_pwc_test <- function(df,  ...){
  my_comparisons <- list( c("0.5", "1"), c("1", "2"), c("0.5", "2") )
  bxp <- ggboxplot(df, x = "dose", y = "len")+
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
              info = paste("Observed label =", label_coords_observed$label))
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
              y = c(34.494,35.385,35.385,41.622,
                    42.513,42.513,38.058,38.949,38.949),
           yend = c(35.385,35.385,34.494,42.513,
                    42.513,41.622,38.949,38.949,38.058),
     annotation = c("1.3e-07",
                  "1.3e-07","1.3e-07","4.4e-14","4.4e-14","4.4e-14",
                  "1.9e-05","1.9e-05","1.9e-05"),
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
    y = c(34.494,35.385,35.385,41.622,
          42.513,42.513,38.058,38.949,38.949),
    yend = c(35.385,35.385,34.494,42.513,
             42.513,41.622,38.949,38.949,38.058),
    annotation = c("****", "****","****","****","****","****",
                   "****","****","****"),
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
    y = c(34.494,35.385,35.385,41.622,
          42.513,42.513,38.058,38.949,38.949),
    yend = c(35.385,35.385,34.494,42.513,
             42.513,41.622,38.949,38.949,38.058),
    annotation = c("****", "****","****","****","****","****",
                   "****","****","****"),
    stringsAsFactors = FALSE
  )
  expect_equal(pwc_expected, pwc_observed)
})


