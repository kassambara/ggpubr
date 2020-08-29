context("test-geom_pwc")

# Data preparation
df <- ToothGrowth
df$dose <- as.factor(df$dose)
df$group <- factor(rep(c("grp1", "grp2"), 30))
head(df, 3)

test_that("geom_pwc works for two independent tests", {
  bxp <- ggboxplot(df, x = "supp", y = "len") +
    geom_pwc(method = "wilcox_test")
  bxp_build <- ggplot2::ggplot_build(bxp)
  stat.test <- bxp_build$data[[2]]
  label_coords <- as.data.frame(get_pwc_label_coords(stat.test))
  label_coords_expected <- data.frame(
    x = 1.5,
    y = 35.395,
    angle = 0
  )
  expect_equal(stat.test$group, c(1, 1, 1))
  expect_equal(stat.test$bracket.group, c(1, 1, 1))
  expect_equal(as.numeric(stat.test$x), c(1, 1, 2))
  expect_equal(as.numeric(stat.test$xend), c(1, 2, 2))
  expect_equal(stat.test$y, c(34.494, 35.385, 35.385))
  expect_equal(stat.test$yend, c(35.385, 35.385, 34.494))
  expect_equal(label_coords, label_coords_expected)
})

test_that("geom_pwc works for pairwise comparisons", {
  bxp <- ggboxplot(df, x = "dose", y = "len") +
    geom_pwc(method = "wilcox_test")
  bxp_build <- ggplot2::ggplot_build(bxp)
  stat.test <- bxp_build$data[[2]]
  label_coords <- as.data.frame(get_pwc_label_coords(stat.test))
  label_coords_expected <- data.frame(
    x = c(1.5, 2.0, 2.5),
    y = c(35.395, 38.959, 42.523),
    angle = c(0, 0, 0)
  )
  expect_equal(stat.test$group, c(1L, 2L, 3L, 1L, 2L, 3L, 1L, 2L, 3L))
  expect_equal(stat.test$bracket.group, c(1L, 2L, 3L, 1L, 2L, 3L, 1L, 2L, 3L))
  expect_equal(as.numeric(stat.test$x), c(1, 1, 2, 1, 1, 2, 2, 3, 3))
  expect_equal(as.numeric(stat.test$xend), c(1, 1, 2, 2, 3, 3, 2, 3, 3))
  expect_equal(stat.test$y, c(34.494, 38.058, 41.622, 35.385, 38.949, 42.513, 35.385, 38.949, 42.513))
  expect_equal(stat.test$yend, c(35.385, 38.949, 42.513, 35.385, 38.949, 42.513, 34.494, 38.058, 41.622))
  expect_equal(label_coords, label_coords_expected)
})


test_that("geom_pwc works for pairwise comparisons against a reference group", {
  bxp <- ggboxplot(df, x = "dose", y = "len") +
    geom_pwc(method = "wilcox_test", ref.group = "0.5")
  bxp_build <- ggplot2::ggplot_build(bxp)
  stat.test <- bxp_build$data[[2]]
  label_coords <- as.data.frame(get_pwc_label_coords(stat.test))
  label_coords_expected <- data.frame(
    x = c(1.5, 2.0),
    y = c(35.395, 38.959),
    angle = c(0, 0)
  )
  expect_equal(stat.test$group, c(1L, 2L, 1L, 2L, 1L, 2L))
  expect_equal(stat.test$bracket.group, c(1L, 2L, 1L, 2L, 1L, 2L))
  expect_equal(as.numeric(stat.test$x), c(1, 1, 1, 1, 2, 3))
  expect_equal(as.numeric(stat.test$xend), c(1, 1, 2, 3, 2, 3))
  expect_equal(stat.test$y, c(34.494, 38.058, 35.385, 38.949, 35.385, 38.949))
  expect_equal(stat.test$yend, c(35.385, 38.949, 35.385, 38.949, 34.494, 38.058))
  expect_equal(label_coords, label_coords_expected)
})


test_that("geom_pwc works for pairwise comparisons against a reference group when remove.bracket=TRUE", {
  bxp <- ggboxplot(df, x = "dose", y = "len") +
    geom_pwc(method = "wilcox_test", ref.group = "0.5", remove.bracket = TRUE)
  bxp_build <- ggplot2::ggplot_build(bxp)
  stat.test <- bxp_build$data[[2]]
  label_coords <- as.data.frame(get_pwc_label_coords(stat.test))
  label_coords_expected <- data.frame(
    x = c(2, 3),
    y = c(35.395, 38.959),
    angle = c(0, 0)
  )
  expect_equal(label_coords, label_coords_expected)
})

test_that("geom_pwc works for pairwise comparisons against all. Same y.position for all labels", {
  bxp <- ggboxplot(df, x = "dose", y = "len") +
    geom_pwc(method = "wilcox_test", ref.group = "all")
  bxp_build <- ggplot2::ggplot_build(bxp)
  stat.test <- bxp_build$data[[2]]
  label_coords <- as.data.frame(get_pwc_label_coords(stat.test))
  label_coords_expected <- data.frame(
    x = c(1, 2, 3),
    y = c(35.395, 35.395, 35.395),
    angle = c(0, 0, 0)
  )
  expect_equal(label_coords, label_coords_expected)
})

