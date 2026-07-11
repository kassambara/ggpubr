context("test-stat_cld")

df <- ToothGrowth
df$dose <- as.factor(df$dose)

test_that("stat_cld letters match rstatix::add_cld() (tukey_hsd)", {
  p <- ggboxplot(df, x = "dose", y = "len") + stat_cld()
  stat.data <- ggplot2::ggplot_build(p)$data[[2]]
  stat.data <- stat.data[order(stat.data$x), ]

  truth <- rstatix::add_cld(rstatix::tukey_hsd(df, len ~ dose))
  truth <- truth[order(as.numeric(as.character(truth$group))), ]

  expect_equal(as.character(stat.data$label), as.character(truth$cld))
  expect_equal(as.numeric(stat.data$x), c(1, 2, 3))
})

test_that("stat_cld places one letter per group above the group max", {
  p <- ggboxplot(df, x = "dose", y = "len") + stat_cld()
  stat.data <- ggplot2::ggplot_build(p)$data[[2]]
  stat.data <- stat.data[order(stat.data$x), ]

  grp_max <- as.numeric(tapply(df$len, df$dose, max))
  expect_equal(nrow(stat.data), 3)
  expect_equal(as.numeric(stat.data$y), grp_max)
})

test_that("stat_cld label.y sets a common height for all letters", {
  p <- ggboxplot(df, x = "dose", y = "len") + stat_cld(label.y = 40)
  stat.data <- ggplot2::ggplot_build(p)$data[[2]]
  expect_equal(as.numeric(stat.data$y), rep(40, 3))
})

test_that("stat_cld groups that share a letter are not significantly different", {
  set.seed(42)
  d <- data.frame(
    grp = factor(rep(c("A", "B", "C", "D", "E", "F"), each = 15)),
    val = c(rnorm(45, 10, 2), rnorm(45, 20, 2))
  )
  p <- ggboxplot(d, x = "grp", y = "val") + stat_cld()
  stat.data <- ggplot2::ggplot_build(p)$data[[2]]
  stat.data <- stat.data[order(stat.data$x), ]
  # A,B,C form one non-different cluster; D,E,F another
  expect_equal(length(unique(stat.data$label[1:3])), 1)
  expect_equal(length(unique(stat.data$label[4:6])), 1)
  expect_false(stat.data$label[1] == stat.data$label[4])
})

test_that("stat_cld works with the minimum of 2 groups", {
  d2 <- subset(df, dose %in% c("0.5", "1"))
  d2$dose <- droplevels(d2$dose)
  p <- ggboxplot(d2, x = "dose", y = "len") + stat_cld()
  stat.data <- ggplot2::ggplot_build(p)$data[[2]]
  expect_equal(nrow(stat.data), 2)
})

test_that("stat_cld warns and draws nothing with a single group", {
  d1 <- subset(df, dose == "0.5")
  d1$dose <- droplevels(d1$dose)
  p <- ggboxplot(d1, x = "dose", y = "len") + stat_cld()
  # ggplot2 wraps a compute_panel error as a build-time warning and drops the layer
  expect_warning(ggplot2::ggplot_build(p), "stat_cld")
})

test_that("stat_cld supports non-parametric and other methods", {
  for (m in c("games_howell_test", "dunn_test", "wilcox_test", "t_test")) {
    p <- ggboxplot(df, x = "dose", y = "len") + stat_cld(method = m)
    stat.data <- ggplot2::ggplot_build(p)$data[[2]]
    expect_equal(nrow(stat.data), 3)
    expect_true(all(nchar(stat.data$label) >= 1))
  }
})
