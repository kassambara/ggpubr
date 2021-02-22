context("test-stat_kruskal_test")

# Data preparation
df <- ToothGrowth
df$dose <- as.factor(df$dose)
df$id <- rep(1:10, 6) # Add individuals id
# Add a random grouping variable
set.seed(123)
df$group <- sample(factor(rep(c("grp1", "grp2", "grp3"), 20)))
df <- df %>% mutate(
  len = ifelse(group == "grp2", len+2, len),
  len = ifelse(group == "grp3", len+7, len)
)


# Basic plots -----------------------------------
test_that("stat_kruskal_test works for basic ggplots", {
  bxp <- ggboxplot(df, x = "dose", y = "len") +
    stat_kruskal_test()
  bxp_build <- ggplot2::ggplot_build(bxp)
  stat.test <- bxp_build$data[[2]]
  expect_equal(as.numeric(stat.test$x), 1)
  expect_equal(as.numeric(stat.test$y), 36.4)
  expect_equal(stat.test$label, "Kruskal-Wallis, p < 0.0001")
})


# Grouped plots-----------------------------------
test_that("stat_kruskal_test works for grouped plots: grouped by x position", {
  bxp <- ggboxplot(df, x = "group", y = "len", color = "dose") +
    stat_kruskal_test(aes(group = dose), label = "{p.adj.format}")
  bxp_build <- ggplot2::ggplot_build(bxp)
  stat.test <- bxp_build$data[[2]]
  expect_equal(as.numeric(stat.test$x), c(1, 2, 3))
  expect_equal(as.numeric(stat.test$y), c(36.4, 36.4, 36.4))
  expect_equal(stat.test$label, c("0.0028", "0.0056", "0.0056"))
})

test_that("stat_kruskal_test works for grouped plots: grouped by legend variable", {
  bxp <- ggboxplot(df, x = "group", y = "len", color = "dose") +
    stat_kruskal_test(aes(group = dose), label = "{p.format}", group.by  = "legend.var")
  bxp_build <- ggplot2::ggplot_build(bxp)
  stat.test <- bxp_build$data[[2]]
  expect_equal(as.numeric(stat.test$x), c(0.73, 0.73, 0.73))
  expect_equal(as.numeric(stat.test$y), c(36.4, 39.62, 42.84))
  expect_equal(stat.test$label, c("0.012", "0.018", "0.038"))
})
