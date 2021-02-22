context("test-stat_friedman_test")

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


# Basic plots----------------------
test_that("stat_friedman_test works for basic plots", {
  df$id <- as.factor(c(rep(1:10, 3), rep(11:20, 3)))
  bxp <- ggboxplot(df, x = "dose", y = "len") +
    stat_friedman_test(wid = "id")
  bxp_build <- ggplot2::ggplot_build(bxp)
  stat.test <- bxp_build$data[[2]]
  expect_equal(as.numeric(stat.test$x), 1)
  expect_equal(as.numeric(stat.test$y), 36.4)
  expect_equal(stat.test$label, "Friedman test, p < 0.0001")
})


test_that("stat_friedman_test works for grouped plot: group by x var", {
  df$id <- as.factor(rep(1:10, 6))
  bxp <- ggboxplot(df, x = "dose", y = "len", color = "supp", palette = "jco") +
    stat_friedman_test(aes(group = supp, color = supp),
                    label = "p.format", wid = "id", group.by = "x.var")
  bxp_build <- ggplot2::ggplot_build(bxp)
  stat.test <- bxp_build$data[[2]]
  expect_equal(as.numeric(stat.test$x), c(1, 2, 3))
  expect_equal(as.numeric(stat.test$y), c(36.4, 36.4, 36.4))
  expect_equal(stat.test$label, c("0.011", "0.058", "1"))
})

test_that("stat_friedman_test works for grouped one-way repeated measure anova: group by legend var", {
  df$id <- as.factor(rep(1:10, 6))
  bxp <- ggboxplot(df, x = "dose", y = "len", color = "supp", palette = "jco") +
    stat_friedman_test(aes(group = supp, color = supp),
                    wid = "id", group.by = "legend.var")
  bxp_build <- ggplot2::ggplot_build(bxp)
  stat.test <- bxp_build$data[[2]]
  expect_equal(as.numeric(stat.test$x), c(0.8, 0.8))
  expect_equal(as.numeric(stat.test$y), c(36.40, 39.62))
  expect_equal(stat.test$label, c("Friedman test, p = 0.02472", "Friedman test, p = 0.00012"))
})
