context("test-stat_anova_test")

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
test_that("stat_anova_test works for basic ggplots", {
  bxp <- ggboxplot(df, x = "dose", y = "len") +
    stat_anova_test()
  bxp_build <- ggplot2::ggplot_build(bxp)
  stat.test <- bxp_build$data[[2]]
  expect_equal(as.numeric(stat.test$x), 1)
  expect_equal(as.numeric(stat.test$y), 36.4)
  expect_equal(stat.test$label, "Anova, p < 0.0001")
})

# Grouped plots-----------------------------------
test_that("stat_anova_test works for grouped plots: grouped by x position", {
  bxp <- ggboxplot(df, x = "group", y = "len", color = "dose") +
    stat_anova_test(aes(group = dose), label = "{p.adj.format}")
  bxp_build <- ggplot2::ggplot_build(bxp)
  stat.test <- bxp_build$data[[2]]
  expect_equal(as.numeric(stat.test$x), c(1, 2, 3))
  expect_equal(as.numeric(stat.test$y), c(36.4, 36.4, 36.4))
  expect_equal(stat.test$label, c("<0.0001", "0.00014", "0.00014"))
})

test_that("stat_anova_test works for grouped plots: grouped by legend variable", {
  bxp <- ggboxplot(df, x = "group", y = "len", color = "dose") +
    stat_anova_test(aes(group = dose), label = "{p.format}", group.by = "legend.var")
  bxp_build <- ggplot2::ggplot_build(bxp)
  stat.test <- bxp_build$data[[2]]
  expect_equal(as.numeric(stat.test$x), c(0.73, 0.73, 0.73))
  expect_equal(as.numeric(stat.test$y), c(36.4, 39.62, 42.84))
  expect_equal(stat.test$label, c("0.009", "0.012", "0.019"))
})


# One-way repeated measure ANOVA ----------------------
test_that("stat_anova_test works for one-way repeated measure anova", {
  df$id <- as.factor(c(rep(1:10, 3), rep(11:20, 3)))
  bxp <- ggboxplot(df, x = "dose", y = "len") +
    stat_anova_test(wid = "id", method = "one_way_repeated")
  bxp_build <- ggplot2::ggplot_build(bxp)
  stat.test <- bxp_build$data[[2]]
  expect_equal(as.numeric(stat.test$x), 1)
  expect_equal(as.numeric(stat.test$y), 36.4)
  expect_equal(stat.test$label, "Anova, p < 0.0001")
})


test_that("stat_anova_test works for grouped one-way repeated measure anova: group by x var", {
  df$id <- as.factor(rep(1:10, 6))
  bxp <- ggboxplot(df, x = "dose", y = "len", color = "supp", palette = "jco") +
    stat_anova_test(aes(group = supp, color = supp),
                    label = "p",
                    method = "one_way_repeated",
                    wid = "id", group.by = "x.var")
  bxp_build <- ggplot2::ggplot_build(bxp)
  stat.test <- bxp_build$data[[2]]
  expect_equal(as.numeric(stat.test$x), c(1, 2, 3))
  expect_equal(as.numeric(stat.test$y), c(36.4, 36.4, 36.4))
  expect_equal(stat.test$label, c("0.016", "0.05", "0.848"))
})

test_that("stat_anova_test works for grouped one-way repeated measure anova: group by legend var", {
  df$id <- as.factor(rep(1:10, 6))
  bxp <- ggboxplot(df, x = "dose", y = "len", color = "supp", palette = "jco") +
    stat_anova_test(aes(group = supp, color = supp),
                    method = "one_way_repeated",
                    wid = "id", group.by = "legend.var")
  bxp_build <- ggplot2::ggplot_build(bxp)
  stat.test <- bxp_build$data[[2]]
  expect_equal(as.numeric(stat.test$x), c(0.8, 0.8))
  expect_equal(as.numeric(stat.test$y), c(36.40, 39.62))
  expect_equal(stat.test$label, c("Anova, p = 0.004", "Anova, p < 0.0001"))
})

test_that("stat_anova_test works for two-way repeated measure anova", {
  df$id <- as.factor(rep(1:10, 6))
  bxp <- ggboxplot(df, x = "dose", y = "len", color = "supp", palette = "jco") +
    stat_anova_test(aes(group = supp), method = "two_way_repeated", wid = "id")
  bxp_build <- ggplot2::ggplot_build(bxp)
  stat.test <- bxp_build$data[[2]]
  expect_equal(as.numeric(stat.test$x), 0.8)
  expect_equal(as.numeric(stat.test$y), 36.4)
  expect_equal(stat.test$label, "Anova, p = 0.12")
})


# Two-way mixed anova -----------------------------
test_that("stat_anova_test works for two-way mixed anova", {

  anxiety <- data.frame(
    score = c(14.1,14.5,15.7,16,16.5,16.9,17,17,
              17.3,17.3,17.8,17.9,19.1,19.4,19.8,13.7,14.7,14.9,15.1,
              15.8,16.4,16.6,16.9,16.9,17.2,17.8,17.8,18.2,18.4,
              19.3,14.6,15,15.5,15.7,16.4,16.9,17.1,17.3,17.5,17.6,
              17.8,17.9,18.4,18.5,19,14.4,14.6,15.2,15.5,15.8,16.5,
              16.8,17.1,16.9,17.1,17.7,17.7,19.4,19.2,20,13.4,14.8,
              14.4,14.8,14.8,15.7,16.9,16.9,16.9,17.3,18.2,17.7,
              17.8,18.5,18.9,13,13,12.7,14.1,14.4,14.5,15.6,15.4,
              15.6,14.8,16.1,15.7,16.7,16.4,17.2,14.1,14.3,14.9,15.3,
              15.7,16.2,16.5,16.6,16.5,16.7,17.3,17.5,19.3,17.3,
              19.4,12.7,13.1,13.6,13.6,14.2,14.9,16.1,16.1,16.3,15.9,
              17.4,16.9,17.1,17.3,17.7,11.7,11.9,11,12.1,12.3,13.6,
              14.3,14.2,14.4,13.8,14.3,13.8,15.4,15.1,15.5),
    id = as.factor(c("1","2","3","4","5",
                     "6","7","8","9","10","11","12","13","14",
                     "15","16","17","18","19","20","21","22","23",
                     "24","25","26","27","28","29","30","31","32",
                     "33","34","35","36","37","38","39","40","41",
                     "42","43","44","45","1","2","3","4","5",
                     "6","7","8","9","10","11","12","13","14","15",
                     "16","17","18","19","20","21","22","23","24",
                     "25","26","27","28","29","30","31","32",
                     "33","34","35","36","37","38","39","40","41",
                     "42","43","44","45","1","2","3","4","5","6",
                     "7","8","9","10","11","12","13","14","15",
                     "16","17","18","19","20","21","22","23","24",
                     "25","26","27","28","29","30","31","32","33",
                     "34","35","36","37","38","39","40","41","42",
                     "43","44","45")),
    group = as.factor(c("grp1","grp1","grp1",
                        "grp1","grp1","grp1","grp1","grp1","grp1",
                        "grp1","grp1","grp1","grp1","grp1","grp1","grp2",
                        "grp2","grp2","grp2","grp2","grp2","grp2","grp2",
                        "grp2","grp2","grp2","grp2","grp2","grp2",
                        "grp2","grp3","grp3","grp3","grp3","grp3","grp3",
                        "grp3","grp3","grp3","grp3","grp3","grp3",
                        "grp3","grp3","grp3","grp1","grp1","grp1","grp1",
                        "grp1","grp1","grp1","grp1","grp1","grp1","grp1",
                        "grp1","grp1","grp1","grp1","grp2","grp2",
                        "grp2","grp2","grp2","grp2","grp2","grp2","grp2",
                        "grp2","grp2","grp2","grp2","grp2","grp2","grp3",
                        "grp3","grp3","grp3","grp3","grp3","grp3",
                        "grp3","grp3","grp3","grp3","grp3","grp3","grp3",
                        "grp3","grp1","grp1","grp1","grp1","grp1",
                        "grp1","grp1","grp1","grp1","grp1","grp1","grp1",
                        "grp1","grp1","grp1","grp2","grp2","grp2","grp2",
                        "grp2","grp2","grp2","grp2","grp2","grp2",
                        "grp2","grp2","grp2","grp2","grp2","grp3","grp3",
                        "grp3","grp3","grp3","grp3","grp3","grp3","grp3",
                        "grp3","grp3","grp3","grp3","grp3","grp3")),
    time = as.factor(c("t1","t1","t1","t1",
                       "t1","t1","t1","t1","t1","t1","t1","t1","t1",
                       "t1","t1","t1","t1","t1","t1","t1","t1",
                       "t1","t1","t1","t1","t1","t1","t1","t1","t1",
                       "t1","t1","t1","t1","t1","t1","t1","t1","t1",
                       "t1","t1","t1","t1","t1","t1","t2","t2","t2",
                       "t2","t2","t2","t2","t2","t2","t2","t2","t2",
                       "t2","t2","t2","t2","t2","t2","t2","t2","t2",
                       "t2","t2","t2","t2","t2","t2","t2","t2",
                       "t2","t2","t2","t2","t2","t2","t2","t2","t2",
                       "t2","t2","t2","t2","t2","t2","t2","t3","t3",
                       "t3","t3","t3","t3","t3","t3","t3","t3","t3",
                       "t3","t3","t3","t3","t3","t3","t3","t3","t3",
                       "t3","t3","t3","t3","t3","t3","t3","t3","t3",
                       "t3","t3","t3","t3","t3","t3","t3","t3",
                       "t3","t3","t3","t3","t3","t3","t3","t3"))
  )

  # two way
  bxp <- ggboxplot(anxiety, x = "group", y = "score", color = "time") +
    stat_anova_test(aes(group = time), method = "two_way_mixed", wid = "id")
  bxp_build <- ggplot2::ggplot_build(bxp)
  stat.test <- bxp_build$data[[2]]
  res_mixed_anova <- data.frame(
    x  = as.numeric(stat.test$x),
    y  = as.numeric(stat.test$y),
    label = as.character(stat.test$label),
    stringsAsFactors = FALSE
  )
  res_mixed_anova_expected <- data.frame(x = 0.73, y = 20, label = "Anova, p < 0.0001", stringsAsFactors = FALSE)


  # Effect of group: Group by time and compute anova between x groups
  bxp <- ggboxplot(anxiety, x = "group", y = "score", color = "time") +
    stat_anova_test(aes(group = time, color = time), method = "one_way", group.by = "legend.var")
  bxp_build <- ggplot2::ggplot_build(bxp)
  stat.test <- bxp_build$data[[2]]
  res_group_effect <- data.frame(
    x  = as.numeric(stat.test$x),
    y  = as.numeric(stat.test$y),
    label = as.character(stat.test$label),
    stringsAsFactors = FALSE
  )
  res_group_effect_expected <- data.frame(x = c(0.73, 0.73, 0.73),
                                         y = c(20.0, 20.9, 21.8),
                                         label = c("Anova, p = 0.696", "Anova, p = 0.006", "Anova, p < 0.0001"),
                                         stringsAsFactors = FALSE)



  # Effect of time: group by x  groups and perform the test within group (here time)
  bxp <- ggboxplot(anxiety, x = "group", y = "score", color = "time") +
    stat_anova_test(aes(group = time),  method = "one_way_repeated", wid = "id", group.by = "x.var")
  bxp_build <- ggplot2::ggplot_build(bxp)
  stat.test <- bxp_build$data[[2]]
  res_time_effect <- data.frame(
    x  = as.numeric(stat.test$x),
    y  = as.numeric(stat.test$y),
    label = as.character(stat.test$label),
    stringsAsFactors = FALSE
  )
  res_time_effect_expected <- data.frame(x = 1:3,
                                         y = c(20, 20, 20),
                                         label = c("Anova, p < 0.0001", "Anova, p < 0.0001", "Anova, p < 0.0001"),
                                         stringsAsFactors = FALSE)

  expect_equal(res_mixed_anova,  res_mixed_anova_expected )
  expect_equal(res_group_effect, res_group_effect_expected )
  expect_equal(res_time_effect, res_time_effect_expected )
})


