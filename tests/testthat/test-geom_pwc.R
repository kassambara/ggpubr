context("test-geom_pwc")

# Data preparation
df <- ToothGrowth
df$dose <- as.factor(df$dose)
df$group <- factor(rep(c("grp1", "grp2"), 30))


# Basic plots -----------------------------------
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


# Grouped plots -----------------------------------

test_that("Grouped plots: Two groups by x position", {
  bxp <- ggboxplot(df, x = "dose", y = "len", color = "supp") +
    geom_pwc(aes(group = supp), method = "wilcox_test")
  bxp_build <- ggplot2::ggplot_build(bxp)
  stat.test <- bxp_build$data[[2]]
  label_coords <- as.data.frame(get_pwc_label_coords(stat.test))
  label_coords_expected <- data.frame(
    x = c(1, 2, 3),
    y = c(35.395, 35.395, 35.395),
    angle = c(0, 0, 0)
  )
  expect_equal(stat.test$group, c(1L, 2L, 3L, 1L, 2L, 3L, 1L, 2L, 3L))
  expect_equal(stat.test$bracket.group, c(1L, 1L, 1L, 1L, 1L, 1L, 1L, 1L, 1L))
  expect_equal(as.numeric(stat.test$x), c(0.8, 1.8, 2.8, 0.8, 1.8, 2.8, 1.2, 2.2, 3.2))
  expect_equal(as.numeric(stat.test$xend), c(0.8, 1.8, 2.8, 1.2, 2.2, 3.2, 1.2, 2.2, 3.2))
  expect_equal(stat.test$y, c(34.494, 34.494, 34.494, 35.385, 35.385, 35.385, 35.385, 35.385, 35.385))
  expect_equal(stat.test$yend, c(35.385, 35.385, 35.385, 35.385, 35.385, 35.385, 34.494, 34.494, 34.494))
  expect_equal(label_coords, label_coords_expected)
})


test_that("Grouped plots: grouping by legend var and comparing x-axis groups", {
  bxp <- ggboxplot(df, x = "dose", y = "len", color = "supp") +
    geom_pwc(aes(group = supp, color = supp), group.by = "legend.var")
  bxp_build <- ggplot2::ggplot_build(bxp)
  stat.test <- bxp_build$data[[2]]
  label_coords <- as.data.frame(get_pwc_label_coords(stat.test))
  label_coords_expected <- data.frame(
    x = c(1.3, 1.8, 2.3, 1.7, 2.2, 2.7),
    y = c(35.395, 38.959, 42.523, 46.087, 49.651, 53.215),
    angle = c(0, 0, 0, 0, 0, 0)
  )
  expect_equal(stat.test$group, c(1L, 1L, 1L, 2L, 2L, 2L, 1L, 1L, 1L, 2L, 2L, 2L, 1L, 1L, 1L, 2L, 2L, 2L))
  expect_equal(stat.test$bracket.group, c(1L, 2L, 3L, 4L, 5L, 6L, 1L, 2L, 3L, 4L, 5L, 6L, 1L, 2L, 3L, 4L, 5L, 6L))
  expect_equal(as.numeric(stat.test$x), c(0.8, 0.8, 1.8, 1.2, 1.2, 2.2, 0.8, 0.8, 1.8, 1.2, 1.2, 2.2, 1.8, 2.8, 2.8, 2.2, 3.2, 3.2))
  expect_equal(as.numeric(stat.test$xend), c(0.8, 0.8, 1.8, 1.2, 1.2, 2.2, 1.8, 2.8, 2.8, 2.2, 3.2, 3.2, 1.8, 2.8, 2.8, 2.2, 3.2, 3.2))
  expect_equal(stat.test$y, c(34.494, 38.058, 41.622, 45.186, 48.75, 52.314, 35.385, 38.949, 42.513, 46.077, 49.641, 53.205, 35.385, 38.949, 42.513, 46.077, 49.641, 53.205))
  expect_equal(stat.test$yend, c(35.385, 38.949, 42.513, 46.077, 49.641, 53.205, 35.385, 38.949, 42.513, 46.077, 49.641, 53.205, 34.494, 38.058, 41.622, 45.186, 48.75, 52.314))
  expect_equal(label_coords, label_coords_expected)
})


test_that("Grouped plots: grouping by legend var and comparing x-axis groups, using dodge=0 and bracket.group.by = 'legend.var'", {
  bxp <- ggboxplot(df, x = "dose", y = "len", color = "supp") +
    geom_pwc(
      aes(group = supp, color = supp), group.by = "legend.var",
      dodge = 0, bracket.group.by = "legend.var"
      )
  bxp_build <- ggplot2::ggplot_build(bxp)
  stat.test <- bxp_build$data[[2]]
  label_coords <- as.data.frame(get_pwc_label_coords(stat.test))
  label_coords_expected <- data.frame(
    x = c(1.5, 1.5, 2, 2, 2.5, 2.5),
    y = c(35.395, 38.959, 42.523, 46.087, 49.651, 53.215),
    angle = c(0, 0, 0, 0, 0, 0)
  )
  expect_equal(stat.test$group, c(1L, 2L, 1L, 2L, 1L, 2L, 1L, 2L, 1L, 2L, 1L, 2L, 1L, 2L, 1L, 2L, 1L, 2L))
  expect_equal(stat.test$bracket.group, c(1L, 2L, 3L, 4L, 5L, 6L, 1L, 2L, 3L, 4L, 5L, 6L, 1L, 2L, 3L, 4L, 5L, 6L))
  expect_equal(as.numeric(stat.test$x), c(1, 1, 1, 1, 2, 2, 1, 1, 1, 1, 2, 2, 2, 2, 3, 3, 3, 3))
  expect_equal(as.numeric(stat.test$xend), c(1, 1, 1, 1, 2, 2, 2, 2, 3, 3, 3, 3, 2, 2, 3, 3, 3, 3))
  expect_equal(stat.test$y, c(34.494, 38.058, 41.622, 45.186, 48.75, 52.314, 35.385, 38.949, 42.513, 46.077, 49.641, 53.205, 35.385, 38.949, 42.513, 46.077, 49.641, 53.205))
  expect_equal(stat.test$yend, c(35.385, 38.949, 42.513, 46.077, 49.641, 53.205, 35.385, 38.949, 42.513, 46.077, 49.641, 53.205, 34.494, 38.058, 41.622, 45.186, 48.75, 52.314))
  expect_equal(label_coords, label_coords_expected)
})


test_that("Grouped plots: 3 groups at x position. All Pairwise comparisons.", {
  bxp <- ggboxplot(df, x = "supp", y = "len", color = "dose") +
    geom_pwc(aes(group = dose))
  bxp_build <- ggplot2::ggplot_build(bxp)
  stat.test <- bxp_build$data[[2]]
  label_coords <- as.data.frame(get_pwc_label_coords(stat.test))
  label_coords$x <- round(label_coords$x, 2)
  label_coords$y <- round(label_coords$y, 2)
  label_coords_expected <- data.frame(
    x = c(0.87, 1, 1.13, 1.87, 2, 2.13),
    y = c(35.39, 38.96, 42.52, 35.39, 38.96, 42.52),
    angle = c(0, 0, 0, 0, 0, 0)
  )

  stat.test$x <- round(stat.test$x, 2)
  stat.test$xend <- round(stat.test$xend, 2)
  stat.test$y <- round(stat.test$y, 2)
  stat.test$yend <- round(stat.test$yend, 2)

  expect_equal(stat.test$group, c(1L, 2L, 3L, 4L, 5L, 6L, 1L, 2L, 3L, 4L, 5L, 6L, 1L, 2L, 3L, 4L, 5L, 6L))
  expect_equal(stat.test$bracket.group, c(1L, 2L, 3L, 1L, 2L, 3L, 1L, 2L, 3L, 1L, 2L, 3L, 1L, 2L, 3L, 1L, 2L, 3L))
  expect_equal(as.numeric(stat.test$x), c(0.73, 0.73, 1, 1.73, 1.73, 2, 0.73, 0.73, 1, 1.73, 1.73, 2, 1, 1.27, 1.27, 2, 2.27, 2.27))
  expect_equal(as.numeric(stat.test$xend), c(0.73, 0.73, 1, 1.73, 1.73, 2, 1, 1.27, 1.27, 2, 2.27, 2.27, 1, 1.27, 1.27, 2, 2.27, 2.27))
  expect_equal(stat.test$y, c(34.49, 38.06, 41.62, 34.49, 38.06, 41.62, 35.38, 38.95, 42.51, 35.38, 38.95, 42.51, 35.38, 38.95, 42.51, 35.38, 38.95, 42.51))
  expect_equal(stat.test$yend, c(35.38, 38.95, 42.51, 35.38, 38.95, 42.51, 35.38, 38.95, 42.51, 35.38, 38.95, 42.51, 34.49, 38.06, 41.62, 34.49, 38.06, 41.62))
  expect_equal(label_coords, label_coords_expected)
})


test_that("Grouped plots: 3 groups at x position. Pairwise comparisons against a ref group.", {
  bxp <- ggboxplot(df, x = "supp", y = "len", color = "dose") +
    geom_pwc(aes(group = dose), ref.group = 1)
  bxp_build <- ggplot2::ggplot_build(bxp)
  stat.test <- bxp_build$data[[2]]
  label_coords <- as.data.frame(get_pwc_label_coords(stat.test))
  label_coords$x <- round(label_coords$x, 2)
  label_coords$y <- round(label_coords$y, 2)
  label_coords_expected <- data.frame(
    x = c(0.87, 1, 1.87, 2),
    y = c(35.39, 38.96, 35.39, 38.96),
    angle = c(0, 0, 0, 0)
  )

  stat.test$x <- round(stat.test$x, 2)
  stat.test$xend <- round(stat.test$xend, 2)
  stat.test$y <- round(stat.test$y, 2)
  stat.test$yend <- round(stat.test$yend, 2)

  expect_equal(stat.test$group, c(1L, 2L, 3L, 4L, 1L, 2L, 3L, 4L, 1L, 2L, 3L, 4L))
  expect_equal(stat.test$bracket.group, c(1L, 2L, 1L, 2L, 1L, 2L, 1L, 2L, 1L, 2L, 1L, 2L))
  expect_equal(as.numeric(stat.test$x), c(0.73, 0.73, 1.73, 1.73, 0.73, 0.73, 1.73, 1.73, 1, 1.27, 2, 2.27))
  expect_equal(as.numeric(stat.test$xend), c(0.73, 0.73, 1.73, 1.73, 1, 1.27, 2, 2.27, 1, 1.27, 2, 2.27))
  expect_equal(stat.test$y, c(34.49, 38.06, 34.49, 38.06, 35.38, 38.95, 35.38, 38.95, 35.38, 38.95, 35.38, 38.95))
  expect_equal(stat.test$yend, c(35.38, 38.95, 35.38, 38.95, 35.38, 38.95, 35.38, 38.95, 34.49, 38.06, 34.49, 38.06))
  expect_equal(label_coords, label_coords_expected)
})

test_that("Grouped plots: 3 groups at x position. Pairwise comparisons against a ref group. Check label coords when remove.bracket=TRUE.", {
  bxp <- ggboxplot(df, x = "supp", y = "len", color = "dose") +
    geom_pwc(aes(group = dose), ref.group = 1, remove.bracket = TRUE)
  bxp_build <- ggplot2::ggplot_build(bxp)
  stat.test <- bxp_build$data[[2]]
  label_coords <- as.data.frame(get_pwc_label_coords(stat.test))
  label_coords$x <- round(label_coords$x, 2)
  label_coords$y <- round(label_coords$y, 2)
  label_coords_expected <- data.frame(
    x = c(1, 1.27, 2, 2.27),
    y = c(35.39, 38.96, 35.39, 38.96),
    angle = c(0, 0, 0, 0)
  )
  expect_equal(label_coords, label_coords_expected)
})


test_that("Grouped plots: test that geom_pwc() works with different number of groups at each x pos.", {
  # https://github.com/kassambara/ggpubr/issues/326
  demo_data <- data.frame(
    stringsAsFactors = FALSE,
    Study = c("A","A","A","A","A","A",
              "A","A","A","A","B","B","B","B","B","B","B","B",
              "B","B","C","C","C","C","C","C","C","C","C",
              "C","C","C","C","C","C","D","D","D","D","D",
              "D","D","D","D","D","D","D","D","D","D"),
    Studytype = c("X","X","X","X","X","X",
                  "X","X","X","X","X","X","X","X","X","X","X","X",
                  "X","X","Y","Y","Y","Y","Y","Y","Y","Y","Y",
                  "Y","Y","Y","Y","Y","Y","Y","Y","Y","Y","Y",
                  "Y","Y","Y","Y","Y","Y","Y","Y","Y","Y"),
    Values = c(4469L,4797L,5101L,5397L,
               4542L,2780L,4326L,3396L,3657L,3199L,9221L,10176L,
               9277L,10500L,9707L,7406L,7756L,7601L,7586L,7353L,
               1811L,1485L,3003L,1629L,2495L,4207L,4265L,3629L,
               4157L,3495L,2075L,2112L,2973L,3086L,2943L,5664L,6690L,
               3538L,5741L,7880L,5848L,6390L,6569L,6114L,6520L,
               7389L,6843L,7611L,6621L,7340L),
    Group = as.factor(c("CTR",
                        "CTR","CTR","CTR","CTR","Dis1","Dis1","Dis1",
                        "Dis1","Dis1","CTR","CTR","CTR","CTR",
                        "CTR","Dis1","Dis1","Dis1","Dis1","Dis1",
                        "CTR","CTR","CTR","CTR","CTR","Dis2","Dis2",
                        "Dis2","Dis2","Dis2","Dis3","Dis3",
                        "Dis3","Dis3","Dis3","CTR","CTR","CTR","CTR",
                        "CTR","Dis2","Dis2","Dis2","Dis2","Dis2",
                        "Dis3","Dis3","Dis3","Dis3","Dis3"))
  )
  bxp <- ggboxplot(demo_data, x = "Study", y = "Values", fill = "Group") +
    geom_pwc(aes(group = Group), dodge = 0.8, label = "p", ref.group = 1)
  bxp_build <- ggplot2::ggplot_build(bxp)
  stat.test <- bxp_build$data[[2]]
  label_coords <- as.data.frame(get_pwc_label_coords(stat.test))
  label_coords$x <- round(label_coords$x, 2)
  label_coords$y <- round(label_coords$y, 2)
  label_coords_expected <- data.frame(
    x = c(1, 2, 2.87, 3, 3.87, 4),
    y = c(10950.76, 10950.76, 10950.76, 12032.56, 10950.76, 12032.56),
    angle = c(0, 0, 0, 0, 0, 0)
  )

  stat.test$x <- round(stat.test$x, 2)
  stat.test$xend <- round(stat.test$xend, 2)
  stat.test$y <- round(stat.test$y, 2)
  stat.test$yend <- round(stat.test$yend, 2)

  expect_equal(stat.test$group, c(1L, 2L, 3L, 4L, 5L, 6L, 1L, 2L, 3L, 4L, 5L, 6L, 1L, 2L, 3L, 4L, 5L, 6L))
  expect_equal(stat.test$bracket.group, c(1L, 1L, 1L, 2L, 1L, 2L, 1L, 1L, 1L, 2L, 1L, 2L, 1L, 1L, 1L, 2L, 1L, 2L))
  expect_equal(as.numeric(stat.test$x), c(0.8, 1.8, 2.73, 2.73, 3.73, 3.73, 0.8, 1.8, 2.73, 2.73, 3.73, 3.73, 1.2, 2.2, 3, 3.27, 4, 4.27))
  expect_equal(as.numeric(stat.test$xend), c(0.8, 1.8, 2.73, 2.73, 3.73, 3.73, 1.2, 2.2, 3, 3.27, 4, 4.27, 1.2, 2.2, 3, 3.27, 4, 4.27))
  expect_equal(stat.test$y, c(10680.3, 10680.3, 10680.3, 11762.1, 10680.3, 11762.1, 10950.75, 10950.75, 10950.75, 12032.55, 10950.75, 12032.55, 10950.75, 10950.75, 10950.75, 12032.55, 10950.75, 12032.55))
  expect_equal(stat.test$yend, c(10950.75, 10950.75, 10950.75, 12032.55, 10950.75, 12032.55, 10950.75, 10950.75, 10950.75, 12032.55, 10950.75, 12032.55, 10680.3, 10680.3, 10680.3, 11762.1, 10680.3, 11762.1))
  expect_equal(label_coords, label_coords_expected)
})
