test_that("comparison labels preserve per-segment aesthetics", {
  d <- ToothGrowth
  d$dose <- factor(d$dose)
  comparisons <- list(c("0.5", "1"), c("1", "2"))
  p <- ggboxplot(d, "dose", "len") + stat_compare_means(
    comparisons = comparisons,
    color = rep(c("red", "blue"), 3),
    family = rep(c("serif", "mono"), 3)
  )
  layer <- ggplot2::layer_data(p, 2)
  expect_identical(
    list(colour = sort(unique(layer$colour)), family = sort(unique(layer$family))),
    list(colour = c("blue", "red"), family = c("mono", "serif"))
  )
})


test_that("comparisons ignore duplicate lower-level ggsignif spellings", {
  p <- ggplot(ToothGrowth, aes(factor(dose), len)) +
    geom_boxplot() +
    stat_compare_means(
      comparisons = list(c("0.5", "1")),
      textsize = 9, y_position = 40
    )

  expect_no_error(ggplot2::ggplot_build(p))
})
test_that("comparison path forwards documented geometry and layer controls", {
  d <- ToothGrowth
  d$dose <- factor(d$dose)
  p <- ggboxplot(d, "dose", "len") + stat_compare_means(
    comparisons = list(c("0.5", "1")),
    fontface = "bold", angle = 45, alpha = 0.2,
    show.legend = FALSE, position = "identity", na.rm = TRUE
  )
  layer.data <- ggplot2::layer_data(p, 2)
  layer <- p$layers[[2]]
  expect_identical(
    list(
      fontface = unique(layer.data$fontface), angle = unique(layer.data$angle),
      alpha = unique(layer.data$alpha), show.legend = layer$show.legend,
      position = class(layer$position)[1], na.rm = layer$geom_params$na.rm
    ),
    list(
      fontface = "bold", angle = 45, alpha = 0.2, show.legend = FALSE,
      position = "PositionIdentity", na.rm = TRUE
    )
  )
})
