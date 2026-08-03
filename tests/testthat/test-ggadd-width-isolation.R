test_that("ggadd keeps box and summary error widths independent", {
  d <- ToothGrowth
  d$dose <- factor(d$dose)
  p <- ggviolin(d, "dose", "len", add = "none")
  p <- ggadd(p, c("boxplot", "mean_se"), error.plot = "errorbar")
  built <- ggplot2::ggplot_build(p)
  widths <- lapply(built$data[2:3], function(layer) sort(unique(round(layer$xmax - layer$xmin, 6))))
  expect_identical(widths, list(0.2, 0.1))
})
