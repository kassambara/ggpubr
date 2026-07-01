test_that("ggmaplot line.color controls the threshold line color (#322)", {
  data("diff_express", package = "ggpubr")
  hline_color <- function(p) {
    b <- ggplot2::ggplot_build(p)
    i <- which(sapply(b$data, function(d) "yintercept" %in% names(d)))[1]
    unique(b$data[[i]]$colour)
  }
  # No-regression: default threshold lines are black
  expect_equal(hline_color(ggmaplot(diff_express)), "black")
  # Regression: line.color changes them
  expect_equal(hline_color(ggmaplot(diff_express, line.color = "red")), "red")
})
