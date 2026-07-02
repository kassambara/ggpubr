context("test-ggbarplot-numeric-x")

# #463: numeric.x.axis (already available in ggline/ggerrorplot) is now
# supported by ggbarplot. Default (FALSE) keeps the discrete-factor x axis.

x_is_discrete <- function(p) {
  ggplot2::ggplot_build(p)$layout$panel_scales_x[[1]]$is_discrete()
}

test_that("ggbarplot default keeps a discrete x axis (no regression, #463)", {
  df <- data.frame(x = c(1, 2, 5, 10), y = c(3, 5, 4, 8))
  expect_true(x_is_discrete(ggbarplot(df, "x", "y")))
})

test_that("ggbarplot numeric.x.axis = TRUE gives a numeric x axis (#463)", {
  df <- data.frame(x = c(1, 2, 5, 10), y = c(3, 5, 4, 8))
  p <- ggbarplot(df, "x", "y", numeric.x.axis = TRUE, width = 1)
  expect_false(x_is_discrete(p))
  expect_no_error(ggplot2::ggplot_gtable(ggplot2::ggplot_build(p)))
  # bars sit at their numeric x positions
  bx <- ggplot2::ggplot_build(p)$data[[1]]$x
  expect_equal(sort(bx), c(1, 2, 5, 10))
})

test_that("ggbarplot numeric.x.axis works with summary (mean_se) (#463)", {
  df <- data.frame(x = rep(c(1, 2, 5), each = 4), y = rnorm(12, 5))
  p <- ggbarplot(df, "x", "y", add = "mean_se", numeric.x.axis = TRUE, width = 0.8)
  expect_false(x_is_discrete(p))
  expect_no_error(ggplot2::ggplot_gtable(ggplot2::ggplot_build(p)))
})
