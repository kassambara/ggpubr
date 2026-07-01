# Regression test for #652: legend.direction was ignored for legend = "bottom"/"top"
# (it only took effect via ggplot2 defaults for "left"/"right").

.df <- transform(mtcars, cyl = as.factor(cyl))

test_that("legend.direction is applied with legend='bottom' (#652)", {
  p <- ggscatter(.df, x = "wt", y = "mpg", color = "cyl",
                 legend = "bottom", legend.direction = "vertical")
  expect_equal(p$theme$legend.direction, "vertical")
  expect_no_error(ggplot2::ggplot_gtable(ggplot2::ggplot_build(p)))
})

test_that("legend.direction still works with legend='right' (#652)", {
  p <- ggscatter(.df, x = "wt", y = "mpg", color = "cyl",
                 legend = "right", legend.direction = "horizontal")
  expect_equal(p$theme$legend.direction, "horizontal")
})

test_that("omitting legend.direction leaves it unset (no regression, #652)", {
  p <- ggscatter(.df, x = "wt", y = "mpg", color = "cyl", legend = "bottom")
  expect_null(p$theme$legend.direction)
  expect_no_error(ggplot2::ggplot_gtable(ggplot2::ggplot_build(p)))
})
