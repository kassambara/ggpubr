# Regression test for #517: ggboxplot() ignored `coef` (it was not in geom_exec's
# allowed_options), so `coef = 0` could not be used to omit the whiskers.

.nowhisker <- function(p) {
  d <- ggplot2::ggplot_build(p)$data[[1]]
  all(abs(d$ymin - d$lower) < 1e-9 & abs(d$ymax - d$upper) < 1e-9)
}

test_that("ggboxplot(coef = 0) omits the whiskers (#517)", {
  p <- ggboxplot(ToothGrowth, "dose", "len", coef = 0)
  expect_true(.nowhisker(p))
  expect_no_error(ggplot2::ggplot_gtable(ggplot2::ggplot_build(p)))
})

test_that("ggboxplot() default keeps whiskers (no regression, #517)", {
  p <- ggboxplot(ToothGrowth, "dose", "len")
  expect_false(.nowhisker(p))
  # coef = 1.5 (the geom_boxplot default) is byte-identical to not passing coef
  d0 <- ggplot2::ggplot_build(p)$data[[1]]
  d1 <- ggplot2::ggplot_build(ggboxplot(ToothGrowth, "dose", "len", coef = 1.5))$data[[1]]
  expect_equal(d0$ymin, d1$ymin)
  expect_equal(d0$ymax, d1$ymax)
})
