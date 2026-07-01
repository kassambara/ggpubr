# Regression tests for #312: stat_regline_equation() hardcoded signif(., 2) for
# the equation coefficients and R^2. New coef.digits / rr.digits control the
# number of significant digits; defaults (2) reproduce the previous output.

.set <- function() {
  set.seed(1)
  data.frame(x = 1:20, y = -3.3462 + 1.02 * (1:20) + rnorm(20))
}
.eq_label <- function(d, aes.label = ggplot2::aes(label = ggplot2::after_stat(eq.label)), ...) {
  p <- ggplot2::ggplot(d, ggplot2::aes(x, y)) + ggplot2::geom_point() +
    stat_regline_equation(aes.label, ...)
  as.character(ggplot2::ggplot_build(p)$data[[2]]$label)
}
.rr_label <- function(d, ...) {
  p <- ggplot2::ggplot(d, ggplot2::aes(x, y)) + ggplot2::geom_point() +
    stat_regline_equation(ggplot2::aes(label = ggplot2::after_stat(rr.label)), ...)
  as.character(ggplot2::ggplot_build(p)$data[[2]]$label)
}

test_that("default digits are unchanged (no regression, #312)", {
  d <- .set()
  # default equals explicit coef.digits = 2 / rr.digits = 2
  expect_identical(.eq_label(d), .eq_label(d, coef.digits = 2))
  expect_identical(.rr_label(d), .rr_label(d, rr.digits = 2))
  # the default equation is the 2-sig-fig form (slope 1.0 hidden by polynom)
  expect_match(.eq_label(d), "3.4")
})

test_that("coef.digits increases equation coefficient precision (#312)", {
  d <- .set()
  lab3 <- .eq_label(d, coef.digits = 3)
  # slope becomes visible at 3 sig figs and intercept shows more digits
  expect_match(lab3, "1.04")
  expect_match(lab3, "3.38")
  expect_false(identical(.eq_label(d), lab3))
})

test_that("rr.digits controls R^2 precision (#312)", {
  d <- .set()
  expect_match(.rr_label(d, rr.digits = 4), "0.9789")
  expect_false(identical(.rr_label(d, rr.digits = 2), .rr_label(d, rr.digits = 4)))
})
