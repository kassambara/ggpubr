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

# Regression tests for #653: stat_regline_equation() displayed the orthogonal
# poly() coefficients as if they were raw polynomial coefficients, giving a
# wrong equation. It now refits with raw = TRUE for display.

test_that("stat_regline_equation shows the correct raw polynomial for poly(x, 2) (#653)", {
  test_db <- data.frame(
    x = c(0, -0.1297, -0.2185, -0.2795),
    y = c(0.7569, 0.7396, 0.6561, 0.5476)
  )
  eq <- function(formula) {
    p <- ggplot2::ggplot(test_db, ggplot2::aes(x, y)) + ggplot2::geom_point() +
      stat_regline_equation(
        ggplot2::aes(label = ggplot2::after_stat(eq.label)),
        formula = formula, coef.digits = 3
      )
    as.character(ggplot2::ggplot_build(p)$data[[2]]$label)
  }
  # correct raw equation: y = -4.24 x^2 - 0.447 x + 0.756
  lab.orth <- eq(y ~ poly(x, 2))
  expect_match(lab.orth, "-4.24", fixed = TRUE)
  expect_match(lab.orth, "0.756", fixed = TRUE)
  # orthogonal poly() now matches raw = TRUE (same fitted curve)
  expect_identical(lab.orth, eq(y ~ poly(x, 2, raw = TRUE)))
  # the buggy orthogonal coefficient (~ -0.0716) must NOT appear
  expect_false(grepl("0.0716", lab.orth, fixed = TRUE))
})

test_that("stat_regline_equation linear/raw/I() equations are unchanged (#653)", {
  d <- .set()
  # y ~ x has no poly() -> untouched
  expect_match(.eq_label(d), "3.4")
  # helper: rewrite only orthogonal poly()
  expect_null(.polynomial_raw_formula(y ~ x))
  expect_null(.polynomial_raw_formula(y ~ poly(x, 2, raw = TRUE)))
  expect_null(.polynomial_raw_formula(y ~ x + I(x^2)))
  expect_equal(
    deparse(.polynomial_raw_formula(y ~ poly(x, 2))),
    "y ~ poly(x, 2, raw = TRUE)"
  )
  expect_equal(
    deparse(.polynomial_raw_formula(y ~ poly(x, 3, raw = FALSE))),
    "y ~ poly(x, 3, raw = TRUE)"
  )
})

test_that("stat_regline_equation skips unsafe poly rewrites (#653)", {
  # No-intercept: orthogonal poly() != raw poly() fit -> leave as-is
  expect_null(.polynomial_raw_formula(y ~ poly(x, 2) - 1))
  # Transformed poly argument: cannot rewrite reliably -> leave as-is
  expect_null(.polynomial_raw_formula(y ~ poly(log(x), 2)))
  # These still render without error (fall back to original coefficients)
  d <- data.frame(x = 1:20, y = (1:20)^2 + rnorm(20))
  p <- ggplot2::ggplot(d, ggplot2::aes(x, y)) + ggplot2::geom_point() +
    stat_regline_equation(formula = y ~ poly(x, 2) - 1)
  expect_no_error(ggplot2::ggplot_gtable(ggplot2::ggplot_build(p)))
})
