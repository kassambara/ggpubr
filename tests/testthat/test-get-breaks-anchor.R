# Regression tests for #313: yticks.by / xticks.by produced breaks anchored at
# the expanded (slightly negative) axis minimum -- e.g. -19, 81, 181 or
# -20, 80, 180 -- instead of round multiples 0, 100, 200. get_breaks() now
# anchors the first break to a multiple of `by`.

.ybreaks <- function(p) {
  stats::na.omit(ggplot2::ggplot_build(p)$layout$panel_params[[1]]$y$breaks)
}

.df <- data.frame(trt = factor(c("a", "b", "c")), value = c(120, 250, 380))

test_that("yticks.by gives round breaks anchored at 0, not the negative expansion (#313)", {
  p <- ggbarplot(.df, "trt", "value", fill = "grey", yticks.by = 100)
  yb <- .ybreaks(p)
  expect_true(all(yb %% 100 == 0))          # round multiples of 100
  expect_false(any(yb < 0))                 # no negative break label
  expect_true(0 %in% yb)
})

test_that("ylim + yticks.by shows 0..400 round breaks (the #313 report)", {
  p <- ggbarplot(.df, "trt", "value", fill = "grey", ylim = c(0, 400), yticks.by = 100)
  yb <- .ybreaks(p)
  expect_setequal(yb, c(0, 100, 200, 300, 400))
})

test_that("get_breaks() anchors the first break to a multiple of `by` (#313)", {
  f <- get_breaks(by = 100)
  # expanded range dipping below 0 -> breaks still land on 0, 100, ...
  expect_true(all(f(c(-19, 399)) %% 100 == 0))
  expect_true(0 %in% f(c(-19, 399)))
  # genuinely negative data -> still round multiples
  expect_true(all(f(c(-50, 380)) %% 100 == 0))
})

test_that("strictly-positive-data breaks are unchanged (no regression, #313/#333)", {
  # xticks.by on positive scatter data: visible breaks identical to before
  p <- ggscatter(iris, x = "Sepal.Length", y = "Sepal.Width", xticks.by = 0.5)
  xb <- stats::na.omit(ggplot2::ggplot_build(p)$layout$panel_params[[1]]$x$breaks)
  expect_true(all(abs(diff(xb) - 0.5) < 1e-8))
  expect_true(min(xb) >= 4 && max(xb) <= 8)
})
