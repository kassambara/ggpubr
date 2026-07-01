# Regression tests for #333: when BOTH xticks.by and yticks.by were supplied,
# .set_ticksby() used an `else if`, so xticks.by was silently ignored.

.x_breaks <- function(p) {
  b <- ggplot2::ggplot_build(p)
  stats::na.omit(b$layout$panel_params[[1]]$x$breaks)
}
.y_breaks <- function(p) {
  b <- ggplot2::ggplot_build(p)
  stats::na.omit(b$layout$panel_params[[1]]$y$breaks)
}

test_that("xticks.by and yticks.by both apply when supplied together (#333)", {
  p <- ggscatter(iris, x = "Sepal.Length", y = "Sepal.Width",
                 xticks.by = 0.5, yticks.by = 0.5)
  xb <- .x_breaks(p)
  yb <- .y_breaks(p)
  # 0.5 spacing on both axes
  expect_true(all(abs(diff(xb) - 0.5) < 1e-8))
  expect_true(all(abs(diff(yb) - 0.5) < 1e-8))
})

test_that("xticks.by alone is unchanged (no regression, #333)", {
  p_both  <- ggscatter(iris, x = "Sepal.Length", y = "Sepal.Width", xticks.by = 0.5)
  xb <- .x_breaks(p_both)
  expect_true(all(abs(diff(xb) - 0.5) < 1e-8))
})

test_that("yticks.by alone is unchanged (no regression, #333)", {
  p <- ggscatter(iris, x = "Sepal.Length", y = "Sepal.Width", yticks.by = 0.5)
  yb <- .y_breaks(p)
  expect_true(all(abs(diff(yb) - 0.5) < 1e-8))
  # x axis keeps ggplot2 default breaks (integers here), i.e. NOT 0.5-spaced
  xb <- .x_breaks(p)
  expect_false(isTRUE(all(abs(diff(xb) - 0.5) < 1e-8)))
})

test_that("xticks.by on a discrete x axis does not error, yticks.by still applies (#333)", {
  # ggboxplot has a discrete x: applying a continuous x scale would error, so
  # xticks.by is skipped (as it was silently before), while yticks.by still
  # works. Must RENDER (draw-time), not just build.
  p <- ggboxplot(ToothGrowth, x = "supp", y = "len",
                 xticks.by = 1, yticks.by = 5)
  expect_no_error(grob <- ggplot2::ggplot_gtable(ggplot2::ggplot_build(p)))
  expect_s3_class(grob, "gtable")
  yb <- .y_breaks(p)
  expect_true(all(abs(diff(yb) - 5) < 1e-8))
})

test_that(".is_discrete_x detects discrete vs continuous x (#333)", {
  # numeric x -> continuous -> not discrete
  expect_false(ggpubr:::.is_discrete_x(
    ggscatter(iris, x = "Sepal.Length", y = "Sepal.Width")))
  # factor x -> discrete
  expect_true(ggpubr:::.is_discrete_x(
    ggboxplot(ToothGrowth, x = "supp", y = "len")))
  # Date x is continuous (numeric storage) -> not discrete, so xticks.by still
  # applies exactly as in previous versions.
  df <- data.frame(d = as.Date("2020-01-01") + 0:9, y = seq_len(10))
  pd <- ggplot2::ggplot(df, ggplot2::aes(x = d, y = y)) + ggplot2::geom_point()
  expect_false(ggpubr:::.is_discrete_x(pd))
})
