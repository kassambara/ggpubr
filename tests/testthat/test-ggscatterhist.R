context("test-ggscatterhist")

# Regression tests for #220 / #420: the marginal plots were built from the raw
# data, so transformations that change the scatter limits (ellipse, position
# jitter, explicit limits) were not reflected in the margins, leaving them
# misaligned with the scatter. The margins now inherit the scatter's ranges.

panel_x_range <- function(p) {
  pp <- ggplot2::ggplot_build(p)$layout$panel_params[[1]]
  r <- pp[["x.range"]]
  if (is.null(r)) r <- pp[["x"]]$continuous_range
  r
}
panel_y_range <- function(p) {
  pp <- ggplot2::ggplot_build(p)$layout$panel_params[[1]]
  r <- pp[["y.range"]]
  if (is.null(r)) r <- pp[["y"]]$continuous_range
  r
}

test_that("ggscatterhist marginal plots inherit the scatter's ranges with jitter (#420)", {
  set.seed(1)
  res <- ggscatterhist(
    mpg, x = "displ", y = "hwy",
    position = ggplot2::position_jitter(width = 5, height = 5, seed = 1),
    print = FALSE
  )
  sp.x <- panel_x_range(res$sp)
  sp.y <- panel_y_range(res$sp)
  # xplot (top margin) x-axis matches the scatter x-axis
  expect_equal(panel_x_range(res$xplot), sp.x, tolerance = 1e-6)
  # yplot is coord_flip()'d, so its y-axis (post-flip) matches the scatter y-axis
  expect_equal(panel_y_range(res$yplot), sp.y, tolerance = 1e-6)
  # the jittered scatter range is much wider than the raw data range: the fix
  # must have widened the margin beyond the raw displ range (1.6-7.0)
  expect_lt(sp.x[1], 1.6)
  expect_gt(sp.x[2], 7.0)
})

test_that("ggscatterhist marginal plots inherit the scatter's ranges with ellipse (#220)", {
  res <- ggscatterhist(
    iris, x = "Sepal.Length", y = "Sepal.Width",
    color = "Species", ellipse = TRUE, print = FALSE
  )
  expect_equal(panel_x_range(res$xplot), panel_x_range(res$sp), tolerance = 1e-6)
  expect_equal(panel_y_range(res$yplot), panel_y_range(res$sp), tolerance = 1e-6)
})

test_that("ggscatterhist default still renders and margins align (#220/#420)", {
  res <- ggscatterhist(iris, x = "Sepal.Length", y = "Sepal.Width", print = FALSE)
  expect_s3_class(res, "ggscatterhist")
  expect_equal(panel_x_range(res$xplot), panel_x_range(res$sp), tolerance = 1e-6)
  expect_equal(panel_y_range(res$yplot), panel_y_range(res$sp), tolerance = 1e-6)
  expect_no_error(print(res))
})

test_that("ggscatterhist does NOT force a transformed (log) scatter range onto the raw-data margin (#220/#420)", {
  # A log-scaled scatter axis lives in different units than the raw-data margin.
  # The margin must stay in linear data units (visible), NOT be forced to the
  # scatter's log range (which would push it off-screen). Regression guard.
  res <- ggscatterhist(mpg, x = "displ", y = "hwy", xscale = "log10", print = FALSE)
  xplot.range <- panel_x_range(res$xplot)
  # margin still spans (roughly) the raw displ range 1.6..7.0, not the log range ~0.2..0.9
  expect_gt(xplot.range[2], 5)
  expect_gt(xplot.range[1], 1)
  expect_false(isTRUE(all.equal(xplot.range, panel_x_range(res$sp))))
  expect_no_error(print(res))
})

test_that("ggscatterhist preserves the marginal density-axis expansion (no default change, #220/#420)", {
  # Aligning the DATA axis must not remove the count/density-axis expansion:
  # the density peak should not sit flush against the panel edge.
  res <- ggscatterhist(iris, x = "Sepal.Length", y = "Sepal.Width", print = FALSE)
  dens.range <- panel_y_range(res$xplot)   # xplot's y = density magnitude
  built <- ggplot2::ggplot_build(res$xplot)$data[[1]]
  expect_lt(dens.range[1], 0)              # bottom expansion below 0 retained
  expect_gt(dens.range[2], max(built$y))   # top gap above the peak retained
})
