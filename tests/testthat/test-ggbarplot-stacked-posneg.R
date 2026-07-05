context("test-ggbarplot-stacked-posneg")

# #426: for STACKED bars (default position = "stack"), ggbarplot() placed every
# error bar with a single cumulative sum, which assumes all stacked segments share
# one sign. position_stack() accumulates positive and negative segments SEPARATELY,
# so with mixed signs the error bars of one sign were drawn on the wrong side (and
# which one looked correct flipped with the factor-level order). Error bars are now
# cumulated per sign, matching position_stack(). Single-sign data is unchanged.

suppressPackageStartupMessages(library(ggplot2))

# self-contained data with KNOWN per-cell means (two identical rows per cell so
# the mean is exact); group g1 is negative, g2/g3 positive.
.mk_posneg <- function(sign1 = -1) {
  vals <- c(
    # x = A
    A_g1 = 10 * sign1, A_g2 = 5,  A_g3 = 8,
    # x = B
    B_g1 = 6  * sign1, B_g2 = 4,  B_g3 = 7
  )
  d <- do.call(rbind, lapply(names(vals), function(k) {
    xg <- strsplit(k, "_")[[1]]
    data.frame(x = xg[1], g = xg[2], y = rep(vals[[k]], 2))
  }))
  d$x <- factor(d$x); d$g <- factor(d$g)
  d
}

# error-bar centers per x position (sorted), from the built plot
.ebar_centers_by_x <- function(p) {
  b <- suppressWarnings(ggplot2::ggplot_build(p))
  ei <- which(vapply(p$layers, function(l) inherits(l$geom, "GeomErrorbar"), logical(1)))[1]
  d <- b$data[[ei]]
  d$center <- (d$ymin + d$ymax) / 2
  lapply(split(d$center, d$x), function(v) sort(round(v, 6)))
}

test_that("stacked mixed-sign error bars sit at the correct per-sign cumulative position (#426)", {
  d <- .mk_posneg(sign1 = -1)          # g1 negative, g2/g3 positive
  p <- ggbarplot(d, "x", "y", add = "mean_se", color = "g")
  centers <- .ebar_centers_by_x(p)
  # stacking order is desc(g): g3, g2, g1. Positives cumulate (g3 then g2), the
  # negative g1 cumulates on its own side.
  #   x = A: g3 -> 8, g2 -> 8 + 5 = 13, g1 -> -10
  #   x = B: g3 -> 7, g2 -> 7 + 4 = 11, g1 -> -6
  expect_equal(centers[["1"]], c(-10, 8, 13))
  expect_equal(centers[["2"]], c(-6, 7, 11))
})

test_that("the mixed-sign fix does not move a wholly-positive stack (#426)", {
  d <- .mk_posneg(sign1 = 1)           # all positive
  p <- ggbarplot(d, "x", "y", add = "mean_se", color = "g")
  centers <- .ebar_centers_by_x(p)
  # pure cumsum: x = A -> 8, 13, 23 ; x = B -> 7, 11, 17
  expect_equal(centers[["1"]], c(8, 13, 23))
  expect_equal(centers[["2"]], c(7, 11, 17))
})

test_that("error bars line up with the actual stacked bar mean-ends (#426)", {
  d <- .mk_posneg(sign1 = -1)
  p <- ggbarplot(d, "x", "y", add = "mean_se", color = "g")
  b <- suppressWarnings(ggplot2::ggplot_build(p))
  bi <- which(vapply(p$layers, function(l) inherits(l$geom, "GeomCol") ||
                       inherits(l$geom, "GeomBar"), logical(1)))[1]
  ei <- which(vapply(p$layers, function(l) inherits(l$geom, "GeomErrorbar"), logical(1)))[1]
  bar <- b$data[[bi]]
  # a stacked segment's mean-end is its boundary farther from zero (robust to
  # multiple same-sign segments, not just the one that touches zero).
  bar$meanend <- ifelse(abs(bar$ymax) >= abs(bar$ymin), bar$ymax, bar$ymin)
  eb <- b$data[[ei]]; eb$center <- (eb$ymin + eb$ymax) / 2
  m <- merge(eb[, c("x", "colour", "center")], bar[, c("x", "colour", "meanend")],
             by = c("x", "colour"))
  expect_equal(nrow(m), 6)
  expect_true(all(abs(m$center - m$meanend) < 1e-6))
})

test_that("the corrected side is independent of the factor-level order (#426)", {
  d <- .mk_posneg(sign1 = -1)
  d$g <- factor(d$g, levels = c("g3", "g2", "g1"))   # reversed order (OP's note)
  p <- ggbarplot(d, "x", "y", add = "mean_se", color = "g")
  b <- suppressWarnings(ggplot2::ggplot_build(p))
  bi <- which(vapply(p$layers, function(l) inherits(l$geom, "GeomCol") ||
                       inherits(l$geom, "GeomBar"), logical(1)))[1]
  ei <- which(vapply(p$layers, function(l) inherits(l$geom, "GeomErrorbar"), logical(1)))[1]
  bar <- b$data[[bi]]
  # a stacked segment's mean-end is its boundary farther from zero (robust to
  # multiple same-sign segments, not just the one that touches zero).
  bar$meanend <- ifelse(abs(bar$ymax) >= abs(bar$ymin), bar$ymax, bar$ymin)
  eb <- b$data[[ei]]; eb$center <- (eb$ymin + eb$ymax) / 2
  m <- merge(eb[, c("x", "colour", "center")], bar[, c("x", "colour", "meanend")],
             by = c("x", "colour"))
  expect_true(all(abs(m$center - m$meanend) < 1e-6))  # aligned regardless of order
})
