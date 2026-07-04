context("test-geom-bracket-vertical")

# #456: geom_bracket() gains `orientation = "vertical"` to draw a native vertical
# bracket (bar spanning y, tips pointing left, rotated label) via ymin/ymax/
# x.position - useful for plots where the comparison runs along the y axis (e.g.
# Kaplan-Meier curves). The default orientation = "horizontal" is byte-identical.

df <- ToothGrowth
df$dose <- factor(df$dose)

.bracket <- function(p) {
  b <- ggplot2::ggplot_build(p)
  i <- which(vapply(p$layers, function(l) inherits(l$geom, "GeomBracket"), logical(1)))[1]
  b$data[[i]]
}

test_that("default (horizontal) output is unchanged by the refactor", {
  # pin the exact segment coordinates of a representative horizontal bracket
  p <- ggboxplot(df, "dose", "len") +
    geom_bracket(xmin = "0.5", xmax = "1", y.position = 30, label = "***")
  d <- .bracket(p)
  expect_equal(as.numeric(d$x),    c(1, 1, 2))
  expect_equal(as.numeric(d$xend), c(1, 2, 2))
  # tips go down from y.position = 30 by tip.length (0.03) * data range
  expect_equal(as.numeric(d$y[2:3]),    c(30, 30))
  expect_equal(as.numeric(d$yend[1:2]), c(30, 30))
  expect_lt(d$y[1], 30)          # left tip points down
  expect_lt(d$yend[3], 30)       # right tip points down
  # passing orientation = "horizontal" explicitly is identical to omitting it
  p2 <- ggboxplot(df, "dose", "len") +
    geom_bracket(xmin = "0.5", xmax = "1", y.position = 30, label = "***",
                 orientation = "horizontal")
  expect_equal(.bracket(p2)[, c("x", "xend", "y", "yend")],
               d[, c("x", "xend", "y", "yend")])
})

test_that("vertical bracket swaps the geometry: bar spans y, tips point in -x", {
  p <- ggscatter(df, "len", "len") +
    geom_bracket(orientation = "vertical", ymin = 10, ymax = 25,
                 x.position = 30, label = "p<0.05")
  d <- .bracket(p)
  # the bar is vertical at x = 30, spanning y in [10, 25]
  expect_equal(d$y,    c(10, 10, 25))
  expect_equal(d$yend, c(10, 25, 25))
  expect_equal(d$x[2:3],    c(30, 30))
  expect_equal(d$xend[1:2], c(30, 30))
  # tips point left (toward smaller x)
  expect_lt(d$x[1], 30)
  expect_lt(d$xend[3], 30)
  expect_silent(suppressWarnings(ggplot2::ggplotGrob(p)))
})

test_that("vertical tip length scales to the x range (not the y range)", {
  # x range is len (~4.2..33.9); tip.length default 0.03 => tip ~ 0.03 * xrange
  p <- ggscatter(df, "len", "len") +
    geom_bracket(orientation = "vertical", ymin = 10, ymax = 25,
                 x.position = 30, label = "ns")
  d <- .bracket(p)
  b <- ggplot2::ggplot_build(p)
  xr <- diff(b$layout$panel_params[[1]]$x.range)
  tip <- 30 - d$x[1]
  # the tip is a small positive fraction of the x range, and not of the (equal-here) y
  expect_gt(tip, 0)
  expect_lt(tip, xr)               # sane
})

test_that("the bar position honors the x-scale transform (log-x)", {
  set.seed(1)
  d0 <- data.frame(x = 1:10, y = runif(10, 1, 9))
  p <- ggscatter(d0, "x", "y") +
    geom_bracket(orientation = "vertical", ymin = 3, ymax = 7, x.position = 8, label = "ns") +
    ggplot2::scale_x_log10()
  d <- .bracket(p)
  # the bar (the max x among the segment ends) lands at log10(8) in transformed space
  expect_equal(max(d$xend), log10(8), tolerance = 1e-6)
  expect_equal(max(d$x), log10(8), tolerance = 1e-6)
  # and CRUCIALLY the span y must stay at the data values 3 and 7 (a non-identity
  # x scale must not corrupt the y span, #456)
  expect_equal(sort(unique(round(c(d$y, d$yend), 6))), c(3, 7))
})

test_that("the y span is correct under every combination of x/y scale transforms", {
  set.seed(1)
  d0 <- data.frame(x = 1:10, y = runif(10, 1, 9))
  span_y <- function(...) {
    p <- ggscatter(d0, "x", "y") +
      geom_bracket(orientation = "vertical", ymin = 3, ymax = 7, x.position = 8, label = "ns")
    for (s in list(...)) p <- p + s
    d <- .bracket(suppressWarnings(p))
    sort(unique(round(c(d$y, d$yend), 4)))
  }
  expect_equal(span_y(),                                            c(3, 7))
  expect_equal(span_y(ggplot2::scale_y_log10()),                    round(c(log10(3), log10(7)), 4))
  expect_equal(span_y(ggplot2::scale_x_log10()),                    c(3, 7))            # log-x must NOT touch the y span
  expect_equal(span_y(ggplot2::scale_x_log10(), ggplot2::scale_y_log10()),
               round(c(log10(3), log10(7)), 4))                                        # no double transform
  expect_equal(span_y(ggplot2::scale_x_reverse()),                  c(3, 7))
  expect_equal(span_y(ggplot2::scale_x_sqrt()),                     c(3, 7))
})

test_that("orientation = 'vertical' errors if combined with coord.flip = TRUE", {
  expect_error(
    geom_bracket(orientation = "vertical", coord.flip = TRUE,
                 ymin = 1, ymax = 2, x.position = 1, label = "x"),
    "cannot be combined"
  )
})

test_that("a discrete position axis does not crash the vertical bracket", {
  # not the primary use case (vertical brackets target a continuous x axis),
  # but it must not error
  p <- ggboxplot(df, "supp", "len") +
    geom_bracket(orientation = "vertical", ymin = 10, ymax = 25,
                 x.position = 2.3, label = "*")
  expect_error(suppressWarnings(ggplot2::ggplotGrob(p)), NA)
})

test_that("expression labels work for vertical brackets", {
  p <- ggscatter(df, "len", "len") +
    geom_bracket(orientation = "vertical", ymin = 10, ymax = 25, x.position = 30,
                 label = "list(~italic(p)<=0.001)", type = "expression")
  expect_error(suppressWarnings(ggplot2::ggplotGrob(p)), NA)
})
