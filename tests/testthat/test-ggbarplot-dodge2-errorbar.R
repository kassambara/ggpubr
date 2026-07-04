context("test-ggbarplot-dodge2-errorbar")

# #363: with position_dodge2() the thin error bars did not line up with the
# dodged bars (dodge2 packs elements by their own width, so a thin error bar
# lands off the centre of its wide bar). ggbarplot() now re-centres the error
# bars on the actual bar positions when position_dodge2() is used. Every other
# position (identity / dodge / stack) keeps the standard path unchanged.

.bar_x <- function(p) {
  b <- ggplot2::ggplot_build(p)
  i <- which(vapply(p$layers, function(l) inherits(l$geom, "GeomBar"), logical(1)))[1]
  sort(as.numeric(round(b$data[[i]]$x, 4)))
}
.err_x <- function(p) {
  b <- ggplot2::ggplot_build(p)
  i <- which(vapply(p$layers, function(l)
    inherits(l$geom, c("GeomErrorbar", "GeomPointrange", "GeomLinerange")), logical(1)))[1]
  sort(as.numeric(round(b$data[[i]]$x, 4)))
}

.reporter <- data.frame(
  Treatment = c("+", "+", "+", "+", "+", "+", "-", "-", "-"),
  Group     = c("a", "a", "a", "b", "b", "b", "a", "a", "a"),
  Count     = c(12, 13, 11, 14, 15, 14, 23, 24, 25)
)

test_that("dodge2 error bars align with the bars (#363)", {
  p <- ggbarplot(.reporter, x = "Treatment", y = "Count", add = "mean_se",
                 error.plot = "upper_errorbar", fill = "Group",
                 position = position_dodge2(preserve = "single"))
  expect_equal(.err_x(p), .bar_x(p))
  # renders at draw time (positioning bugs often only surface on draw)
  expect_silent(ggplot2::ggplotGrob(p))
})

test_that("alignment holds for both-sided errorbar, pointrange and linerange", {
  for (ep in c("errorbar", "pointrange", "linerange")) {
    p <- ggbarplot(.reporter, x = "Treatment", y = "Count", add = "mean_se",
                   error.plot = ep, fill = "Group",
                   position = position_dodge2(preserve = "single"))
    expect_equal(.err_x(p), .bar_x(p), info = ep)
  }
})

test_that("alignment is independent of input row order", {
  shuffled <- .reporter[c(7, 1, 4, 8, 2, 5, 9, 3, 6), ]
  p <- ggbarplot(shuffled, x = "Treatment", y = "Count", add = "mean_se",
                 error.plot = "upper_errorbar", fill = "Group",
                 position = position_dodge2(preserve = "single"))
  expect_equal(.err_x(p), .bar_x(p))
})

test_that("alignment holds for >2 groups with missing subgroups", {
  d <- data.frame(
    Treatment = factor(rep(c("+", "-"), c(9, 6)), levels = c("+", "-")),
    Group = c(rep(c("a", "b", "c"), each = 3), rep(c("a", "c"), each = 3)),
    Count = c(12, 13, 11, 14, 15, 14, 9, 10, 11, 23, 24, 25, 7, 8, 9)
  )
  p <- ggbarplot(d, x = "Treatment", y = "Count", add = "mean_se",
                 error.plot = "upper_errorbar", fill = "Group",
                 position = position_dodge2(preserve = "single"))
  expect_equal(.err_x(p), .bar_x(p))
})

test_that("alignment holds across facets", {
  d <- data.frame(
    Sex = rep(c("M", "F"), each = 9),
    Treatment = factor(rep(rep(c("+", "-"), c(6, 3)), 2), levels = c("+", "-")),
    Group = rep(c("a", "a", "a", "b", "b", "b", "a", "a", "a"), 2),
    Count = c(12, 13, 11, 14, 15, 14, 23, 24, 25, 8, 9, 7, 11, 12, 10, 20, 21, 19)
  )
  p <- ggbarplot(d, x = "Treatment", y = "Count", add = "mean_se",
                 error.plot = "upper_errorbar", fill = "Group",
                 facet.by = "Sex", position = position_dodge2(preserve = "single"))
  expect_equal(.err_x(p), .bar_x(p))
})

# ---- No-regression: non-dodge2 positions are unchanged ----

test_that("position_dodge() error bars are unchanged (still aligned)", {
  p <- ggbarplot(.reporter, x = "Treatment", y = "Count", add = "mean_se",
                 error.plot = "upper_errorbar", fill = "Group",
                 position = position_dodge(0.8))
  # dodge (v1) has always aligned thin error bars; pin the exact positions
  expect_equal(.err_x(p), c(0.8, 1.2, 2.0))
  expect_equal(.bar_x(p), c(0.8, 1.2, 2.0))
})

test_that("stacked bars keep the dedicated stacked-errorbar path", {
  d <- data.frame(
    Treatment = rep(c("+", "-"), each = 4),
    Group = rep(c("a", "b"), 4),
    Count = c(12, 14, 13, 15, 23, 10, 24, 11)
  )
  p <- ggbarplot(d, x = "Treatment", y = "Count", add = "mean_se",
                 error.plot = "upper_errorbar", fill = "Group",
                 position = position_stack())
  # stacked error bars sit at the x-tick centres (1 and 2), not dodged
  expect_equal(.err_x(p), c(1, 1, 2, 2))
  expect_silent(ggplot2::ggplotGrob(p))
})

test_that("asymmetric summaries (median_hilow/q1q3) keep correct interval under dodge2", {
  # These are NOT centre +/- error, so the aligned path must bail out and let the
  # standard path draw the real asymmetric interval (regression guard).
  set.seed(1)
  d <- data.frame(
    T = rep(c("+", "-"), c(12, 6)),
    G = c(rep(c("a", "b"), each = 6), rep("a", 6)),
    Count = c(rnorm(12, 15, 3), rnorm(6, 24, 3))
  )
  .h <- function(p) {
    b <- ggplot2::ggplot_build(p)
    i <- which(vapply(p$layers, function(l) inherits(l$geom, "GeomErrorbar"), logical(1)))[1]
    sort(round(b$data[[i]]$ymax - b$data[[i]]$ymin, 3))
  }
  for (f in c("median_hilow", "median_q1q3")) {
    p_d2 <- ggbarplot(d, "T", "Count", add = f, error.plot = "errorbar", fill = "G",
                      position = position_dodge2(preserve = "single"))
    p_d1 <- ggbarplot(d, "T", "Count", add = f, error.plot = "errorbar", fill = "G",
                      position = position_dodge(0.8))
    expect_equal(.h(p_d2), .h(p_d1), info = f)          # same interval as before
    expect_true(all(.h(p_d2) > 0), info = f)            # not collapsed to zero
  }
})

test_that("error bar colour follows the color argument (default black; grouped when mapped)", {
  pd <- ggbarplot(.reporter, x = "Treatment", y = "Count", add = "mean_se",
                  error.plot = "upper_errorbar", fill = "Group",
                  position = position_dodge2(preserve = "single"))
  b <- ggplot2::ggplot_build(pd)
  ei <- which(vapply(pd$layers, function(l) inherits(l$geom, "GeomErrorbar"), logical(1)))[1]
  expect_equal(unique(b$data[[ei]]$colour), "black")

  pc <- ggbarplot(.reporter, x = "Treatment", y = "Count", add = "mean_se",
                  error.plot = "upper_errorbar", fill = "Group", color = "Group",
                  position = position_dodge2(preserve = "single"))
  bc <- ggplot2::ggplot_build(pc)
  eic <- which(vapply(pc$layers, function(l) inherits(l$geom, "GeomErrorbar"), logical(1)))[1]
  expect_gt(length(unique(bc$data[[eic]]$colour)), 1)
})
