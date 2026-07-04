context("test-geom-bracket-tip-collapse")

# #631: geom_bracket() tips collapsed to an invisible flat line whenever the
# position axis was trained to a zero-width range - the classic case being a
# SINGLE bracket over a stat-computed y (geom_bar()/geom_histogram()), where the
# bar counts do not train the y scale by the time StatBracket runs, so the only
# y value is the bracket's own y.position. StatBracket now flags that case and
# GeomBracket$draw_group re-derives the tips against the fully-trained panel range
# at draw time (which knows coord_cartesian(ylim=) / scale limits). Normal
# brackets (mapped y, non-zero range) are byte-identical - the flag is FALSE and
# the draw path is untouched.

suppressPackageStartupMessages(library(ggplot2))

# Drawn segment grob (npc coords) for the bracket layer.
.bracket_segments <- function(p) {
  li <- which(vapply(p$layers, function(l) inherits(l$geom, "GeomBracket"), logical(1)))[1]
  lg <- ggplot2::layer_grob(p, li)[[1]]
  ch <- lg$children
  seg <- ch[[which(vapply(ch, function(g) inherits(g, "segments"), logical(1)))[1]]]
  list(y0 = as.numeric(seg$y0), y1 = as.numeric(seg$y1))
}

# Built (pre-transform) bracket-layer data.
.bracket_data <- function(p) {
  b <- suppressWarnings(suppressMessages(ggplot2::ggplot_build(p)))
  li <- which(vapply(p$layers, function(l) inherits(l$geom, "GeomBracket"), logical(1)))[1]
  b$data[[li]]
}

set.seed(1)
bard <- data.frame(Q3 = sample(c("First", "Second", "Third"), 90, replace = TRUE))

# ---- The fix: a single bracket over geom_bar() now draws visible tips ----

test_that("single bracket over geom_bar draws tips of length tip.length (npc)", {
  p <- ggplot(bard, aes(Q3)) + geom_bar() + coord_cartesian(ylim = c(0, 30)) +
    geom_bracket(xmin = "First", xmax = "Second", y.position = 26,
                 tip.length = 0.2, label = "**")
  s <- .bracket_segments(p)
  # rows: 1 = left tip, 2 = bar (flat), 3 = right tip. The bar npc is y at row 2.
  bar <- s$y1[2]
  expect_equal(s$y0[2], s$y1[2])                 # bar is flat
  # left tip drops from bar down by tip.length in npc (axis reference)
  expect_equal(bar - s$y0[1], 0.2, tolerance = 1e-6)
  # right tip drops from bar down by tip.length in npc
  expect_equal(bar - s$y1[3], 0.2, tolerance = 1e-6)
  expect_lt(s$y0[1], bar)                         # visibly below the bar
})

test_that("the fix also works with scale_y_continuous(limits=) and geom_histogram", {
  p1 <- ggplot(bard, aes(Q3)) + geom_bar() + scale_y_continuous(limits = c(0, 40)) +
    geom_bracket(xmin = "First", xmax = "Second", y.position = 30, tip.length = 0.15, label = "*")
  s1 <- .bracket_segments(p1)
  expect_equal(s1$y1[2] - s1$y0[1], 0.15, tolerance = 1e-6)

  dh <- data.frame(v = rnorm(300))
  p2 <- ggplot(dh, aes(v)) + geom_histogram(bins = 20) +
    geom_bracket(xmin = -1, xmax = 1, y.position = 30, tip.length = 0.1, label = "*")
  s2 <- .bracket_segments(p2)
  expect_equal(s2$y1[2] - s2$y0[1], 0.1, tolerance = 1e-6)
})

test_that("asymmetric tip.length is honored per side in the collapsed case", {
  p <- ggplot(bard, aes(Q3)) + geom_bar() + coord_cartesian(ylim = c(0, 30)) +
    geom_bracket(xmin = "First", xmax = "Second", y.position = 26,
                 tip.length = c(0.05, 0.2), label = "**")
  s <- .bracket_segments(p)
  bar <- s$y1[2]
  expect_equal(bar - s$y0[1], 0.05, tolerance = 1e-6)   # left
  expect_equal(bar - s$y1[3], 0.2, tolerance = 1e-6)    # right
})

test_that("tip.length = 0 stays flat even in the collapsed case (intent respected)", {
  p <- ggplot(bard, aes(Q3)) + geom_bar() + coord_cartesian(ylim = c(0, 30)) +
    geom_bracket(xmin = "First", xmax = "Second", y.position = 26,
                 tip.length = 0, label = "**")
  s <- .bracket_segments(p)
  expect_equal(s$y0[1], s$y1[2], tolerance = 1e-9)      # no tip
  expect_equal(s$y1[3], s$y1[2], tolerance = 1e-9)
})

# ---- Characterization: normal brackets are unchanged (flag FALSE) ----

test_that("StatBracket flags only the zero-width (collapsed) case", {
  # collapsed: single bracket over geom_bar -> zero-width y range -> flag TRUE
  d_bar <- .bracket_data(
    ggplot(bard, aes(Q3)) + geom_bar() +
      geom_bracket(xmin = "First", xmax = "Second", y.position = 26, label = "*")
  )
  expect_true(all(d_bar$.bracket.tip.zero.))

  # normal: mapped y with a real range -> flag FALSE
  df <- ToothGrowth; df$dose <- factor(df$dose)
  d_box <- .bracket_data(
    ggboxplot(df, "dose", "len") +
      geom_bracket(xmin = "0.5", xmax = "1", y.position = 35, tip.length = 0.03, label = "p")
  )
  expect_false(any(d_box$.bracket.tip.zero.))
})

test_that("normal bracket tips are computed in the data range as before (unchanged)", {
  df <- ToothGrowth; df$dose <- factor(df$dose)
  p <- ggboxplot(df, "dose", "len") +
    geom_bracket(xmin = "0.5", xmax = "1", y.position = 35, tip.length = 0.03, label = "p")
  d <- .bracket_data(p)
  # tips are baked into the built data (non-zero, computed against the data range);
  # row 1 y (left tip bottom) sits below the bar at 35, and the fallback did NOT run.
  expect_lt(d$y[1], 35)
  expect_equal(d$y[2], 35)
  expect_true(all(d$.bracket.tip.zero. == FALSE))
})

test_that("two brackets over geom_bar (already-working case) still render with tips", {
  p <- ggplot(bard, aes(Q3)) + geom_bar() +
    geom_bracket(xmin = "First", xmax = "Second", y.position = 19, tip.length = 0.2, label = "*") +
    geom_bracket(xmin = "First", xmax = "Third",  y.position = 22, tip.length = 0.2, label = "*")
  expect_error(ggplot2::ggplot_gtable(ggplot2::ggplot_build(p)), NA)
  # first (two-position) layer has a non-zero range -> not flagged
  d <- .bracket_data(p)
  expect_false(any(d$.bracket.tip.zero.))
})

test_that("stat_pvalue_manual (shared draw path) is unaffected and renders", {
  df <- ToothGrowth; df$dose <- factor(df$dose)
  stat.test <- data.frame(group1 = "0.5", group2 = "1", p.adj = 0.01, y.position = 35)
  p <- ggboxplot(df, "dose", "len") +
    stat_pvalue_manual(stat.test, label = "p.adj", tip.length = 0.03)
  expect_error(ggplot2::ggplot_gtable(ggplot2::ggplot_build(p)), NA)
  d <- .bracket_data(p)
  expect_false(any(d$.bracket.tip.zero.))   # mapped y, real range -> not flagged
})
