context("test-ggbarplot-facet-summary")

# #739: a summarized ggbarplot (add = "mean_se") must be faceted with the
# `facet.by=` argument, which pre-computes the summaries per panel. This test
# locks that correct behavior: with facet.by=, panels with different data get
# DIFFERENT (per-panel) bar heights - they are not pooled across the whole data.

suppressPackageStartupMessages(library(ggplot2))

.bar_by_panel <- function(p) {
  b <- suppressWarnings(ggplot2::ggplot_build(p))
  bi <- which(vapply(p$layers, function(l) inherits(l$geom, "GeomBar") ||
                       inherits(l$geom, "GeomCol"), logical(1)))[1]
  d <- b$data[[bi]]
  split(round(d$y, 6), d$PANEL)
}

test_that("facet.by= gives correct per-panel summaries (not pooled) (#739)", {
  tg <- ToothGrowth
  tg$dose <- factor(tg$dose)
  # two blocks with deliberately different level: 'high' block shifted up by 20
  tg$blk <- ifelse(tg$len < stats::median(tg$len), "low", "high")

  p <- ggbarplot(tg, "dose", "len", add = "mean_se", fill = "supp", facet.by = "blk")
  panels <- .bar_by_panel(p)

  expect_length(panels, 2)                       # two panels
  # the two panels summarize different data -> their bar heights must differ
  expect_false(isTRUE(all.equal(panels[[1]], panels[[2]])))
})

test_that("facet.by= panels reflect their own subset means (#739)", {
  # construct data where panel means are known and different
  d <- rbind(
    data.frame(g = "p1", x = c("a", "a", "b", "b"), y = c(2, 2, 4, 4)),
    data.frame(g = "p2", x = c("a", "a", "b", "b"), y = c(10, 10, 20, 20))
  )
  d$x <- factor(d$x)
  p <- ggbarplot(d, "x", "y", add = "mean_se", facet.by = "g")
  panels <- .bar_by_panel(p)
  # p1 bars = 2,4 ; p2 bars = 10,20 (each panel its own means, not pooled 6,12)
  expect_equal(sort(unname(panels[["1"]])), c(2, 4))
  expect_equal(sort(unname(panels[["2"]])), c(10, 20))
})
