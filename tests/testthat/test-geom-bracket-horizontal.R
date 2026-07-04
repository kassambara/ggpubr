context("test-geom-bracket-horizontal")

# Characterization tests that PIN the horizontal geom_bracket() geometry (the
# long-standing default behavior, shared by stat_pvalue_manual() and
# stat_compare_means()). These lock the exact segment coordinates produced by
# StatBracket so that any future change which silently alters horizontal
# brackets - tip length, step increase, bracket.shorten, bracket.nudge.y, the
# log-scale (#342) or axis (#362) handling - fails here instead of shipping.

df <- ToothGrowth
df$dose <- factor(df$dose)

.seg <- function(p) {
  b <- ggplot2::ggplot_build(p)
  i <- which(vapply(p$layers, function(l) inherits(l$geom, "GeomBracket"), logical(1)))[1]
  d <- b$data[[i]]
  data.frame(
    x = as.numeric(d$x), xend = as.numeric(d$xend),
    y = as.numeric(d$y), yend = as.numeric(d$yend),
    annotation = as.character(d$annotation), stringsAsFactors = FALSE
  )
}

test_that("single bracket with character xmin/xmax is unchanged", {
  d <- .seg(ggboxplot(df, "dose", "len") +
    geom_bracket(xmin = "0.5", xmax = "1", y.position = 30, label = "***"))
  expect_equal(d$x,    c(1, 1, 2))
  expect_equal(d$xend, c(1, 2, 2))
  expect_equal(d$y,    c(29.109, 30, 30), tolerance = 1e-4)
  expect_equal(d$yend, c(30, 30, 29.109), tolerance = 1e-4)
  expect_equal(d$annotation, rep("***", 3))
})

test_that("multiple brackets with step.increase are unchanged", {
  d <- .seg(ggboxplot(df, "dose", "len") +
    geom_bracket(xmin = c("0.5", "1"), xmax = c("1", "2"),
                 y.position = c(30, 35), label = c("***", "**"),
                 tip.length = 0.01, step.increase = 0.1))
  expect_equal(d$x,    c(1, 1, 2, 2, 2, 3))
  expect_equal(d$xend, c(1, 2, 2, 2, 3, 3))
  expect_equal(d$y,    c(29.692, 30, 30, 37.772, 38.08, 38.08), tolerance = 1e-3)
  expect_equal(d$yend, c(30, 30, 29.692, 38.08, 38.08, 37.772), tolerance = 1e-3)
})

test_that("per-tip tip.length vector is unchanged", {
  d <- .seg(ggboxplot(df, "dose", "len") +
    geom_bracket(xmin = "0.5", xmax = "1", y.position = 30, label = "*",
                 tip.length = c(0.2, 0.02)))
  # asymmetric tips: left tip is long (0.2), right tip is short (0.02)
  expect_equal(d$y,    c(24.06, 30, 30), tolerance = 1e-3)
  expect_equal(d$yend, c(30, 30, 29.406), tolerance = 1e-3)
})

test_that("bracket.shorten and bracket.nudge.y are unchanged", {
  d <- .seg(ggboxplot(df, "dose", "len") +
    geom_bracket(xmin = "0.5", xmax = "2", y.position = 30, label = "*",
                 bracket.shorten = 0.2, bracket.nudge.y = 2))
  # shorten pulls the ends in by 0.1 each (1->1.1, 3->2.9); nudge lifts y by 2
  expect_equal(d$x,    c(1.1, 1.1, 2.9))
  expect_equal(d$xend, c(1.1, 2.9, 2.9))
  expect_equal(d$y,    c(31.109, 32, 32), tolerance = 1e-3)
  expect_equal(d$yend, c(32, 32, 31.109), tolerance = 1e-3)
})

test_that("y positions land in transformed space on a log y scale (#342)", {
  d <- .seg(ggboxplot(df, "dose", "len") +
    geom_bracket(xmin = "0.5", xmax = "1", y.position = 30, label = "*") +
    ggplot2::scale_y_log10())
  # y.position = 30 -> log10(30) = 1.4771; tip goes down to ~1.4499
  expect_equal(d$y,    c(1.4499, 1.4771, 1.4771), tolerance = 1e-4)
  expect_equal(d$yend, c(1.4771, 1.4771, 1.4499), tolerance = 1e-4)
})

test_that("tip.length.ref = 'axis' uses the axis range, not the data range (#362)", {
  # with ylim(0, 60) the tip is 0.03 * 60 = 1.8 below y.position
  d <- .seg(ggboxplot(df, "dose", "len") +
    geom_bracket(xmin = "0.5", xmax = "1", y.position = 30, label = "*",
                 tip.length.ref = "axis") + ggplot2::ylim(0, 60))
  expect_equal(min(d$y), 30 - 0.03 * 60, tolerance = 1e-4)
})

test_that("numeric xmin/xmax on a continuous x axis are unchanged", {
  d <- .seg(ggscatter(df, "len", "len") +
    geom_bracket(xmin = 10, xmax = 20, y.position = 30, label = "ns"))
  expect_equal(d$x,    c(10, 10, 20))
  expect_equal(d$xend, c(10, 20, 20))
  expect_equal(d$y[2:3], c(30, 30))
})

test_that("stat_pvalue_manual() draws the same horizontal bracket segments", {
  stat.test <- compare_means(len ~ dose, df, method = "t.test")
  stat.test$y.position <- c(32, 35, 38)
  d <- .seg(ggboxplot(df, "dose", "len") +
    stat_pvalue_manual(stat.test, label = "p.signif"))
  expect_equal(nrow(d), 9)                       # 3 comparisons x 3 segments
  expect_equal(unique(d$x),    c(1, 2, 3))
  expect_equal(unique(d$xend), c(1, 2, 3))
  # the bars sit at the requested y positions
  expect_true(all(c(32, 35, 38) %in% round(d$y, 3)))
})

test_that("a plain horizontal geom_bracket() draws without warning", {
  expect_silent(
    ggplot2::ggplotGrob(
      ggboxplot(df, "dose", "len") +
        geom_bracket(xmin = "0.5", xmax = "1", y.position = 30, label = "*")
    )
  )
})

# The label is drawn in GeomBracket$draw_group (not in the built layer data), so
# these render-and-inspect tests pin its ANGLE and PANEL position - guarding the
# label control flow (horizontal vs coord.flip vs vertical branches).
.label_grob <- function(p) {
  g <- ggplot2::ggplotGrob(p)
  txt <- NULL
  find_txt <- function(gr) {
    if (inherits(gr, "text")) { txt <<- gr; return(invisible()) }
    if (!is.null(gr$children)) for (ch in gr$children) find_txt(ch)
    if (inherits(gr, "gTree") && !is.null(gr$grobs)) for (ch in gr$grobs) find_txt(ch)
  }
  for (pi in which(grepl("panel", g$layout$name))) find_txt(g$grobs[[pi]])
  txt
}

test_that("horizontal label is centered above the bracket, not rotated", {
  t <- .label_grob(ggboxplot(df, "dose", "len") +
    geom_bracket(xmin = "0.5", xmax = "1", y.position = 30, label = "H"))
  expect_false(is.null(t))
  expect_equal(t$rot, 0)                                  # horizontal: no rotation
  expect_equal(as.numeric(t$x), 0.344, tolerance = 0.03)  # centered over the span
  expect_equal(as.numeric(t$y), 0.845, tolerance = 0.03)  # above the bar
})

test_that("coord.flip label sits to the side and is rotated -90", {
  t <- .label_grob(ggboxplot(df, "dose", "len") +
    geom_bracket(xmin = "0.5", xmax = "1", y.position = 30, label = "C",
                 coord.flip = TRUE) + ggplot2::coord_flip())
  expect_false(is.null(t))
  expect_equal(t$rot, -90)
  expect_equal(as.numeric(t$x), 0.845, tolerance = 0.03)
  expect_equal(as.numeric(t$y), 0.344, tolerance = 0.03)
})
