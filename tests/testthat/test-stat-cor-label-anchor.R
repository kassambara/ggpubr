context("test-stat-cor-label-anchor")

# #248 (Cause 2): stat_cor()/stat_regline_equation() gain label.anchor. With
# label.anchor = "panel" the label is placed at true panel-relative npc (via
# AsIs), so labels align across panels/facets with different axis ranges (e.g.
# scales = "free_y" or geom_smooth extending each panel differently). The default
# label.anchor = "data" is byte-identical to the historical behavior.

.cor_label <- function(p) {
  b <- suppressMessages(ggplot2::ggplot_build(p))
  i <- which(vapply(p$layers, function(l) inherits(l$geom, "GeomText"), logical(1)))[1]
  d <- b$data[[i]]
  d[order(d$PANEL), ]
}

d1 <- data.frame(x = c(1, 2, 3, 4, 5, 6), y = c(2, 4, 5, 4, 6, 7))

# ---- Characterization: the default "data" positions must not change ----

test_that("default stat_cor label position (data anchor) is unchanged", {
  d <- .cor_label(ggscatter(d1, "x", "y") + stat_cor())
  # "left" -> x = data min (1); "top" -> y = data max (7); vjust = label.y.step (1.4).
  # expect_identical (not just equal) pins the exact endpoint values, so a
  # regression that changes the anchor formula (wrong fraction, wrong endpoint,
  # or accidentally emitting an npc/AsIs value) fails here.
  expect_identical(as.numeric(d$x), 1)
  expect_identical(as.numeric(d$y), 7)
  expect_identical(d$vjust, 1.4)
  expect_false(inherits(d$x, "AsIs"))    # data coords, not npc
})

test_that("numeric and character npc positions (data anchor) are unchanged", {
  dn <- .cor_label(ggscatter(d1, "x", "y") + stat_cor(label.x.npc = 0.1, label.y.npc = 0.9))
  expect_equal(as.numeric(dn$x), 1.5)    # 1 + 0.1*(6-1)
  expect_equal(as.numeric(dn$y), 6.5)    # 2 + 0.9*(7-2)
  dc <- .cor_label(ggscatter(d1, "x", "y") + stat_cor(label.x.npc = "center", label.y.npc = "center"))
  expect_identical(as.numeric(dc$x), mean(c(1, 6)))  # center uses mean(range) exactly
  expect_identical(as.numeric(dc$y), mean(c(2, 7)))
  db <- .cor_label(ggscatter(d1, "x", "y") + stat_cor(label.x.npc = "right", label.y.npc = "bottom"))
  expect_identical(as.numeric(db$x), 6)  # data max x
  expect_identical(as.numeric(db$y), 2)  # data min y
})

test_that("per-group vjust formulas (top/bottom/center) are unchanged", {
  set.seed(1)
  dg <- data.frame(x = rnorm(30), y = rnorm(30), g = rep(c("a", "b", "c"), 10))
  vj <- function(npc) {
    p <- ggplot(dg, aes(x, y, color = g)) + geom_point() +
      stat_cor(aes(color = g), label.y.npc = npc)
    sort(.cor_label(p)$vjust)
  }
  # group ids 1,2,3 with label.y.step = 1.4 (.label_params runs per group, so
  # length(group.id) == 1 in each call)
  expect_equal(vj("top"),    c(1.4, 2.8, 4.2))            # step * id
  expect_equal(vj("bottom"), c(-4.2, -2.8, -1.4))         # -step * id
  expect_equal(vj("center"), sort(1.4 * (1:3) - 1.4 / 2))  # step*id - step/2
})

test_that("stat_compare_means labels are byte-identical (shared helper untouched)", {
  df <- ToothGrowth
  df$dose <- factor(df$dose)
  scm <- function(p) {
    b <- suppressMessages(ggplot2::ggplot_build(p))
    i <- which(vapply(p$layers, function(l) inherits(l$geom, "GeomText"), logical(1)))[1]
    d <- b$data[[i]]
    data.frame(x = as.numeric(d$x), y = as.numeric(d$y), vjust = d$vjust)
  }
  s1 <- scm(ggboxplot(df, "dose", "len") + stat_compare_means())
  expect_identical(as.numeric(s1$x), 1)
  expect_identical(s1$y, 33.9)                # data max area (dimension()[2]) - exact
  expect_false(inherits(s1$y, "AsIs"))        # never npc/AsIs
  s2 <- scm(ggboxplot(df, "dose", "len", color = "supp") + stat_compare_means(aes(group = supp)))
  expect_equal(as.numeric(s2$x), c(1, 2, 3))
  expect_true(all(s2$y == 33.9))
})

test_that("explicit label.x/label.y stay in data coordinates in both modes", {
  dd <- .cor_label(ggscatter(d1, "x", "y") + stat_cor(label.x = 2, label.y = 5))
  expect_equal(as.numeric(dd$x), 2); expect_equal(as.numeric(dd$y), 5)
  # even with label.anchor = "panel", explicit coords win and stay data coords
  dp <- .cor_label(ggscatter(d1, "x", "y") + stat_cor(label.x = 2, label.y = 5, label.anchor = "panel"))
  expect_equal(as.numeric(dp$x), 2); expect_equal(as.numeric(dp$y), 5)
  expect_false(inherits(dp$x, "AsIs"))
})

# ---- The fix: panel anchor aligns labels across free-scale facets ----

.free_facet <- function(anchor) {
  d <- data.frame(x = c(1, 2, 3, 1, 1, 2, 3, 1), y = c(1, 2, 3, 3, 1, 2, 2, 2),
                  z = c("A", "A", "A", "A", "B", "B", "B", "B"))
  p <- ggplot(d, aes(x, y)) + geom_point() +
    ggplot2::geom_smooth(method = "lm", se = FALSE, formula = y ~ x) +
    stat_cor(label.anchor = anchor) +
    ggplot2::facet_wrap(ggplot2::vars(z), scales = "free_y")
  b <- suppressMessages(ggplot2::ggplot_build(p))
  i <- which(vapply(p$layers, function(l) inherits(l$geom, "GeomText"), logical(1)))[1]
  td <- b$data[[i]]
  td[order(td$PANEL), ]
}

test_that("label.anchor = 'panel' emits AsIs npc equal across free_y panels", {
  tp <- .free_facet("panel")
  expect_true(inherits(tp$y, "AsIs"))
  expect_equal(length(unique(as.numeric(tp$y))), 1L)   # same npc in every panel
  # default 'data' anchor gives DIFFERENT y per panel (the bug being fixed)
  tdd <- .free_facet("data")
  expect_false(inherits(tdd$y, "AsIs"))
  expect_gt(length(unique(round(as.numeric(tdd$y), 4))), 1L)
})

test_that("label.anchor = 'panel' renders for stat_cor and stat_regline_equation", {
  d <- data.frame(x = c(1, 2, 3, 1, 1, 2, 3, 1), y = c(1, 2, 3, 3, 1, 2, 2, 2),
                  z = c("A", "A", "A", "A", "B", "B", "B", "B"))
  base <- ggplot(d, aes(x, y)) + geom_point() +
    ggplot2::geom_smooth(method = "lm", se = FALSE, formula = y ~ x) +
    ggplot2::facet_wrap(ggplot2::vars(z), scales = "free_y")
  expect_error(suppressMessages(ggplot2::ggplotGrob(base + stat_cor(label.anchor = "panel"))), NA)
  expect_error(suppressMessages(ggplot2::ggplotGrob(base + stat_regline_equation(label.anchor = "panel"))), NA)
})

test_that("multiple grouped labels still stack under panel anchor (label.y.step)", {
  set.seed(1)
  dg <- data.frame(x = rnorm(30), y = rnorm(30), g = rep(c("a", "b", "c"), 10))
  p <- ggplot(dg, aes(x, y, color = g)) + geom_point() +
    stat_cor(aes(color = g), label.anchor = "panel")
  td <- .cor_label(p)
  expect_equal(nrow(td), 3)
  expect_true(inherits(td$y, "AsIs"))
  expect_equal(length(unique(as.numeric(td$y))), 1L)   # same npc anchor...
  expect_equal(length(unique(td$vjust)), 3L)            # ...offset by vjust per group
})

test_that("label.anchor rejects invalid values", {
  # message is locale-dependent (match.arg); just assert it errors
  expect_error(stat_cor(label.anchor = "nope"))
  expect_error(stat_regline_equation(label.anchor = "nope"))
})
