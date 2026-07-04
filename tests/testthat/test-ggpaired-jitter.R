context("test-ggpaired-jitter")

# #407: ggpaired() gains an opt-in `jitter` argument that spreads the paired
# points horizontally to reduce overlap, while keeping each pair's connecting
# line intact (one offset per subject id) and never moving the values (y).
# Default jitter = 0 leaves the output byte-identical.

before <- c(200.1, 190.9, 192.7, 213, 241.4, 196.9, 172.2, 185.5, 205.2, 193.7)
after  <- c(392.9, 393.2, 345.1, 393, 434, 427.9, 422, 383.9, 392.3, 352.2)
d <- data.frame(before = before, after = after)

.layers <- function(p) {
  b <- ggplot2::ggplot_build(p)
  pt <- which(vapply(p$layers, function(l) inherits(l$geom, "GeomPoint"), logical(1)))[1]
  ln <- which(vapply(p$layers, function(l) inherits(l$geom, "GeomLine"), logical(1)))[1]
  list(pt = b$data[[pt]], ln = b$data[[ln]])
}

test_that("default jitter = 0 is byte-identical to omitting it", {
  p_omit <- ggpaired(d, cond1 = "before", cond2 = "after", fill = "condition")
  p_zero <- ggpaired(d, cond1 = "before", cond2 = "after", fill = "condition", jitter = 0)
  expect_equal(ggplot2::ggplot_build(p_omit)$data, ggplot2::ggplot_build(p_zero)$data)
  # points sit exactly on x = 1 and x = 2 (no spread)
  expect_equal(sort(unique(round(.layers(p_zero)$pt$x, 6))), c(1, 2))
})

test_that("jitter > 0 spreads the points but does not move the values", {
  p <- ggpaired(d, cond1 = "before", cond2 = "after", jitter = 0.1)
  L <- .layers(p)
  expect_gt(diff(range(L$pt$x)), 0.15)                 # horizontal spread
  expect_equal(sort(L$pt$y), sort(c(before, after)))   # y untouched
  expect_silent(ggplot2::ggplotGrob(p))
})

test_that("the connecting lines follow the jittered points", {
  p <- ggpaired(d, cond1 = "before", cond2 = "after", jitter = 0.12)
  L <- .layers(p)
  lv <- as.matrix(L$ln[order(L$ln$x, L$ln$y), c("x", "y")])
  pv <- as.matrix(L$pt[order(L$pt$x, L$pt$y), c("x", "y")])
  expect_equal(lv, pv, tolerance = 1e-6)
})

test_that("jitter is paired: each subject's two points share the same offset", {
  p <- ggpaired(d, cond1 = "before", cond2 = "after", jitter = 0.1)
  pt <- .layers(p)$pt
  cond <- ifelse(pt$x < 1.5, "before", "after")
  ob <- (pt$x[cond == "before"] - 1)[order(match(round(pt$y[cond == "before"], 4), round(before, 4)))]
  oa <- (pt$x[cond == "after"]  - 2)[order(match(round(pt$y[cond == "after"], 4),  round(after, 4)))]
  expect_equal(ob, oa, tolerance = 1e-6)
})

test_that("jitter is reproducible across calls (fixed seed) and leaves the RNG untouched", {
  p1 <- ggpaired(d, cond1 = "before", cond2 = "after", jitter = 0.1)
  p2 <- ggpaired(d, cond1 = "before", cond2 = "after", jitter = 0.1)
  expect_equal(.layers(p1)$pt$x, .layers(p2)$pt$x)

  set.seed(999); r1 <- runif(3)
  set.seed(999); invisible(ggpaired(d, cond1 = "before", cond2 = "after", jitter = 0.1)); r2 <- runif(3)
  expect_equal(r1, r2)
})

test_that("jitter works in x/y mode with color and a supplied id", {
  tg <- ToothGrowth
  tg$id <- rep(1:30, 2)
  p <- ggpaired(tg, x = "supp", y = "len", color = "supp", id = "id", jitter = 0.08)
  L <- .layers(p)
  lv <- as.matrix(L$ln[order(L$ln$x, L$ln$y), c("x", "y")])
  pv <- as.matrix(L$pt[order(L$pt$x, L$pt$y), c("x", "y")])
  expect_equal(lv, pv, tolerance = 1e-6)
  expect_silent(ggplot2::ggplotGrob(p))
})
