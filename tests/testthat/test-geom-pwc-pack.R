context("test-geom-pwc-pack")

# One row per bracket: its x-span and its shelf (y) level.
bracket_shelves <- function(p) {
  b <- ggplot2::ggplot_build(p)
  idx <- which(vapply(
    b$data, function(d) all(c("x", "xend", "y", "yend", "group") %in% names(d)),
    logical(1)
  ))
  d <- b$data[[idx[1]]]
  do.call(rbind, lapply(split(d, d$group), function(g) {
    data.frame(xmin = min(g$x, g$xend), xmax = max(g$x, g$xend), y = max(g$y))
  }))
}

strict_overlap <- function(a, b) a$xmin < b$xmax & b$xmin < a$xmax

# ---- helper unit tests -----------------------------------------------------
test_that(".pack_bracket_shelves uses the minimum number of shelves", {
  # 6 groups -> all-pairwise x-spans; max overlap depth (max clique) at the
  # centre is 9, so an optimal packing uses exactly 9 shelves.
  combos <- t(utils::combn(6, 2))
  xmin <- combos[, 1]
  xmax <- combos[, 2]
  shelves <- ggpubr:::.pack_bracket_shelves(xmin, xmax)
  expect_equal(max(shelves), 9)
  # No two brackets on the same shelf may strictly overlap.
  for (s in unique(shelves)) {
    idx <- which(shelves == s)
    if (length(idx) > 1) {
      for (i in seq_along(idx)[-length(idx)]) {
        for (j in (i + 1):length(idx)) {
          a <- idx[i]
          b <- idx[j]
          expect_false(xmin[a] < xmax[b] && xmin[b] < xmax[a])
        }
      }
    }
  }
})

test_that(".pack_bracket_shelves lets touching spans share a shelf", {
  # [1,2] and [2,3] touch but do not overlap -> same shelf.
  shelves <- ggpubr:::.pack_bracket_shelves(c(1, 2), c(2, 3))
  expect_equal(length(unique(shelves)), 1)
})

test_that(".pack_bracket_shelves handles NA spans without erroring", {
  # An NA span must not crash; it is given its own shelf and must not disturb
  # the packing of the well-defined spans ([1,2] and [3,4] still share a shelf).
  shelves <- ggpubr:::.pack_bracket_shelves(c(1, NA, 3), c(2, 4, 4))
  expect_length(shelves, 3)
  expect_false(anyNA(shelves))
  expect_equal(shelves[1], shelves[3])
  expect_false(shelves[2] %in% shelves[c(1, 3)])
})

# ---- integration tests -----------------------------------------------------
set.seed(1)
df6 <- data.frame(
  g = factor(rep(1:6, each = 12)),
  y = rnorm(72, rep(1:6, each = 12))
)

test_that("pack = 'auto' produces no overlapping brackets on the same shelf", {
  p <- ggboxplot(df6, x = "g", y = "y") +
    geom_pwc(method = "t_test", pack = "auto")
  agg <- bracket_shelves(p)
  bad <- 0
  for (yy in unique(agg$y)) {
    s <- agg[agg$y == yy, ]
    if (nrow(s) > 1) {
      for (i in seq_len(nrow(s) - 1)) {
        for (j in (i + 1):nrow(s)) {
          if (strict_overlap(s[i, ], s[j, ])) bad <- bad + 1
        }
      }
    }
  }
  expect_equal(bad, 0)
})

test_that("pack = 'auto' compacts the stack versus the default", {
  p_none <- ggboxplot(df6, x = "g", y = "y") +
    geom_pwc(method = "t_test")
  p_auto <- ggboxplot(df6, x = "g", y = "y") +
    geom_pwc(method = "t_test", pack = "auto")
  n_none <- length(unique(round(bracket_shelves(p_none)$y, 6)))
  n_auto <- length(unique(round(bracket_shelves(p_auto)$y, 6)))
  expect_equal(n_none, 15) # one shelf per comparison
  expect_equal(n_auto, 9) # optimal packing for 6 groups
  expect_lt(n_auto, n_none)
})

test_that("pack default is 'none' and leaves bracket y-positions unchanged", {
  df <- ToothGrowth
  df$dose <- factor(df$dose)
  p_default <- ggboxplot(df, x = "dose", y = "len") +
    geom_pwc(method = "wilcox_test")
  p_explicit <- ggboxplot(df, x = "dose", y = "len") +
    geom_pwc(method = "wilcox_test", pack = "none")
  b_default <- ggplot2::ggplot_build(p_default)$data[[2]]
  b_explicit <- ggplot2::ggplot_build(p_explicit)$data[[2]]
  expect_equal(b_default$y, b_explicit$y)
  expect_equal(b_default$x, b_explicit$x)
})
