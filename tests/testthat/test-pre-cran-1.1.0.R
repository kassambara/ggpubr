# Regression tests for the changes carried into 1.1.0 from the pre-CRAN review.
# Deliberately narrow: each asserts a value or a coordinate, against a base-R
# reference or against ggpubr 1.0.0's own measured output.

test_that("ggbarplot() keeps each dodged error bar on its own bar (#404)", {
  # The error layer's dodge key was ordered differently from the bars' own
  # grouping, so with three discrete variables half the error bars were centred
  # on a neighbouring bar's mean. ggpubr 1.0.0 put 4 of 8 on the wrong bar.
  set.seed(1)
  d <- data.frame(
    g = rep(c("A", "B"), each = 12), f = rep(c("f1", "f2"), 12),
    a = rep(c("a1", "a2"), each = 6, times = 2), v = stats::rnorm(24, 10)
  )
  ref <- stats::aggregate(v ~ g + f + a, data = d, FUN = mean)
  ref.se <- stats::aggregate(v ~ g + f + a, data = d,
    FUN = function(z) stats::sd(z) / sqrt(length(z)))
  p <- suppressWarnings(ggbarplot(d, x = "g", y = "v", fill = "f", alpha = "a",
    add = "mean_se", position = ggplot2::position_dodge(0.8)))
  b <- suppressWarnings(ggplot2::ggplot_build(p))
  expect_s3_class(ggplot2::ggplotGrob(b), "gtable") # draws, no "alpha * 255"
  bar <- b$data[[1]]
  eb <- b$data[[2]]
  expect_equal(nrow(bar), 8L)
  expect_equal(nrow(eb), 8L)
  expect_equal(sort(bar$y), sort(ref$v), tolerance = 1e-8)
  bar <- bar[order(bar$x, bar$y), ]
  eb <- eb[order(eb$x, (eb$ymin + eb$ymax) / 2), ]
  # element-wise, so a permutation cannot pass
  expect_equal(as.numeric(bar$y), (eb$ymin + eb$ymax) / 2, tolerance = 1e-8)
  expect_equal(sort((eb$ymax - eb$ymin) / 2), sort(ref.se$v), tolerance = 1e-8)
  # An `alpha` naming one of desc_statby()'s summary columns is legitimate - the
  # bar layer is drawn from that summary, so geom_exec() resolves the mapping
  # against it - and must keep rendering.
  for (a in c("se", "sd", "ci")) {
    p2 <- suppressWarnings(ggbarplot(d, x = "g", y = "v", fill = "f",
      add = "mean_se", position = ggplot2::position_dodge(0.8), alpha = a))
    expect_s3_class(ggplot2::ggplotGrob(suppressWarnings(
      ggplot2::ggplot_build(p2))), "gtable")
  }

  # A missing value in the alpha column is ordinary research data. interaction()
  # returns NA for such a row, giving it no dodge rank, while ggplot2's own
  # id_var(drop = TRUE) sorts na.last = TRUE and keeps NA as a real trailing
  # level - so without addNA() the two orderings diverge from the NA cell onward
  # and an error bar is drawn on a neighbour's bar with ITS mean and ITS error.
  set.seed(7)
  dna <- data.frame(
    g = rep(c("A", "B"), each = 12), f = rep(c("f1", "f2"), 12),
    a = rep(c("a1", "a2"), each = 6, times = 2), v = stats::rnorm(24, 10)
  )
  dna$a[dna$g == "A" & dna$f == "f1"] <- NA
  pna <- suppressWarnings(ggbarplot(dna, x = "g", y = "v", fill = "f",
    alpha = "a", add = "mean_se", position = ggplot2::position_dodge(0.8)))
  bna <- suppressWarnings(ggplot2::ggplot_build(pna))
  expect_s3_class(ggplot2::ggplotGrob(bna), "gtable")
  nbar <- bna$data[[1]]
  neb <- bna$data[[2]]
  expect_equal(nrow(neb), nrow(nbar))
  expect_equal(
    as.numeric(nbar$y)[order(as.numeric(nbar$x))],
    ((neb$ymin + neb$ymax) / 2)[order(as.numeric(neb$x))],
    tolerance = 1e-8
  )

  # `color=` mapped to a column is the shape that a two-column key gets WRONG:
  # ggplot2 orders `colour` BEFORE `fill` in the layer data, so bar groups are
  # colour-slowest, and keying on (fill, alpha) alone is exactly transposed. That
  # configuration is CORRECT on the released version, so getting it wrong here
  # would be a regression - and no earlier test crossed alpha with color.
  ref.se <- function(dd, keys) {
    stats::aggregate(stats::reformulate(keys, "v"), data = dd,
      FUN = function(z) stats::sd(z) / sqrt(length(z)))$v
  }
  for (aes.extra in list(
    list(fill = "f", color = "a"),   # colour and alpha on the SAME column
    list(color = "a"),               # colour only, no fill
    list(fill = "a")                 # fill and alpha on the same column
  )) {
    args <- c(list(d, x = "g", y = "v", alpha = "a", add = "mean_se",
      position = ggplot2::position_dodge(0.8)), aes.extra)
    pc <- suppressWarnings(do.call(ggbarplot, args))
    bc <- suppressWarnings(ggplot2::ggplot_build(pc))
    expect_s3_class(ggplot2::ggplotGrob(bc), "gtable")
    cbar <- bc$data[[1]]
    ceb <- bc$data[[2]]
    lab <- paste(names(aes.extra), unlist(aes.extra), collapse = " ")
    expect_equal(nrow(ceb), nrow(cbar), info = lab)
    expect_equal(
      as.numeric(cbar$y)[order(as.numeric(cbar$x))],
      ((ceb$ymin + ceb$ymax) / 2)[order(as.numeric(ceb$x))],
      tolerance = 1e-8, info = lab
    )
  }

  # The level ORDER of the alpha column must not change the pairing either: a
  # reversed, an unused and an ordered level set all key the same way ggplot2
  # groups the bars.
  for (acol in list(
    factor(d$a, levels = c("a2", "a1")),
    factor(d$a, levels = c("a1", "a2", "a3")),
    factor(d$a, ordered = TRUE)
  )) {
    dl <- d
    dl$a <- acol
    pl <- suppressWarnings(ggbarplot(dl, x = "g", y = "v", fill = "f",
      alpha = "a", add = "mean_se", position = ggplot2::position_dodge(0.8)))
    bl <- suppressWarnings(ggplot2::ggplot_build(pl))
    expect_s3_class(ggplot2::ggplotGrob(bl), "gtable")
    lbar <- bl$data[[1]]
    leb <- bl$data[[2]]
    expect_equal(nrow(leb), nrow(lbar))
    expect_equal(
      as.numeric(lbar$y)[order(as.numeric(lbar$x))],
      ((leb$ymin + leb$ymax) / 2)[order(as.numeric(leb$x))],
      tolerance = 1e-8
    )
  }
})

test_that("ggbarplot() dodge key follows ggplot2's own grouping rule (#404)", {
  # ggplot2 ids a layer's groups with id() over its DISCRETE columns only
  # (is_discrete(): factor/character/logical). A numeric column mapped to
  # colour/fill therefore contributes nothing to the bars' grouping, and keying
  # the error layer on it adds a dimension the bars do not have: it becomes the
  # slowest-varying factor of the key while the bars ignore it, so the two
  # orderings transpose and every interval carries a neighbour's mean and error.
  # ggpubr 1.0.0 drew this case correctly; it must stay correct.
  set.seed(1)
  d <- data.frame(
    site = rep(c("S1", "S2"), each = 12),
    arm = rep(c("Active", "Placebo"), each = 6, times = 2),
    response = stats::rnorm(24, 10)
  )
  d$dose_mg <- ifelse(d$arm == "Active", 10, 5) # a dose recorded as a NUMBER
  ref <- stats::aggregate(response ~ site + arm, data = d, FUN = mean)
  ref.se <- stats::aggregate(response ~ site + arm, data = d,
    FUN = function(z) stats::sd(z) / sqrt(length(z)))
  p <- suppressWarnings(ggbarplot(d, x = "site", y = "response",
    fill = "dose_mg", alpha = "arm", add = "mean_se",
    position = ggplot2::position_dodge(0.8)))
  b <- suppressWarnings(ggplot2::ggplot_build(p))
  expect_s3_class(ggplot2::ggplotGrob(b), "gtable")
  bar <- b$data[[1]]
  eb <- b$data[[2]]
  expect_equal(nrow(eb), nrow(bar))
  ord.b <- order(as.numeric(bar$x))
  ord.e <- order(as.numeric(eb$x))
  # element-wise at matched x: a transposition cannot pass
  expect_equal(as.numeric(bar$x)[ord.b], as.numeric(eb$x)[ord.e], tolerance = 1e-8)
  expect_equal(
    as.numeric(bar$y)[ord.b],
    ((eb$ymin + eb$ymax) / 2)[ord.e],
    tolerance = 1e-8
  )
  # and the drawn statistics are the independently computed ones
  expect_equal(sort(as.numeric(bar$y)), sort(ref$response), tolerance = 1e-8)
  expect_equal(sort((eb$ymax - eb$ymin) / 2), sort(ref.se$response),
    tolerance = 1e-8)

  # A user-set add.params$group naming a column mapped to NO aesthetic does not
  # split the bars, so it must not split the error layer either. Keying on it
  # produced one error row per (fill, group, alpha) cell - twice as many rows as
  # bars, half of them all-NA and dropped silently, none centred on a bar.
  set.seed(11)
  d2 <- expand.grid(
    g = c("A", "B", "C"), f = c("f1", "f2"), cc = c("c1", "c2"),
    a = c("a1", "a2"), rep = 1:4, stringsAsFactors = FALSE
  )
  d2$v <- stats::rnorm(nrow(d2), 10, 2)
  p2 <- suppressWarnings(ggbarplot(d2, x = "g", y = "v", fill = "f",
    alpha = "a", add = "mean_se", position = ggplot2::position_dodge(0.8),
    add.params = list(group = "cc")))
  b2 <- suppressWarnings(ggplot2::ggplot_build(p2))
  expect_s3_class(ggplot2::ggplotGrob(b2), "gtable")
  bar2 <- b2$data[[1]]
  eb2 <- b2$data[[2]]
  expect_equal(nrow(eb2), nrow(bar2))
  expect_false(any(is.na((eb2$ymin + eb2$ymax) / 2)))
  expect_equal(
    as.numeric(bar2$y)[order(as.numeric(bar2$x))],
    ((eb2$ymin + eb2$ymax) / 2)[order(as.numeric(eb2$x))],
    tolerance = 1e-8
  )
})

test_that("ggbarplot() leaves a non-discrete colour/fill exactly as released (#404)", {
  # When a numeric/integer/Date column is mapped to colour or fill, ggplot2 does
  # not group the bars by it, but desc_statby() still splits the summary on it -
  # so the layer draws more rects than there are dodge slots and two bars share a
  # slot. No key can put an error bar on "its own" bar because the mapping is not
  # one-to-one. The dodge key therefore falls back to the released pairing rather
  # than trading one wrong arrangement for another.
  #
  # Expected values below were captured from ggpubr 1.0.0 (origin/master,
  # 2dccc55, R/ggbarplot.R byte-identical to the CRAN 1.0.0 file) with:
  #   ggplot_build(p)$data[[2]][, c("x", "ymin", "ymax")]
  set.seed(5)
  d <- expand.grid(
    g = paste0("x", 1:3), f = paste0("f", 1:2), a = paste0("a", 1:2),
    rep = 1:5, stringsAsFactors = TRUE
  )
  d$v <- stats::rnorm(nrow(d), 10 * as.integer(d$g) + 3 * as.integer(d$f) +
    as.integer(d$a))
  d$f <- as.integer(factor(d$f)) # fill mapped to an INTEGER column
  p <- suppressWarnings(ggbarplot(d, x = "g", y = "v", fill = "f", alpha = "a",
    add = "mean_se", position = ggplot2::position_dodge()))
  b <- suppressWarnings(ggplot2::ggplot_build(p))
  expect_s3_class(ggplot2::ggplotGrob(b), "gtable")
  eb <- b$data[[2]]
  expect_equal(nrow(eb), 12L)
  expect_equal(as.numeric(eb$x), c(
    0.64375, 1.64375, 2.64375, 0.88125, 1.88125, 2.88125,
    1.11875, 2.11875, 3.11875, 1.35625, 2.35625, 3.35625
  ), tolerance = 1e-8)
  expect_equal((eb$ymin + eb$ymax) / 2, c(
    13.43277994, 23.77273185, 33.75847997, 17.29500968, 27.60582578,
    35.99321436, 14.97955727, 25.23713646, 35.62538695, 18.54933911,
    28.64474292, 38.12591724
  ), tolerance = 1e-7)
})

test_that("ggbarplot() leaves a statistic-named alpha column as released (#404)", {
  # desc_statby() names its output columns after the statistics it computes, so
  # an `alpha` column called `se`/`sd`/`median`/... is REPLACED in the summary by
  # the computed numeric statistic. geom_exec() resolves the bar layer's alpha
  # against that, ggplot2 sees a continuous column and does not group the bars by
  # it, so no key can match one error bar to one bar - the same degeneracy as a
  # non-discrete colour/fill, and the released arrangement is likewise kept.
  #
  # Expected values captured from ggpubr 1.0.0 (origin/master, 2dccc55) with:
  #   ggplot_build(p)$data[[2]][, c("x", "ymin", "ymax")]
  set.seed(1)
  d <- data.frame(
    g = rep(c("A", "B"), each = 12), f = rep(c("f1", "f2"), 12),
    se = rep(c("a1", "a2"), each = 6, times = 2), v = stats::rnorm(24, 10)
  )
  p <- suppressWarnings(ggbarplot(d, x = "g", y = "v", fill = "f", alpha = "se",
    add = "mean_se", position = ggplot2::position_dodge(0.8)))
  b <- suppressWarnings(ggplot2::ggplot_build(p))
  expect_s3_class(ggplot2::ggplotGrob(b), "gtable")
  eb <- b$data[[2]]
  expect_equal(as.numeric(eb$x),
    c(0.7, 1.7, 0.9, 1.9, 1.1, 2.1, 1.3, 2.3), tolerance = 1e-8)
  expect_equal((eb$ymin + eb$ymax) / 2, c(
    9.622475116, 10.16250002, 10.31948525, 9.561400905,
    10.85833052, 10.60492118, 10.27425985, 9.795561975
  ), tolerance = 1e-7)

  # `len` is NOT one of those columns, so it is an ordinary alpha column and DOES
  # get the fix: 4 of 8 on their own bar in 1.0.0, 8 of 8 now.
  names(d)[names(d) == "se"] <- "len"
  p2 <- suppressWarnings(ggbarplot(d, x = "g", y = "v", fill = "f",
    alpha = "len", add = "mean_se", position = ggplot2::position_dodge(0.8)))
  b2 <- suppressWarnings(ggplot2::ggplot_build(p2))
  bar2 <- b2$data[[1]]
  eb2 <- b2$data[[2]]
  expect_equal(
    as.numeric(bar2$y)[order(as.numeric(bar2$x))],
    ((eb2$ymin + eb2$ymax) / 2)[order(as.numeric(eb2$x))],
    tolerance = 1e-8
  )
})
