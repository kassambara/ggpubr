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
