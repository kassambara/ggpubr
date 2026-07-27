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
})
