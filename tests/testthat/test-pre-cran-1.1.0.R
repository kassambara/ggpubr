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
  # a character `alpha` naming no column reports the typo instead of failing in grid
  expect_error(
    ggbarplot(d, x = "g", y = "v", fill = "f", alpha = "zzz", add = "mean_se"),
    "not a column of", fixed = TRUE
  )
})

test_that("show.n counts the marks the panel draws, NA excluded and Inf kept", {
  # ggplot2 removes NA/NaN before drawing but KEEPS +/-Inf, squeezing it onto the
  # panel edge, so an infinite value is still a visible mark. The label must
  # match the marks in both directions.
  d <- data.frame(
    g = rep(c("a", "b", "c"), each = 8),
    y = c(1:6, NA, NaN,        # a: 6 drawn
      1:6, Inf, Inf,           # b: 8 drawn (Inf is drawn)
      1:8)                     # c: 8 drawn
  )
  labs <- function(p) {
    b <- suppressWarnings(ggplot2::ggplot_build(p))
    ggplot2::ggplotGrob(b)
    z <- unlist(lapply(b$data, function(q) if ("label" %in% names(q)) as.character(q$label)))
    as.integer(sub("^n = ", "", z[grepl("^n = ", z)]))
  }
  # base-R reference: what ggplot2 will actually draw, per group
  ref <- as.integer(tapply(d$y, d$g, function(v) sum(!is.na(v))))
  expect_equal(ref, c(6L, 8L, 8L))
  for (p in list(
    suppressWarnings(ggstripchart(d, "g", "y", show.n = TRUE)),
    suppressWarnings(ggboxplot(d, "g", "y", add = "jitter", show.n = TRUE)),
    suppressWarnings(ggviolin(d, "g", "y", show.n = TRUE))
  )) {
    expect_equal(sort(labs(p)), sort(ref))
  }
})
