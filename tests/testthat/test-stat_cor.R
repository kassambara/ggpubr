context("test-stat_cor")

.cor_label <- function(p) {
  b <- ggplot2::ggplot_build(p)
  d <- b$data[[which(vapply(b$data, function(x) "label" %in% names(x), logical(1)))[1]]]
  as.character(d$label)[1]
}

set.seed(1)
.df <- data.frame(x = rnorm(20))
.df$y <- .df$x * 0.4 + rnorm(20)
.sp <- ggscatter(.df, "x", "y")

test_that("stat_cor default label is unchanged (no regression, #540/#541)", {
  lab <- .cor_label(.sp + stat_cor(p.accuracy = 0.001, r.accuracy = 0.01))
  expect_equal(lab, "italic(R)~`=`~0.20*`,`~italic(p)~`=`~0.392")
})

test_that("stat_cor r.leading.zero = FALSE drops the coefficient leading zero (#540)", {
  # For expression output the stripped value is QUOTED so plotmath renders it
  # literally (a bare .20 would be re-normalized to 0.20 at draw time). The
  # quoted form is what actually renders as ".20"/"-.87".
  lab <- .cor_label(.sp + stat_cor(p.accuracy = 0.001, r.accuracy = 0.01, r.leading.zero = FALSE))
  expect_true(grepl('italic(R)~`=`~".20"', lab, fixed = TRUE))
  # negative coefficient keeps the minus sign outside the quotes: -0.87 -> -".87"
  lab.neg <- .cor_label(
    ggscatter(mtcars, "wt", "mpg") + stat_cor(r.accuracy = 0.01, r.leading.zero = FALSE)
  )
  expect_true(grepl('`=`~-".87"', lab.neg, fixed = TRUE))
})

test_that("stat_cor p.coef.name = 'P' uses an uppercase p-value symbol (#541)", {
  lab <- .cor_label(.sp + stat_cor(p.accuracy = 0.001, r.accuracy = 0.01, p.coef.name = "P"))
  expect_true(grepl("italic(P)~`=`~", lab, fixed = TRUE))
  expect_false(grepl("italic(p)", lab, fixed = TRUE))
})

test_that("stat_cor APA style: both leading-zero drops + uppercase P together (#540/#541)", {
  lab <- .cor_label(.sp + stat_cor(
    p.accuracy = 0.001, r.accuracy = 0.01,
    r.leading.zero = FALSE, p.leading.zero = FALSE, p.coef.name = "P"
  ))
  # Both stripped values are quoted (render as .20 / .392 respectively).
  expect_equal(lab, 'italic(R)~`=`~".20"*`,`~italic(P)~`=`~".392"')
})

test_that("stat_cor leading-zero drop survives plotmath rendering (#540)", {
  # Guard against the plotmath re-normalization bug: the stripped value must be
  # quoted in the expression so it renders literally. A bare `.20` would draw
  # as `0.20`. Rendering the plot must not error.
  p <- .sp + stat_cor(r.accuracy = 0.01, r.leading.zero = FALSE, label.x = -1)
  lab <- .cor_label(p)
  expect_true(grepl('"', lab, fixed = TRUE))                   # value is quoted
  # the R coefficient itself is not a bare 0.-prefixed number
  expect_false(grepl('italic(R)~`=`~0.', lab, fixed = TRUE))
  expect_no_error(ggplot2::ggplot_gtable(ggplot2::ggplot_build(p)))
})

test_that("stat_cor new label options work with text output (#540/#541)", {
  lab <- .cor_label(.sp + stat_cor(
    p.accuracy = 0.001, r.accuracy = 0.01, output.type = "text",
    r.leading.zero = FALSE, p.coef.name = "P"
  ))
  expect_true(grepl("R = .20", lab, fixed = TRUE))
  expect_true(grepl("P = ", lab, fixed = TRUE))
})
