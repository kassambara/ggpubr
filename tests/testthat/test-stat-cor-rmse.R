context("test-stat-cor-rmse")

# stat_cor() exposes RMSE/RMSD (root mean square deviation between x and y) as a
# computed variable `rmse` and a formatted label `rmse.label` (#458). Purely
# additive: the default label is unchanged.

.cor_label <- function(p) {
  b <- ggplot2::ggplot_build(p)
  d <- b$data[[which(vapply(b$data, function(x) "label" %in% names(x), logical(1)))[1]]]
  as.character(d$label)[1]
}

set.seed(1)
.df <- data.frame(x = rnorm(20))
.df$y <- .df$x * 0.4 + rnorm(20)
.sp <- ggscatter(.df, "x", "y")

.expected_rmse <- function(x, y, r.digits = 2) {
  ok <- stats::complete.cases(x, y)
  signif(sqrt(mean((x[ok] - y[ok])^2)), r.digits)
}

test_that("rmse.label reports the RMSE between x and y (text output)", {
  expected <- .expected_rmse(.df$x, .df$y)
  lab <- .cor_label(
    .sp + stat_cor(aes(label = after_stat(rmse.label)), output.type = "text")
  )
  expect_equal(lab, paste0("RMSE = ", expected))
})

test_that("rmse.label is a valid plotmath expression by default", {
  expected <- .expected_rmse(.df$x, .df$y)
  lab <- .cor_label(.sp + stat_cor(aes(label = after_stat(rmse.label))))
  expect_equal(lab, paste0("italic(RMSE)~`=`~", expected))
  # must parse as plotmath (draw-time safety, not just build)
  expect_error(parse(text = lab), NA)
})

test_that("rmse.label honors r.accuracy for decimal places", {
  ok <- stats::complete.cases(.df$x, .df$y)
  raw <- sqrt(mean((.df$x[ok] - .df$y[ok])^2))
  lab <- .cor_label(
    .sp + stat_cor(aes(label = after_stat(rmse.label)),
      output.type = "text", r.accuracy = 0.001
    )
  )
  expect_equal(lab, paste0("RMSE = ", formatC(raw, digits = 3, format = "f")))
})

test_that("the default stat_cor label is unchanged (no regression, #458)", {
  lab <- .cor_label(.sp + stat_cor(p.accuracy = 0.001, r.accuracy = 0.01))
  expect_equal(lab, "italic(R)~`=`~0.20*`,`~italic(p)~`=`~0.392")
  expect_false(grepl("RMSE", lab))
})

test_that("rmse is computed per group", {
  d <- .df
  d$g <- rep(c("a", "b"), each = 10)
  sp <- ggscatter(d, "x", "y", color = "g")
  b <- ggplot2::ggplot_build(sp + stat_cor(aes(color = g, label = after_stat(rmse.label)), output.type = "text"))
  dd <- b$data[[which(vapply(b$data, function(x) "label" %in% names(x), logical(1)))[1]]]
  labs <- as.character(dd$label)
  exp_a <- .expected_rmse(d$x[d$g == "a"], d$y[d$g == "a"])
  exp_b <- .expected_rmse(d$x[d$g == "b"], d$y[d$g == "b"])
  expect_setequal(labs, c(paste0("RMSE = ", exp_a), paste0("RMSE = ", exp_b)))
})
