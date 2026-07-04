context("test-stat-cor-conf-int")

# stat_cor() exposes the confidence interval of the correlation coefficient as
# computed variables conf.int.low, conf.int.high (numeric) and conf.int.label
# (formatted), plus a conf.level argument (#418). Pearson only; NA otherwise.
# Purely additive: the default label is unchanged.

.cor_label <- function(p) {
  b <- ggplot2::ggplot_build(p)
  d <- b$data[[which(vapply(b$data, function(x) "label" %in% names(x), logical(1)))[1]]]
  as.character(d$label)[1]
}

set.seed(1)
.df <- data.frame(x = rnorm(30))
.df$y <- .df$x * 0.5 + rnorm(30)
.sp <- ggscatter(.df, "x", "y")

test_that("the default stat_cor label is unchanged (no regression, #418)", {
  # The default composed label is R + p only; the CI variables never join it.
  lab <- .cor_label(.sp + stat_cor(p.accuracy = 0.001, r.accuracy = 0.01))
  expect_match(lab, "^italic\\(R\\)~`=`~-?[0-9.]+\\*`,`~italic\\(p\\)~`=`~")
  expect_false(grepl("CI", lab))
})

test_that("conf.int.label reports the Pearson CI (text output)", {
  ci <- cor.test(.df$x, .df$y)$conf.int
  lab <- .cor_label(
    .sp + stat_cor(aes(label = after_stat(conf.int.label)), output.type = "text")
  )
  expect_equal(lab, paste0("95% CI [", signif(ci[1], 2), ", ", signif(ci[2], 2), "]"))
})

test_that("conf.int.label is valid plotmath and parses", {
  lab <- .cor_label(.sp + stat_cor(aes(label = after_stat(conf.int.label))))
  expect_match(lab, "^\"95% CI \\[")
  expect_error(parse(text = lab), NA)
})

test_that("conf.level controls the interval and the label prefix", {
  ci99 <- cor.test(.df$x, .df$y, conf.level = 0.99)$conf.int
  lab <- .cor_label(
    .sp + stat_cor(aes(label = after_stat(conf.int.label)),
      output.type = "text", conf.level = 0.99)
  )
  expect_equal(lab, paste0("99% CI [", signif(ci99[1], 2), ", ", signif(ci99[2], 2), "]"))
})

test_that("conf.int.low/high numeric match cor.test bounds (Pearson)", {
  ci <- cor.test(.df$x, .df$y)$conf.int
  b <- ggplot2::ggplot_build(
    .sp + stat_cor(aes(label = after_stat(paste(conf.int.low, conf.int.high))))
  )
  d <- b$data[[which(vapply(b$data, function(x) "label" %in% names(x), logical(1)))[1]]]
  expect_equal(as.character(d$label)[1],
               paste(signif(ci[1], 2), signif(ci[2], 2)))
})

test_that("conf.int variables are NA for Spearman and Kendall (no error)", {
  for (m in c("spearman", "kendall")) {
    lab <- .cor_label(
      .sp + stat_cor(aes(label = after_stat(conf.int.label)),
        method = m, output.type = "text")
    )
    expect_true(is.na(lab))
  }
})

test_that("invalid conf.level is rejected with a clear error", {
  expect_error(stat_cor(conf.level = 95), "between 0 and 1")
  expect_error(stat_cor(conf.level = 2), "between 0 and 1")
  expect_error(stat_cor(conf.level = -0.1), "between 0 and 1")
  expect_error(stat_cor(conf.level = c(0.9, 0.95)), "single number")
  expect_error(stat_cor(conf.level = NA), "between 0 and 1")
  # valid values are accepted
  expect_error(stat_cor(conf.level = 0.9), NA)
})

test_that("conf.int.label honors r.accuracy and r.leading.zero", {
  ci <- cor.test(.df$x, .df$y)$conf.int
  lab_acc <- .cor_label(
    .sp + stat_cor(aes(label = after_stat(conf.int.label)),
      output.type = "text", r.accuracy = 0.001)
  )
  expect_equal(lab_acc, paste0("95% CI [",
    formatC(ci[1], digits = 3, format = "f"), ", ",
    formatC(ci[2], digits = 3, format = "f"), "]"))
})
