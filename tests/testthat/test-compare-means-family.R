# Regression tests for #592 / #624: stat_compare_means(family = ...) was ignored
# when `comparisons` was set (the comparisons path delegates to
# ggsignif::geom_signif and did not forward `family`).

.text_family <- function(p) {
  b <- ggplot2::ggplot_build(p)
  fam <- character(0)
  for (i in seq_along(b$data)) {
    d <- b$data[[i]]
    if ("family" %in% names(d)) fam <- c(fam, unique(as.character(d$family)))
  }
  fam
}

cmp <- list(c("0.5", "1"), c("1", "2"))

test_that("family is forwarded to comparison brackets (#592, #624)", {
  p <- ggboxplot(ToothGrowth, x = "dose", y = "len") +
    stat_compare_means(comparisons = cmp, method = "t.test", family = "mono")
  expect_true("mono" %in% .text_family(p))
  # renders at draw time
  expect_no_error(ggplot2::ggplot_gtable(ggplot2::ggplot_build(p)))
})

test_that("default family is unchanged when comparisons set (no regression, #592)", {
  p <- ggboxplot(ToothGrowth, x = "dose", y = "len") +
    stat_compare_means(comparisons = cmp, method = "t.test")
  fam <- .text_family(p)
  # No non-empty family is forced onto the labels (ggsignif default is "").
  expect_true(length(fam) == 0 || all(fam == ""))
  expect_no_error(ggplot2::ggplot_gtable(ggplot2::ggplot_build(p)))
})

test_that("family still works in the non-comparisons path (no regression, #592)", {
  p <- ggboxplot(ToothGrowth, x = "dose", y = "len") +
    stat_compare_means(method = "t.test", family = "mono")
  expect_true("mono" %in% .text_family(p))
})
