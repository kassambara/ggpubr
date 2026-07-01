
test_that("ggsummarytable forwards angle to the text layer (#595)", {
  # Regression: angle rotates the summary-table text
  p90 <- ggsummarytable(ToothGrowth, x = "dose", y = "len",
                        summaries = "mean", angle = 90)
  expect_true(all(ggplot2::layer_data(p90, 1)$angle == 90))

  # No-regression: default (no angle) is unchanged (angle 0)
  p0 <- ggsummarytable(ToothGrowth, x = "dose", y = "len", summaries = "mean")
  expect_true(all(ggplot2::layer_data(p0, 1)$angle == 0))
})
