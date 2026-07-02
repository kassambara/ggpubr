# Regression tests for #293: stat_compare_means(comparisons = ) shows UNADJUSTED
# p-values. Users are now informed once per session (message + docs) and pointed
# to the adjusted-pairwise alternatives.

.cmp3 <- list(c("0.5", "1"), c("1", "2"), c("0.5", "2"))

test_that("multi-comparison stat_compare_means informs about unadjusted p-values (#293)", {
  old <- options(rlib_message_verbosity = "verbose")  # force the once-message
  on.exit(options(old), add = TRUE)
  expect_message(
    ggboxplot(ToothGrowth, "dose", "len") +
      stat_compare_means(comparisons = .cmp3),
    "unadjusted"
  )
  # the note points to the adjusted-pairwise alternatives
  expect_message(
    ggboxplot(ToothGrowth, "dose", "len") +
      stat_compare_means(comparisons = .cmp3),
    "geom_pwc|stat_pvalue_manual"
  )
})

test_that("single comparison does NOT emit the note (#293)", {
  old <- options(rlib_message_verbosity = "verbose")
  on.exit(options(old), add = TRUE)
  expect_no_message(
    ggboxplot(ToothGrowth, "dose", "len") +
      stat_compare_means(comparisons = list(c("0.5", "1")))
  )
})

test_that("stat_compare_means(comparisons=) still renders (no regression, #293)", {
  p <- ggboxplot(ToothGrowth, "dose", "len") +
    stat_compare_means(comparisons = .cmp3)
  expect_no_error(ggplot2::ggplot_gtable(ggplot2::ggplot_build(p)))
})

# Regression tests for #362: tip.length.ref is not supported on the
# stat_compare_means(comparisons=) path (delegates to ggsignif). Instead of
# silently ignoring it, users are informed once and pointed to stat_pvalue_manual.

test_that("stat_compare_means(comparisons=, tip.length.ref=) informs it is ignored (#362)", {
  old <- options(rlib_message_verbosity = "verbose")
  on.exit(options(old), add = TRUE)
  expect_message(
    ggboxplot(ToothGrowth, "dose", "len") +
      stat_compare_means(comparisons = list(c("0.5", "1")), tip.length.ref = "axis"),
    "tip.length.ref.*not supported|ignored"
  )
  expect_message(
    ggboxplot(ToothGrowth, "dose", "len") +
      stat_compare_means(comparisons = list(c("0.5", "1")), tip.length.ref = "axis"),
    "stat_pvalue_manual"
  )
})

test_that("stat_compare_means without tip.length.ref emits no such note (#362)", {
  old <- options(rlib_message_verbosity = "verbose")
  on.exit(options(old), add = TRUE)
  expect_no_message(
    ggboxplot(ToothGrowth, "dose", "len") +
      stat_compare_means(comparisons = list(c("0.5", "1"))),
    message = "tip.length.ref"
  )
})

test_that("passing tip.length.ref to stat_compare_means does not change output (#362)", {
  suppressMessages({
    with_arg <- ggplot2::ggplot_build(
      ggboxplot(ToothGrowth, "dose", "len") +
        stat_compare_means(comparisons = list(c("0.5", "1")), tip.length.ref = "axis")
    )
    without <- ggplot2::ggplot_build(
      ggboxplot(ToothGrowth, "dose", "len") +
        stat_compare_means(comparisons = list(c("0.5", "1")))
    )
  })
  expect_equal(with_arg$data, without$data)
})
