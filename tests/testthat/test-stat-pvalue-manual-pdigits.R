context("test-stat-pvalue-manual-pdigits")

# p.digits formats numeric p-value label columns for display (restores clean
# labels with rstatix >= 1.0.0 full-precision pairwise p-values), while leaving
# character labels (significance symbols, pre-formatted strings, glue output)
# unchanged. Opt out with p.digits = NULL.

# Build a stat.test with deterministic p-values covering a mid-range value,
# a very small value, and a large value.
.make_stat <- function(p = c(0.015625, 0.0000123, 0.7)) {
  data.frame(
    group1 = c("0.5", "0.5", "1"),
    group2 = c("1", "2", "2"),
    p = p,
    p.signif = c("*", "****", "ns"),
    y.position = c(30, 36, 40),
    stringsAsFactors = FALSE
  )
}

# Extract the plotted label aesthetic (one per comparison, in stat.test row
# order) from a built ggplot. The bracket layer stores several rows per bracket
# (the bracket corners), all sharing the same `group` (= stat.test row index),
# so we keep the first label of each group and order by group.
.plotted_labels <- function(layer) {
  p <- ggboxplot(ToothGrowth, x = "dose", y = "len") + layer
  b <- ggplot2::ggplot_build(p)
  for (d in b$data) {
    if (all(c("label", "group") %in% names(d))) {
      first <- d[!duplicated(d$group), ]
      first <- first[order(first$group), ]
      return(as.character(first$label))
    }
  }
  stop("no label aesthetic found in built plot")
}

test_that("numeric p label is rounded by default (p.digits = 3)", {
  st <- .make_stat()
  expect_equal(
    .plotted_labels(stat_pvalue_manual(st, label = "p")),
    format_p_value(st$p, digits = 3)
  )
  # concrete expectation: 0.015625 -> 0.0156
  expect_true("0.0156" %in% .plotted_labels(stat_pvalue_manual(st, label = "p")))
})

test_that("p.digits controls the number of significant digits", {
  st <- .make_stat(p = c(0.012345, 0.5, 0.9))
  expect_equal(
    .plotted_labels(stat_pvalue_manual(st, label = "p", p.digits = 2)),
    format_p_value(st$p, digits = 2)
  )
})

test_that("p.digits = NULL prints the raw numeric value (opt-out)", {
  st <- .make_stat()
  expect_equal(
    .plotted_labels(stat_pvalue_manual(st, label = "p", p.digits = NULL)),
    as.character(st$p)
  )
})

test_that("character significance labels are unaffected by p.digits", {
  st <- .make_stat()
  # explicit character column
  expect_equal(
    .plotted_labels(stat_pvalue_manual(st, label = "p.signif")),
    st$p.signif
  )
  # default label = NULL resolves to the significance column
  expect_equal(
    .plotted_labels(stat_pvalue_manual(st)),
    st$p.signif
  )
})

test_that("numeric NON-p label columns are left untouched (no NA, no warning)", {
  # stat_pvalue_manual is generic: the label column need not be a p-value.
  # Columns outside [0, 1] (statistic, n, effect size, ...) must NOT be routed
  # through format_p_value(), which would coerce them to NA with a warning.
  st <- .make_stat()
  st$statistic <- c(5.3, -2.1, 12.0)
  st$n <- c(20L, 20L, 20L)

  expect_warning(
    labs_stat <- .plotted_labels(stat_pvalue_manual(st, label = "statistic")),
    NA
  )
  expect_equal(labs_stat, as.character(st$statistic))

  expect_warning(
    labs_n <- .plotted_labels(stat_pvalue_manual(st, label = "n")),
    NA
  )
  expect_equal(labs_n, as.character(st$n))
})

test_that("a p-named column holding out-of-[0,1] values is left untouched", {
  # A numeric column merely NAMED like a p-value but holding other quantities
  # must NOT be routed through format_p_value() (which would return NA + warn).
  # The range guard leaves the whole column as-is if any value is outside [0, 1].
  st <- .make_stat(p = c(5.3, 0.2, 0.7))
  expect_warning(
    labs <- .plotted_labels(stat_pvalue_manual(st, label = "p")),
    NA
  )
  expect_equal(labs, as.character(st$p))
})

test_that("glue label expressions are unaffected (documented limitation)", {
  st <- .make_stat()
  # glue is evaluated on the raw numeric column, so it keeps full precision;
  # users round inside the expression, e.g. {signif(p, 3)}.
  expect_equal(
    .plotted_labels(stat_pvalue_manual(st, label = "{p}")),
    as.character(st$p)
  )
})

test_that("NA p-values do not error and render as NA", {
  st <- .make_stat(p = c(0.015625, NA, 0.7))
  labs <- .plotted_labels(stat_pvalue_manual(st, label = "p"))
  expect_equal(labs[2], NA_character_)
  expect_equal(labs[c(1, 3)], format_p_value(c(0.015625, 0.7), digits = 3))
})
