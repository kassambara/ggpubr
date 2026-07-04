context("test-stat-pvalue-manual-pformat")

# stat_pvalue_manual() exposes the same p-value formatting knobs as its sibling
# bracket layers (stat_compare_means / geom_pwc / stat_anova_test):
# p.format.style, p.leading.zero, p.min.threshold, p.decimal.mark. All default
# so that the rendered labels are byte-identical to before (opt-in only).

.make_stat <- function(p = c(0.015625, 0.0000000004, 0.7)) {
  data.frame(
    group1 = c("0.5", "0.5", "1"),
    group2 = c("1", "2", "2"),
    p = p,
    y.position = c(30, 36, 40),
    stringsAsFactors = FALSE
  )
}

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

test_that("defaults leave the rendered p labels byte-identical (opt-in only)", {
  st <- .make_stat()
  # The style arguments at their defaults must reproduce the plain
  # format_p_value(p, digits = 3) output exactly.
  expect_identical(
    .plotted_labels(stat_pvalue_manual(st, label = "p")),
    format_p_value(st$p, digits = 3)
  )
  # Explicitly passing the defaults is a no-op vs omitting them.
  expect_identical(
    .plotted_labels(stat_pvalue_manual(st, label = "p")),
    .plotted_labels(stat_pvalue_manual(
      st, label = "p", p.format.style = "default",
      p.leading.zero = NULL, p.min.threshold = NULL, p.decimal.mark = NULL
    ))
  )
})

test_that("p.min.threshold renders very small p-values as '< threshold'", {
  st <- .make_stat()
  labs <- .plotted_labels(stat_pvalue_manual(st, label = "p", p.min.threshold = 0.001))
  expect_equal(labs[2], "< 0.001")
  # values above the threshold are untouched
  expect_equal(labs[c(1, 3)], format_p_value(st$p[c(1, 3)], digits = 3))
})

test_that("p.format.style selects a journal style", {
  st <- .make_stat()
  expect_equal(
    .plotted_labels(stat_pvalue_manual(st, label = "p", p.format.style = "nejm")),
    format_p_value(st$p, style = "nejm", digits = 3)
  )
})

test_that("p.leading.zero = FALSE drops the leading zero", {
  st <- .make_stat()
  labs <- .plotted_labels(stat_pvalue_manual(st, label = "p", p.leading.zero = FALSE))
  expect_equal(labs[1], ".0156")
})

test_that("p.decimal.mark controls the decimal separator", {
  st <- .make_stat()
  labs <- .plotted_labels(stat_pvalue_manual(st, label = "p", p.decimal.mark = ","))
  expect_equal(labs[1], "0,0156")
})

test_that("p.digits = NULL disables all formatting, including the style args", {
  st <- .make_stat()
  # NULL is the documented raw opt-out; the style args must then be ignored.
  expect_equal(
    .plotted_labels(stat_pvalue_manual(
      st, label = "p", p.digits = NULL,
      p.format.style = "nejm", p.min.threshold = 0.001
    )),
    as.character(st$p)
  )
})

test_that("style args are a no-op on character, significance and glue labels", {
  st <- .make_stat()
  st$p.signif <- c("*", "****", "ns")
  # character significance column
  expect_equal(
    .plotted_labels(stat_pvalue_manual(
      st, label = "p.signif", p.format.style = "nejm", p.min.threshold = 0.001
    )),
    st$p.signif
  )
  # glue expression is evaluated on the raw column, unaffected by style args
  expect_equal(
    .plotted_labels(stat_pvalue_manual(
      st, label = "p = {p}", p.format.style = "nejm", p.leading.zero = FALSE
    )),
    paste0("p = ", as.character(st$p))
  )
})

test_that("style args do not touch non-p numeric columns or out-of-range values", {
  st <- .make_stat()
  st$statistic <- c(5.3, -2.1, 12.0)
  # a numeric column named like a p-value but out of [0, 1]
  st_bad <- .make_stat(p = c(0.5, 5.3, 0.7))

  expect_warning(
    labs_stat <- .plotted_labels(stat_pvalue_manual(
      st, label = "statistic", p.min.threshold = 0.001, p.format.style = "nejm"
    )),
    NA
  )
  expect_equal(labs_stat, as.character(st$statistic))

  expect_warning(
    labs_bad <- .plotted_labels(stat_pvalue_manual(
      st_bad, label = "p", p.min.threshold = 0.001
    )),
    NA
  )
  expect_equal(labs_bad, as.character(st_bad$p))
})
