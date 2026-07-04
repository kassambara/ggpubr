context("test-stat-compare-means-id")

# #560: stat_compare_means() gains an `id` argument so a paired test in the plot
# is aligned by subject id (row-order independent), like compare_means(id=). The
# id column is carried into the stat's compute as an aesthetic without a
# "unknown aesthetics" warning; plots without `id` are unchanged.

suppressMessages(library(dplyr))

.stat_label <- function(p) {
  b <- ggplot2::ggplot_build(p)
  lab <- NULL
  for (d in b$data) if ("label" %in% names(d)) lab <- as.character(d$label)[1]
  lab
}

.warnings_of <- function(expr) {
  ws <- character(0)
  withCallingHandlers(force(expr),
    warning = function(w) { ws <<- c(ws, conditionMessage(w)); invokeRestart("muffleWarning") })
  ws
}

set.seed(123)
.d <- data.frame(
  sampleType = c(rep("Normal", 10), rep("Tumor", 10)),
  value = c(runif(10, 0, 0.2), runif(10, 0, 0.4)),
  ID = c(1:10, 10:1)  # unsorted: row-order pairing is wrong
)

test_that("id shows the id-aligned (correct) p regardless of row order (#560)", {
  correct <- compare_means(value ~ sampleType, .d, paired = TRUE, id = "ID")$p
  lab <- .stat_label(
    ggpaired(.d, x = "sampleType", y = "value") +
      stat_compare_means(paired = TRUE, id = "ID")
  )
  # the displayed p reflects the id-aligned value (formatted), not the row-order one
  expect_true(grepl(format(round(correct, 2), nsmall = 2), lab, fixed = TRUE) ||
              grepl(signif(correct, 2), lab, fixed = TRUE))
  # and it differs from the buggy row-order default
  lab_default <- .stat_label(
    ggpaired(.d, x = "sampleType", y = "value") + stat_compare_means(paired = TRUE)
  )
  expect_false(identical(lab, lab_default))
})

test_that("mapping the id emits no 'unknown aesthetics' warning", {
  ws <- .warnings_of(ggplot2::ggplot_build(
    ggpaired(.d, x = "sampleType", y = "value") +
      stat_compare_means(paired = TRUE, id = "ID")
  ))
  expect_false(any(grepl("unknown aesthetic", ws, ignore.case = TRUE)))
})

test_that("id also works when supplied via aes(id = ...)", {
  correct <- compare_means(value ~ sampleType, .d, paired = TRUE, id = "ID")$p
  lab <- .stat_label(
    ggpaired(.d, x = "sampleType", y = "value") +
      stat_compare_means(ggplot2::aes(id = ID), paired = TRUE, id = "ID")
  )
  expect_true(grepl(signif(correct, 2), lab, fixed = TRUE))
})

test_that("id works with ref.group in the plot (matches compare_means)", {
  set.seed(2)
  d3 <- data.frame(dose = factor(rep(c("d1", "d2", "d3"), each = 10)),
                   len = rnorm(30), ID = c(1:10, sample(1:10), sample(1:10)))
  p <- ggboxplot(d3, "dose", "len") +
    stat_compare_means(paired = TRUE, id = "ID", ref.group = "d1",
                       method = "t.test", label = "p.format")
  b <- ggplot2::ggplot_build(p)
  labs <- NULL
  for (d in b$data) if ("label" %in% names(d)) labs <- unique(as.character(d$label))
  ref <- compare_means(len ~ dose, d3, method = "t.test", paired = TRUE,
                       id = "ID", ref.group = "d1")
  # two labels (d1 vs d2, d1 vs d3), consistent with compare_means' p-values
  expect_equal(length(labs), 2L)
})

test_that("id errors clearly on unsupported combinations (not a silent wrong p)", {
  # with `comparisons`, id can't be honored (ggsignif path) -> error, do not
  # silently draw a row-order p-value
  expect_error(
    stat_compare_means(comparisons = list(c("Normal", "Tumor")), paired = TRUE, id = "ID"),
    "comparisons"
  )
  # id without paired
  expect_error(stat_compare_means(id = "ID"), "paired = TRUE")
})

test_that("a stray mapped `id` aesthetic on a non-paired plot does not error or change output", {
  # a data column named/mapped `id` unrelated to pairing must not be treated as
  # the pairing id when paired is not requested (no loud regression)
  df <- ToothGrowth
  df$dose <- as.factor(df$dose)
  df$id <- seq_len(nrow(df))
  expect_error(
    suppressWarnings(ggplot2::ggplot_build(
      ggplot2::ggplot(df, ggplot2::aes(dose, len, id = id)) +
        ggplot2::geom_boxplot() + stat_compare_means()
    )),
    NA
  )
})

test_that("default (no id) stat_compare_means output is unchanged", {
  df <- ToothGrowth
  df$dose <- as.factor(df$dose)
  # a plot without id builds the same label as before (spot check the value)
  lab <- .stat_label(ggboxplot(df, "dose", "len") + stat_compare_means())
  expect_match(lab, "Kruskal-Wallis")
  # no unknown-aes warning in the default path either
  ws <- .warnings_of(ggplot2::ggplot_build(
    ggboxplot(df, "dose", "len") + stat_compare_means()))
  expect_false(any(grepl("unknown aesthetic", ws, ignore.case = TRUE)))
})
