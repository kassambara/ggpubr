context("test-select-remove")

# Expected values are recomputed from the raw data with base R rather than
# copied from a previous ggpubr run, so the assertions fail if the filtering
# regresses rather than tracking whatever the builder happens to produce.
.med <- function(dose) {
  d <- ToothGrowth[ToothGrowth$dose %in% dose, ]
  as.numeric(tapply(d$len, factor(d$dose), stats::median))
}

.drawn_medians <- function(p) {
  suppressWarnings(ggplot2::ggplot_build(p)$data[[1]]$middle)
}

test_that("select and remove naming the same item is an error", {
  expect_error(
    ggboxplot(ToothGrowth, "dose", "len", select = c("0.5", "1"), remove = "0.5"),
    "have values in common"
  )
  # the offending value is named, so the user can see which one
  expect_error(
    ggboxplot(ToothGrowth, "dose", "len", select = c("0.5", "1"), remove = "0.5"),
    "0.5"
  )
  # more than one overlapping value is listed
  expect_error(
    ggboxplot(ToothGrowth, "dose", "len",
      select = c("0.5", "1", "2"), remove = c("0.5", "2")
    ),
    "0\\.5, 2"
  )
})

test_that("the guard fires only when BOTH are supplied and they overlap", {
  # select alone, remove alone, and a disjoint pair are all legal
  expect_error(ggboxplot(ToothGrowth, "dose", "len", select = c("0.5", "1")), NA)
  expect_error(ggboxplot(ToothGrowth, "dose", "len", remove = "0.5"), NA)
  expect_error(
    ggboxplot(ToothGrowth, "dose", "len", select = c("0.5", "1"), remove = "2"),
    NA
  )
})

test_that("select and remove together draw the right groups with the right values", {
  # Disjoint: `remove` names a group `select` already excluded, so the result is
  # the two selected doses. Before the fix this drew 7.15 for dose 0.5, a value
  # belonging to no group, because the mask was built from the unfiltered data.
  p <- ggboxplot(ToothGrowth, "dose", "len", select = c("0.5", "1"), remove = "2")
  expect_equal(.drawn_medians(p), .med(c(0.5, 1)))
  expect_equal(.drawn_medians(p), c(9.85, 19.25))
  expect_false(anyNA(p$data$dose))
  expect_equal(sort(unique(as.character(p$data$dose))), c("0.5", "1"))
})

test_that("select alone and remove alone are unchanged", {
  p1 <- ggboxplot(ToothGrowth, "dose", "len", select = c("0.5", "1"))
  expect_equal(.drawn_medians(p1), .med(c(0.5, 1)))

  p2 <- ggboxplot(ToothGrowth, "dose", "len", remove = "0.5")
  expect_equal(.drawn_medians(p2), .med(c(1, 2)))

  p3 <- ggboxplot(ToothGrowth, "dose", "len", select = c("0.5", "1", "2"))
  expect_equal(.drawn_medians(p3), .med(c(0.5, 1, 2)))
})

test_that("a no-match select still yields an empty plot rather than an error", {
  # contract asserted by test-show-n.R; the guard must not disturb it
  expect_error(
    p <- ggboxplot(ToothGrowth, "dose", "len", select = "does-not-exist"),
    NA
  )
  expect_equal(nrow(p$data), 0L)
})

test_that("a data column named select/remove/x does not shadow the arguments", {
  base <- data.frame(
    grp = rep(c("a", "b", "c"), each = 4),
    val = c(1:4, 11:14, 21:24),
    stringsAsFactors = FALSE
  )
  keep <- function(p) sort(unique(as.character(p$data$grp)))

  # `subset()` evaluates its expression inside the data frame, so a column of the
  # same name was used in place of the argument and the plot came back empty.
  d1 <- base
  d1$select <- "zzz"
  expect_equal(keep(ggboxplot(d1, "grp", "val", select = c("a", "b"))), c("a", "b"))

  d2 <- base
  d2$remove <- "zzz"
  expect_equal(keep(ggboxplot(d2, "grp", "val", remove = "c")), c("a", "b"))

  d3 <- base
  d3$x <- 99
  expect_equal(keep(ggboxplot(d3, "grp", "val", select = c("a", "b"))), c("a", "b"))
})

test_that("filtering works on a factor x with non-alphabetical levels", {
  d <- data.frame(
    grp = factor(rep(c("a", "b", "c"), each = 4), levels = c("c", "b", "a")),
    val = c(1:4, 11:14, 21:24)
  )
  p <- ggboxplot(d, "grp", "val", select = c("a", "b"))
  expect_equal(sort(unique(as.character(p$data$grp))), c("a", "b"))
})

test_that("the guard reaches builders that take select/remove through dots", {
  # ggscatter does not declare select/remove but forwards ... to .plotter()
  expect_error(
    ggscatter(ToothGrowth, "dose", "len", select = c("0.5", "1"), remove = "0.5"),
    "have values in common"
  )
})
