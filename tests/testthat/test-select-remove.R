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

test_that("select and remove naming the same item warns", {
  expect_warning(
    ggboxplot(ToothGrowth, "dose", "len", select = c("0.5", "1"), remove = "0.5"),
    "have values in common"
  )
  # the offending value is named, so the user can see which one
  expect_warning(
    ggboxplot(ToothGrowth, "dose", "len", select = c("0.5", "1"), remove = "0.5"),
    "0.5"
  )
  # more than one overlapping value is listed
  expect_warning(
    ggboxplot(ToothGrowth, "dose", "len",
      select = c("0.5", "1", "2"), remove = c("0.5", "2")
    ),
    "0\\.5, 2"
  )
})

test_that("an overlapping item is dropped, and the remaining groups are right", {
  # `remove` is applied after `select`, so the overlap loses; the point of
  # warning rather than erroring is that the result is still correct.
  p <- suppressWarnings(
    ggboxplot(ToothGrowth, "dose", "len", select = c("0.5", "1"), remove = "0.5")
  )
  expect_equal(sort(unique(as.character(p$data$dose))), "1")
  expect_equal(.drawn_medians(p), .med(1))
  expect_equal(.drawn_medians(p), 19.25)
  expect_false(anyNA(p$data$dose))

  # `select` naming every group drops nothing, so only `remove` bites. This call
  # drew correctly before the change and must still draw correctly.
  p2 <- suppressWarnings(
    ggboxplot(ToothGrowth, "dose", "len", select = c("0.5", "1", "2"), remove = "2")
  )
  expect_equal(sort(unique(as.character(p2$data$dose))), c("0.5", "1"))
  expect_equal(.drawn_medians(p2), .med(c(0.5, 1)))
})

test_that("the warning fires only when BOTH are supplied and they overlap", {
  # none of these is contradictory, so none should warn -- assert the ABSENCE of
  # the condition rather than matching message text, which is translated.
  seen <- function(expr) {
    w <- character(0)
    withCallingHandlers(force(expr),
      warning = function(cnd) {
        w <<- c(w, conditionMessage(cnd))
        invokeRestart("muffleWarning")
      }
    )
    sum(grepl("values in common", w))
  }
  expect_equal(seen(ggboxplot(ToothGrowth, "dose", "len", select = c("0.5", "1"))), 0L)
  expect_equal(seen(ggboxplot(ToothGrowth, "dose", "len", remove = "0.5")), 0L)
  expect_equal(
    seen(ggboxplot(ToothGrowth, "dose", "len", select = c("0.5", "1"), remove = "2")),
    0L
  )
  # and none of them errors
  expect_error(ggboxplot(ToothGrowth, "dose", "len", select = c("0.5", "1")), NA)
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

test_that("a computed summary is right when select and remove are combined", {
  # ggbarplot(add = "mean_se") derives the bar height and the interval, so a
  # misaligned row filter shows up as a wrong NUMBER rather than a wrong box.
  sub <- ToothGrowth[ToothGrowth$dose %in% c(0.5, 1), ]
  want_mean <- as.numeric(tapply(sub$len, factor(sub$dose), mean))
  want_se <- as.numeric(tapply(sub$len, factor(sub$dose), function(v) {
    stats::sd(v) / sqrt(length(v))
  }))

  p <- ggbarplot(ToothGrowth, "dose", "len",
    add = "mean_se", select = c("0.5", "1"), remove = "2"
  )
  b <- suppressWarnings(ggplot2::ggplot_build(p))
  expect_equal(b$data[[1]]$y, want_mean)
  expect_equal(b$data[[1]]$y, c(10.605, 19.735))

  eb <- b$data[[2]]
  expect_equal(eb$ymax - want_mean, want_se)

  # and the same through ggerrorplot, which draws the summary directly
  p2 <- ggerrorplot(ToothGrowth, "dose", "len", select = c("0.5", "1"), remove = "2")
  b2 <- suppressWarnings(ggplot2::ggplot_build(p2))
  expect_equal(sort(unique(round(b2$data[[1]]$y, 6))), sort(round(want_mean, 6)))
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

  # `subset()` evaluated its expression inside the data frame, so a column of the
  # same name was used in place of the argument. What got filtered then depended
  # on that column's contents, so each of these failed differently: empty plot,
  # nothing dropped, or -- the last one -- the WRONG group dropped.
  d1 <- base
  d1$select <- "zzz" # disjoint contents: drew nothing
  expect_equal(keep(ggboxplot(d1, "grp", "val", select = c("a", "b"))), c("a", "b"))

  d2 <- base
  d2$select <- d2$grp # overlapping contents: dropped nothing
  expect_equal(keep(ggboxplot(d2, "grp", "val", select = c("a", "b"))), c("a", "b"))

  d3 <- base
  d3$remove <- "zzz"
  expect_equal(keep(ggboxplot(d3, "grp", "val", remove = "c")), c("a", "b"))

  d4 <- base
  d4$remove <- d4$grp # drew nothing
  expect_equal(keep(ggboxplot(d4, "grp", "val", remove = "c")), c("a", "b"))

  d5 <- base
  d5$remove <- "a" # dropped "a" and kept "c" -- the wrong group
  expect_equal(keep(ggboxplot(d5, "grp", "val", remove = "c")), c("a", "b"))

  d6 <- base
  d6$x <- 99
  expect_equal(keep(ggboxplot(d6, "grp", "val", select = c("a", "b"))), c("a", "b"))
})

test_that("as.vector() on the filtered column is kept, so a Date x is unchanged", {
  # NO-REGRESSION PIN, not a statement of desirable behaviour. The filter applies
  # as.vector() to the x column before matching, which for a Date strips it to the
  # day number: a date string matches nothing and the day number matches. That is
  # a separate pre-existing defect. It is pinned here because dropping as.vector()
  # silently changes released output for single-argument calls, and without this
  # test the whole suite stays green when it is removed.
  d <- data.frame(
    grp = rep(as.Date(c("2020-01-01", "2020-01-02", "2020-01-03")), each = 4),
    val = c(1:4, 11:14, 21:24)
  )
  n <- function(p) nrow(p$data)

  expect_equal(n(ggboxplot(d, "grp", "val", select = c("2020-01-01", "2020-01-02"))), 0L)
  expect_equal(n(ggboxplot(d, "grp", "val", remove = "2020-01-03")), 12L)
  expect_equal(n(ggboxplot(d, "grp", "val", select = as.numeric(as.Date("2020-01-02")))), 4L)
})

test_that("filtering works on a factor x with non-alphabetical levels", {
  d <- data.frame(
    grp = factor(rep(c("a", "b", "c"), each = 4), levels = c("c", "b", "a")),
    val = c(1:4, 11:14, 21:24)
  )
  p <- ggboxplot(d, "grp", "val", select = c("a", "b"))
  expect_equal(sort(unique(as.character(p$data$grp))), c("a", "b"))
})

test_that("the warning reaches builders that take select/remove through dots", {
  # ggscatter does not declare select/remove but forwards ... to .plotter()
  expect_warning(
    ggscatter(ToothGrowth, "dose", "len", select = c("0.5", "1"), remove = "0.5"),
    "have values in common"
  )
})
