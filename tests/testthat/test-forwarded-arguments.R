# Regression cover for two documented arguments that were silently dropped:
# ggqqplot(add=) never reached the plotter, and add_summary(shape=) was absent
# from the options passed to stat_summary(). Both defects were invisible because
# nothing exercised the arguments.

n_layers <- function(p) length(ggplot2::ggplot_build(p)$data)

shapes_in <- function(p) {
  b <- ggplot2::ggplot_build(p)
  sort(unique(unlist(lapply(b$data,
    function(d) if ("shape" %in% names(d)) d$shape else NULL))))
}

test_that("ggqqplot(add=) controls whether the reference line is drawn", {
  set.seed(11)
  d <- data.frame(v = stats::rnorm(60))
  with_line <- ggqqplot(d, x = "v", add = "qqline")
  no_line   <- ggqqplot(d, x = "v", add = "none")
  # add = "none" must remove a layer; before the fix both produced 3.
  expect_lt(n_layers(no_line), n_layers(with_line))
  # and the default must behave like an explicit "qqline"
  expect_equal(n_layers(ggqqplot(d, x = "v")), n_layers(with_line))
})

test_that("ggqqplot(add=) still works alongside its other arguments", {
  set.seed(11)
  d <- data.frame(v = stats::rnorm(60), g = rep(c("a", "b"), each = 30))
  expect_no_error(ggplot2::ggplot_build(ggqqplot(d, x = "v", add = "none", conf.int = FALSE)))
  expect_no_error(ggplot2::ggplot_build(ggqqplot(d, x = "v", add = "none", conf.int = TRUE)))
  expect_no_error(ggplot2::ggplot_build(ggqqplot(d, x = "v", add = "none", facet.by = "g")))
  expect_no_error(ggplot2::ggplot_build(ggqqplot(d, x = "v", add = "qqline", color = "g")))
})

test_that("ggqqplot add.params styles the retained reference line", {
  set.seed(11)
  d <- data.frame(v = stats::rnorm(60))
  p <- ggqqplot(
    d, x = "v", conf.int = FALSE,
    add.params = list(color = "red", linetype = "dashed")
  )
  built <- ggplot2::ggplot_build(p)
  line <- built$data[[2]]
  expect_identical(
    list(layers = length(built$data), colour = unique(line$colour), linetype = unique(line$linetype)),
    list(layers = 2L, colour = "red", linetype = "dashed")
  )
})

test_that("add_summary(shape=) reaches the point-range layer", {
  df <- ToothGrowth
  df$dose <- factor(df$dose)
  base <- function() ggboxplot(df, x = "dose", y = "len")
  s19 <- shapes_in(add_summary(base(), fun = "mean_se", shape = 19))
  s17 <- shapes_in(add_summary(base(), fun = "mean_se", shape = 17))
  # Before the fix both returned 19: the argument was dropped entirely.
  expect_false(identical(s19, s17))
  expect_true(17 %in% s17)
})

test_that("add_summary(shape=) is not forwarded to geoms that draw no point", {
  df <- ToothGrowth
  df$dose <- factor(df$dose)
  for (ep in c("errorbar", "linerange", "crossbar")) {
    conditions <- character(0)
    withCallingHandlers(
      invisible(ggplot2::ggplot_build(
        add_summary(ggboxplot(df, x = "dose", y = "len"),
                    fun = "mean_se", error.plot = ep, shape = 17)
      )),
      warning = function(w) {
        conditions <<- c(conditions, conditionMessage(w))
        invokeRestart("muffleWarning")
      }
    )
    # a shape forwarded to a line-only geom would produce
    # "Ignoring unknown parameters" from ggplot2
    expect_length(grep("unknown parameter", conditions, ignore.case = TRUE), 0)
  }
})

test_that("add_summary still honours the arguments it always forwarded", {
  df <- ToothGrowth
  df$dose <- factor(df$dose)
  cols <- function(p) unique(unlist(lapply(ggplot2::ggplot_build(p)$data,
    function(d) if ("colour" %in% names(d)) d$colour else NULL)))
  expect_true("red" %in% cols(
    add_summary(ggboxplot(df, x = "dose", y = "len"), fun = "mean_se", color = "red")))
})

test_that("add_summary(group = 1) keeps one summary per x", {
  df <- ToothGrowth
  df$dose <- factor(df$dose)
  base <- ggboxplot(df, x = "dose", y = "len", color = "supp")

  overall <- ggplot2::ggplot_build(
    add_summary(base, fun = "mean_se")
  )$data[[2]]
  by_supp <- ggplot2::ggplot_build(
    add_summary(base, fun = "mean_se", group = "supp", color = "supp")
  )$data[[2]]

  # A constant group must override the inherited color grouping before the
  # statistic runs. A named group column must continue to produce groupwise
  # summaries, so the two public contracts are pinned together.
  expect_identical(nrow(overall), 3L)
  expect_equal(as.numeric(overall$x), 1:3)
  expect_equal(overall$y, as.numeric(tapply(df$len, df$dose, mean)))
  expect_identical(nrow(by_supp), 6L)
})
