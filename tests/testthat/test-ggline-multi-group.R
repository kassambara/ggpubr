# Regression tests for #616 / #375: ggline() errored ("condition has length > 1")
# when two different variables defined the grouping (e.g. color + linetype, or an
# explicit group + color), because the internal `group` became a length-2 vector.

.n_line_groups <- function(p) {
  b <- ggplot2::ggplot_build(p)
  # the line layer carries both a group column and >2 rows per line
  idx <- which(vapply(b$data, function(d)
    all(c("group", "linetype") %in% names(d)) && nrow(d) > 2, logical(1)))
  length(unique(b$data[[idx[1]]]$group))
}

test_that("ggline() combines color + linetype into one grouping (#616)", {
  set.seed(1)
  data <- data.frame(
    x = rep(seq(0, 1, length.out = 10), 4),
    type  = rep(c("poly", "poly", "inv", "inv"), each = 10),
    order = rep(c("low", "high", "low", "high"), each = 10)
  )
  data$val <- data$x + as.integer(factor(paste(data$type, data$order)))
  p <- ggline(data, x = "x", y = "val", linetype = "type", color = "order",
              numeric.x.axis = TRUE)
  expect_no_error(ggplot2::ggplot_gtable(ggplot2::ggplot_build(p)))
  # 2 types x 2 orders = 4 lines, matching geom_line(aes(linetype=, color=))
  expect_equal(.n_line_groups(p), 4L)
})

test_that("ggline() supports explicit group + color (#375)", {
  df2 <- data.frame(
    supp = rep(c("VC", "OJ", "VC"), each = 3),
    dose = rep(c("D0.5", "D1", "D2"), 3),
    len  = c(6.8, 15, 33, 4.2, 10, 29.5, 12, 6, 14.2),
    num  = as.character(rep(1:3, each = 3))
  )
  p <- ggline(df2, "dose", "len", group = "num", color = "supp")
  expect_no_error(ggplot2::ggplot_gtable(ggplot2::ggplot_build(p)))
  # 3 num groups (matching geom_line(aes(group=num, color=supp)))
  expect_equal(.n_line_groups(p), 3L)
})

test_that("ggline() explicit group takes precedence over color (#375)", {
  # supp appears with BOTH num values (full factorial): an explicit group must
  # group by `num` ALONE (matching geom_line(aes(group=num, color=supp))), not
  # by interaction(supp, num).
  dfe <- data.frame(
    dose = rep(c("D0.5", "D1", "D2"), 4),
    num  = as.character(rep(c(1, 1, 2, 2), each = 3)),
    supp = rep(c("VC", "OJ"), times = 6),
    len  = 1:12
  )
  p <- ggline(dfe, "dose", "len", group = "num", color = "supp")
  expect_no_error(ggplot2::ggplot_gtable(ggplot2::ggplot_build(p)))
  # 2 num groups, NOT 4 interaction groups
  expect_equal(.n_line_groups(p), 2L)
  # matches geom_line's explicit-group grouping
  gl <- ggplot2::ggplot_build(
    ggplot2::ggplot(dfe, ggplot2::aes(dose, len, group = num, color = supp)) +
      ggplot2::geom_line()
  )
  expect_equal(.n_line_groups(p), length(unique(gl$data[[1]]$group)))
})

test_that("ggline() single grouping variable is unchanged (no regression, #616)", {
  p <- ggline(ToothGrowth, x = "dose", y = "len", color = "supp", add = "mean_se")
  b <- ggplot2::ggplot_build(p)
  # the new .ggpubr.group column must NOT be introduced in the single-var path
  expect_false(any(vapply(b$data,
    function(d) ".ggpubr.group" %in% names(d), logical(1))))
  expect_no_error(ggplot2::ggplot_gtable(b))
  expect_equal(.n_line_groups(p), 2L)
})
