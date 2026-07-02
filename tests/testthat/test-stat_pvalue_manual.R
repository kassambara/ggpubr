context("test-stat_pvalue_manual")
data("ToothGrowth")
df <- ToothGrowth

test_with_fill <- function(inherit.aes = NULL) {
  stat.test <- df %>%
    group_by(dose) %>%
    rstatix::t_test(len ~ supp) %>%
    rstatix::add_xy_position(x = "dose")

  if (is.null(inherit.aes)) {
    layer <- stat_pvalue_manual(stat.test)
  } else {
    layer <- stat_pvalue_manual(stat.test, inherit.aes = inherit.aes)
  }

  p <- ggplot(df, aes(x = dose, y = len, color = supp, fill = supp)) +
    geom_boxplot() +
    layer
  ggplot2::ggplot_build(p)
}

test_that("fill aes works well by default", {
  expect_error(test_with_fill(inherit.aes = TRUE))
  expect_no_error(test_with_fill(inherit.aes = FALSE))
  expect_no_error(test_with_fill())
})

# tip.length.ref: "data" (default) vs "axis" (#362) -----------------------

# Helper: absolute tip depth (in y data units) of the bracket in a built plot
.bracket_tip_depth <- function(p) {
  b <- ggplot2::ggplot_build(p)
  for (d in b$data) {
    if (all(c("y", "yend") %in% names(d)) && "annotation" %in% names(d)) {
      return(max(c(d$y, d$yend)) - min(c(d$y, d$yend)))
    }
  }
  NA_real_
}

.tip_plot <- function(values, ylim, y.position, ref = "data") {
  d <- data.frame(
    treatment = c("a", "a", "a", "b", "b", "b"),
    value = values
  )
  stat <- data.frame(group1 = "a", group2 = "b", p = 0.01, y.position = y.position)
  spm <- if (is.null(ref)) {
    stat_pvalue_manual(stat, label = "p", tip.length = 0.2)  # rely on default arg
  } else {
    stat_pvalue_manual(stat, label = "p", tip.length = 0.2, tip.length.ref = ref)
  }
  ggbarplot(d, "treatment", "value") + spm + ggplot2::ylim(ylim)
}

test_that("tip.length.ref default 'data' is unchanged (tips scale with data range)", {
  small <- .tip_plot(c(1, 2, 4, 3, 5, 6),        c(0, 10),  9,  "data")
  big   <- .tip_plot(c(10, 20, 40, 30, 50, 60),  c(0, 100), 90, "data")
  # Historical behavior: tip = tip.length * data range. big's inputs are exactly
  # 10x small's, so its data-mode tip is exactly 10x as deep, and it is NOT a
  # fixed fraction of the axis (that is what 'axis' mode adds).
  tip_small <- .bracket_tip_depth(small)
  tip_big   <- .bracket_tip_depth(big)
  expect_equal(tip_big / tip_small, 10, tolerance = 1e-6)
  expect_false(isTRUE(all.equal(tip_small, 0.2 * 10)))
})

test_that("tip.length.ref = 'data' (default) is byte-identical to omitting the arg", {
  omitted  <- .tip_plot(c(1, 2, 4, 3, 5, 6), c(0, 10), 9, ref = NULL)
  explicit <- .tip_plot(c(1, 2, 4, 3, 5, 6), c(0, 10), 9, ref = "data")
  expect_equal(.bracket_tip_depth(omitted), .bracket_tip_depth(explicit),
               tolerance = 1e-9)
})

test_that("tip.length.ref = 'axis' gives constant fraction of the y-axis range (#362)", {
  small <- .tip_plot(c(1, 2, 4, 3, 5, 6),        c(0, 10),  9,  "axis")
  big   <- .tip_plot(c(10, 20, 40, 30, 50, 60),  c(0, 100), 90, "axis")
  # tip = 0.2 * axis range: 0.2*10 = 2 and 0.2*100 = 20 -> both 20% of their axis
  expect_equal(.bracket_tip_depth(small), 0.2 * 10, tolerance = 1e-6)
  expect_equal(.bracket_tip_depth(big),   0.2 * 100, tolerance = 1e-6)
})

test_that("geom_bracket honors tip.length.ref and validates it", {
  d <- data.frame(
    treatment = c("a", "a", "a", "b", "b", "b"),
    value = c(1, 2, 4, 3, 5, 6)
  )
  p_data <- ggbarplot(d, "treatment", "value") +
    geom_bracket(xmin = "a", xmax = "b", label = "x", y.position = 9,
                 tip.length = 0.2, tip.length.ref = "data") +
    ggplot2::ylim(0, 10)
  p_axis <- ggbarplot(d, "treatment", "value") +
    geom_bracket(xmin = "a", xmax = "b", label = "x", y.position = 9,
                 tip.length = 0.2, tip.length.ref = "axis") +
    ggplot2::ylim(0, 10)
  # here data range (<10) < axis range (10), so axis tips are deeper than data tips
  expect_gt(.bracket_tip_depth(p_axis), .bracket_tip_depth(p_data))
  expect_equal(.bracket_tip_depth(p_axis), 0.2 * 10, tolerance = 1e-6)
  expect_error(
    geom_bracket(xmin = "a", xmax = "b", label = "x", y.position = 9,
                 tip.length.ref = "bogus")
  )
})
