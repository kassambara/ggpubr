# Regression test for #668: theme_pubr() left axis.ticks inheriting a lighter
# grey/thinner style while axis.line was black/0.5, visibly inconsistent when
# zoomed. Ticks now match the axis lines.

test_that("theme_pubr() axis.ticks match axis.line (black, 0.5) (#668)", {
  th <- theme_pubr()
  ticks <- ggplot2::calc_element("axis.ticks", th)
  line <- ggplot2::calc_element("axis.line", th)
  expect_equal(ticks$colour, "black")
  expect_equal(ticks$linewidth, 0.5)
  # consistent with the axis line
  expect_equal(ticks$colour, line$colour)
  expect_equal(ticks$linewidth, line$linewidth)
})

test_that("theme_pubr(border = TRUE) also has black/0.5 ticks (#668)", {
  ticks <- ggplot2::calc_element("axis.ticks", theme_pubr(border = TRUE))
  expect_equal(ticks$colour, "black")
  expect_equal(ticks$linewidth, 0.5)
})

test_that("theme_pubr() still renders and other elements are intact (#668)", {
  p <- ggplot2::ggplot(data.frame(x = 1:3, y = c("A", "B", "C")),
                       ggplot2::aes(x, y)) +
    ggplot2::geom_point() + theme_pubr()
  expect_no_error(ggplot2::ggplot_gtable(ggplot2::ggplot_build(p)))
  th <- theme_pubr()
  # unrelated elements unchanged
  expect_true(inherits(th$panel.grid.major, "element_blank"))
  expect_equal(ggplot2::calc_element("axis.text", th)$colour, "black")
})
