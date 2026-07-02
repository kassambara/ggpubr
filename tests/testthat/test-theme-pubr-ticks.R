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

# Follow-up to #668: with the ggplot2 (>= 3.5.0) default (strip.clip = "on"),
# the facet strip background border is clipped to the strip area, so the outer
# half of the border stroke is cut and the border renders thinner than the
# requested linewidth -- misaligned with the panel/axis when zoomed. theme_pubr()
# now sets strip.clip = "off" so the strip border renders at its full width.
test_that("theme_pubr() sets strip.clip = 'off' so the strip border isn't clipped (#668)", {
  expect_equal(theme_pubr()$strip.clip, "off")
  expect_equal(theme_pubr(border = TRUE)$strip.clip, "off")
})

test_that("theme_pubr() faceted plot renders with a full-width strip border (#668)", {
  df <- data.frame(x = c(1, 2, 3, 1, 2, 3), y = c(2, 3, 4, 1, 5, 3),
                   g = rep(c("G1", "G2"), each = 3))
  p <- ggplot2::ggplot(df, ggplot2::aes(x, y)) +
    ggplot2::geom_point() + ggplot2::facet_wrap(~g) + theme_pubr()
  expect_no_error(ggplot2::ggplot_gtable(ggplot2::ggplot_build(p)))
  # strip.background linewidth preserved (border consistency, not thinned)
  strip <- ggplot2::calc_element("strip.background", theme_pubr())
  expect_equal(strip$linewidth, 0.7)
  expect_equal(strip$colour, "black")
})
