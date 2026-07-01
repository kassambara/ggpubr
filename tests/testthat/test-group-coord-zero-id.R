# Regression tests for #594: stat_compare_means() failed when the grouped x
# value was 0 (or <= 0), because that value was used as a 1-based group index
# in .group_coord(), yielding coord.values[0] -> NA and a downstream
# ".npc coord ... should be ..." error.

test_that(".group_coord falls back to the first value for a non-positive id (#594)", {
  # group.id = 0 must not produce NA; it recycles to the first coord value.
  expect_equal(ggpubr:::.group_coord("left", 0), "left")
  expect_equal(ggpubr:::.group_coord(c("left", "right"), 0), "left")
  expect_equal(ggpubr:::.group_coord(c(0, 3), 0), 0)
})

test_that(".group_coord is unchanged for valid 1-based ids (no regression, #594)", {
  # Every previously-working id (>= 1) behaves exactly as before.
  expect_equal(ggpubr:::.group_coord(c("a", "b", "c"), 1), "a")
  expect_equal(ggpubr:::.group_coord(c("a", "b", "c"), 2), "b")
  expect_equal(ggpubr:::.group_coord(c("a", "b", "c"), 3), "c")
  # id longer than the vector recycles to the first value (unchanged behavior).
  expect_equal(ggpubr:::.group_coord(c("a", "b"), 3), "a")
  # empty coord values pass through untouched.
  expect_equal(ggpubr:::.group_coord(NULL, 0), NULL)
})

test_that("stat_compare_means() renders when a grouped x value is 0 (#594)", {
  data <- data.frame(
    x = c(0, 0, 0, 0, 3, 3, 3, 3, 0, 0, 0, 0, 3, 3, 3, 3),
    y = c(1.1, 1.2, 1.3, 1.4, 1.5, 1.6, 1.7, 1.8,
          1.1, 1.2, 1.3, 1.4, 1.5, 1.6, 1.7, 1.8),
    group = rep(c("A", "B"), each = 8)
  )
  p <- ggplot2::ggplot(data, ggplot2::aes(x = x, y = y, color = group)) +
    ggplot2::geom_point() +
    stat_compare_means(label = "p.signif")
  # Full render (not just build) so any draw-time error surfaces; must not warn
  # with the "Computation failed" message the bug produced.
  expect_no_warning(
    grob <- ggplot2::ggplot_gtable(ggplot2::ggplot_build(p))
  )
  expect_s3_class(grob, "gtable")
})
