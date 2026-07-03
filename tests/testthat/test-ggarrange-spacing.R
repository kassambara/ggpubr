context("test-ggarrange-spacing")

.p1 <- ggscatter(mtcars, "wt", "mpg")
.p2 <- ggboxplot(ToothGrowth, "dose", "len")

test_that("spacing = 0 (default) leaves the plots untouched (#151 no-regression)", {
  plots <- list(.p1, .p2)
  expect_identical(ggpubr:::.add_plot_spacing(plots, 0), plots)
  expect_identical(ggpubr:::.add_plot_spacing(plots, spacing = 0), plots)
  # NULL spacing also a no-op
  expect_identical(ggpubr:::.add_plot_spacing(plots, NULL), plots)
})

test_that("spacing > 0 adds a uniform plot.margin (in lines) to each ggplot (#151)", {
  out <- ggpubr:::.add_plot_spacing(list(.p1, .p2), spacing = 1)
  m1 <- out[[1]]$theme$plot.margin
  expect_true(inherits(m1, "unit"))
  expect_equal(as.numeric(m1), rep(1, 4))
  expect_true(all(grid::unitType(m1) == "lines"))
  # both plots get it
  expect_equal(as.numeric(out[[2]]$theme$plot.margin), rep(1, 4))
})

test_that("spacing passes non-ggplot entries (NULL spacers) through unchanged (#151)", {
  plots <- list(.p1, NULL, .p2)
  out <- ggpubr:::.add_plot_spacing(plots, spacing = 2)
  expect_null(out[[2]])
  expect_equal(as.numeric(out[[1]]$theme$plot.margin), rep(2, 4))
})

test_that("ggarrange gains a spacing argument that renders (#151)", {
  expect_error(ggarrange(.p1, .p2, ncol = 2, spacing = 2), NA)
  # default call still works and returns a ggarrange object
  g <- ggarrange(.p1, .p2, ncol = 2)
  expect_s3_class(g, "ggarrange")
  g2 <- ggarrange(.p1, .p2, ncol = 2, spacing = 1.5)
  expect_s3_class(g2, "ggarrange")
})

test_that("spacing rejects non-numeric / NA input cleanly (#151)", {
  expect_error(ggpubr:::.add_plot_spacing(list(.p1), spacing = "a"), "numeric")
  expect_error(ggpubr:::.add_plot_spacing(list(.p1), spacing = NA), "numeric|non-missing")
  expect_error(ggarrange(.p1, .p2, spacing = "big"), "numeric")
})
