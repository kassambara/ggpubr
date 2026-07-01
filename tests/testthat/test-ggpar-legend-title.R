test_that("ggpar legend.title applies to size and alpha legends (#412)", {
  lab <- function(p, a) ggplot2::ggplot_build(p)$plot$labels[[a]]

  # Regression: legend.title now titles a size legend and an alpha legend
  ps <- ggpar(ggscatter(mtcars, "wt", "mpg", size = "qsec"),
              legend.title = "Size title")
  expect_equal(lab(ps, "size"), "Size title")
  pa <- ggpar(ggscatter(mtcars, "wt", "mpg", alpha = "qsec"),
              legend.title = "Alpha title")
  expect_equal(lab(pa, "alpha"), "Alpha title")

  # No-regression: color aesthetic path unchanged
  pc <- ggpar(ggscatter(mtcars, "wt", "mpg", color = "cyl"),
              legend.title = "Color title")
  expect_equal(lab(pc, "colour"), "Color title")

  # No-regression: without legend.title the size label is untouched
  pd <- ggscatter(mtcars, "wt", "mpg", size = "qsec")
  expect_equal(lab(pd, "size"), "qsec")
})
