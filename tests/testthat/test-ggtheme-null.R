context("test-ggtheme-null")

# #561: `ggtheme = NULL` should skip applying a ggpubr theme, so the plot keeps
# ggplot2's default theme (or the theme set globally with theme_set()). Passing
# nothing or a real theme must stay byte-identical (no-regression).

# helper: is a *complete* theme attached to the plot?
.has_complete_theme <- function(p) isTRUE(attr(p$theme, "complete"))

test_that("default (ggtheme unset) still applies theme_pubr (#561 no-regression)", {
  p <- ggscatter(mtcars, "wt", "mpg")
  expect_true(.has_complete_theme(p))
  # theme_pubr panel background is white
  expect_equal(ggplot2::calc_element("panel.background", p$theme)$fill, "white")
})

test_that("explicit ggtheme = NULL skips theming across wrappers (#561)", {
  wrappers <- list(
    ggscatter(mtcars, "wt", "mpg", ggtheme = NULL),
    ggboxplot(ToothGrowth, "dose", "len", ggtheme = NULL),
    ggline(ToothGrowth, "dose", "len", ggtheme = NULL),
    gghistogram(ToothGrowth, "len", ggtheme = NULL),
    ggbarplot(ToothGrowth[1:6, ], "supp", "len", ggtheme = NULL)
  )
  for (p in wrappers) {
    # no complete ggpubr theme attached -> ggplot2 default governs at draw time
    expect_false(.has_complete_theme(p))
    # and it still renders without error
    expect_error(ggplot2::ggplot_gtable(ggplot2::ggplot_build(p)), NA)
  }
})

test_that("explicit ggtheme = theme_bw() is honored and unchanged (#561 no-regression)", {
  p <- ggscatter(mtcars, "wt", "mpg", ggtheme = ggplot2::theme_bw())
  expect_true(.has_complete_theme(p))
  # theme_bw has a black axis-less panel border and grey grid; distinguishable
  # from theme_pubr by the panel.grid.major being drawn (grey), not blank
  expect_false(inherits(ggplot2::calc_element("panel.grid.major", p$theme), "element_blank"))
})

test_that("faceted default still gets the bordered theme_pubr (#561 no-regression)", {
  p <- ggboxplot(ToothGrowth, "dose", "len", facet.by = "supp")
  expect_true(.has_complete_theme(p))
  expect_equal(ggplot2::calc_element("panel.background", p$theme)$fill, "white")
})

test_that("ggtheme = NULL lets a session-global theme govern (#561)", {
  old <- ggplot2::theme_set(ggplot2::theme_minimal())
  on.exit(ggplot2::theme_set(old), add = TRUE)
  # NULL means 'no ggpubr theme' -> the global theme (theme_minimal) is used at
  # draw time; the plot itself carries no competing complete theme.
  p <- ggscatter(mtcars, "wt", "mpg", ggtheme = NULL)
  expect_false(.has_complete_theme(p))
  expect_error(ggplot2::ggplot_gtable(ggplot2::ggplot_build(p)), NA)
})
