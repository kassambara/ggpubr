# Regression tests for #185: annotate_figure(fig.lab=) positioned the label with
# cowplot's width-proportional hjust (-0.1 / 1.1), so a longer label was pushed
# further from the corner and figure captions no longer aligned across figures.
# The label now uses a length-independent hjust (0 / 1) and a fixed x anchor.

.fig <- ggarrange(ggboxplot(ToothGrowth, "dose", "len"), ncol = 1)

.lab_layer <- function(p) {
  # the figure label is the last (GeomText) layer added by draw_plot_label
  i <- utils::tail(which(vapply(p$layers,
    function(l) inherits(l$geom, "GeomText"), logical(1))), 1)
  p$layers[[i]]
}

test_that("fig.lab hjust is length-independent, not cowplot's -0.1 (#185)", {
  short <- .lab_layer(annotate_figure(.fig, fig.lab = "A", fig.lab.pos = "top.left"))
  long  <- .lab_layer(annotate_figure(.fig, fig.lab = "A very long figure label", fig.lab.pos = "top.left"))
  # top.left -> hjust 0 (was -0.1)
  expect_equal(short$aes_params$hjust, 0)
  # hjust and x anchor identical regardless of label length -> captions align
  expect_equal(short$aes_params$hjust, long$aes_params$hjust)
  expect_equal(short$data$x, long$data$x)
})

test_that("fig.lab positions map to fixed hjust 0/0.5?/1 (#185)", {
  tl <- .lab_layer(annotate_figure(.fig, fig.lab = "L", fig.lab.pos = "top.left"))
  tr <- .lab_layer(annotate_figure(.fig, fig.lab = "R", fig.lab.pos = "top.right"))
  expect_equal(tl$aes_params$hjust, 0)
  expect_equal(tr$aes_params$hjust, 1)
})

test_that("annotate_figure(fig.lab=) still renders (#185)", {
  expect_no_error(print(annotate_figure(.fig, fig.lab = "Fig 1", fig.lab.face = "bold")))
  expect_no_error(print(annotate_figure(.fig, fig.lab = "Fig 1", fig.lab.pos = "bottom.right")))
})

test_that("default fig.lab size/face are unchanged (no regression, #185)", {
  d <- .lab_layer(annotate_figure(.fig, fig.lab = "Fig 1"))
  # cowplot::draw_figure_label defaulted to the theme text size/face (11pt/plain),
  # NOT draw_plot_label's 16/bold -> preserve that default.
  expect_equal(d$aes_params$size, ggplot2::theme_get()$text$size / ggplot2::.pt)
  expect_equal(d$aes_params$fontface, ggplot2::theme_get()$text$face)
  # explicit size/face still honored
  e <- .lab_layer(annotate_figure(.fig, fig.lab = "X", fig.lab.size = 20, fig.lab.face = "italic"))
  expect_equal(e$aes_params$fontface, "italic")
})
