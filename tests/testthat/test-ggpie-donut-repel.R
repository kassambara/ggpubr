context("test-ggpie-donut-repel")

# #655: opt-in `label.repel = TRUE` on ggdonutchart() places the slice labels with
# ggrepel::geom_text_repel() so labels of many small slices no longer overlap. The
# default (label.repel = FALSE) is unchanged / byte-identical. (ggpie() is a solid
# disc where ggrepel in coord_polar cannot reliably clear the face, so the option is
# donut-only.)

suppressPackageStartupMessages(library(ggplot2))

df <- data.frame(
  group = paste0("grp", 1:8),
  value = c(30, 20, 15, 10, 8, 5, 2, 1)
)
df$labs <- paste0(df$group, " (", df$value, "%)")

.has_repel_layer <- function(p) {
  any(vapply(p$layers, function(l) inherits(l$geom, "GeomTextRepel"), logical(1)))
}

test_that("default donut chart has no ggrepel layer (unchanged)", {
  expect_false(.has_repel_layer(ggdonutchart(df, "value", label = "labs", fill = "group")))
})

test_that("label.repel = TRUE adds a ggrepel layer with one label per slice", {
  p <- ggdonutchart(df, "value", label = "labs", fill = "group", label.repel = TRUE)
  expect_true(.has_repel_layer(p))
  b <- suppressWarnings(ggplot2::ggplot_build(p))
  ri <- which(vapply(p$layers, function(l) inherits(l$geom, "GeomTextRepel"), logical(1)))[1]
  expect_equal(nrow(b$data[[ri]]), nrow(df))            # one repelled label per slice
  expect_setequal(as.character(b$data[[ri]]$label), df$labs)
})

test_that("label.repel = TRUE renders without error (draw-time)", {
  expect_error(
    ggplot2::ggplot_gtable(ggplot2::ggplot_build(
      ggdonutchart(df, "value", label = "labs", fill = "group", color = "white",
                   label.repel = TRUE)
    )),
    NA
  )
})

test_that("default label positioning is preserved when label.repel = FALSE", {
  # lab.pos = 'out' (default): labels are axis text -> no text/repel layer
  d_out <- ggdonutchart(df, "value", label = "labs", fill = "group")
  expect_false(.has_repel_layer(d_out))
  expect_false(any(vapply(d_out$layers, function(l) inherits(l$geom, "GeomText"),
                          logical(1))))
  b <- suppressWarnings(ggplot2::ggplot_build(d_out))
  expect_true(any(df$labs %in% b$layout$panel_scales_y[[1]]$get_labels()))
  # lab.pos = 'in' -> a GeomText (not repel) layer
  d_in <- ggdonutchart(df, "value", label = "labs", fill = "group", lab.pos = "in")
  expect_true(any(vapply(d_in$layers, function(l) inherits(l$geom, "GeomText") &&
                           !inherits(l$geom, "GeomTextRepel"), logical(1))))
})
