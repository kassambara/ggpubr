context("test-annotate-figure-titles")

# annotate_figure() gains column.titles / row.titles to add a title above each
# column and to the left of each row of an arranged grid (#573). Opt-in: default
# NULL leaves the figure unchanged.

.fig <- ggarrange(
  ggboxplot(ToothGrowth, "dose", "len"),
  ggboxplot(ToothGrowth, "dose", "len"),
  ggboxplot(ToothGrowth, "dose", "len"),
  ggboxplot(ToothGrowth, "dose", "len"),
  ncol = 2, nrow = 2
)

# --- .titles_grob helper ----------------------------------------------------

test_that(".titles_grob builds one cell per title with the right labels", {
  g <- ggpubr:::.titles_grob(c("A", "B", "C"), rot = 0)
  expect_true(grid::is.grob(g))
  labels <- vapply(g$grobs, function(x) x$label, character(1))
  expect_equal(labels, c("A", "B", "C"))
})

test_that(".titles_grob rotates row titles by 90 degrees", {
  g <- ggpubr:::.titles_grob(c("R1", "R2"), rot = 90)
  rots <- vapply(g$grobs, function(x) x$rot, numeric(1))
  expect_equal(rots, c(90, 90))
})

test_that(".titles_grob accepts a list of grobs (custom styling passthrough)", {
  custom <- list(
    text_grob("X", color = "red", size = 14),
    text_grob("Y", color = "blue", size = 14)
  )
  g <- ggpubr:::.titles_grob(custom, rot = 0)
  labels <- vapply(g$grobs, function(x) x$label, character(1))
  expect_equal(labels, c("X", "Y"))
  expect_equal(g$grobs[[1]]$gp$col, "red")
})

test_that(".titles_grob rejects invalid input", {
  expect_error(ggpubr:::.titles_grob(1:3), "character vector or a list of grobs")
})

# --- annotate_figure() integration ------------------------------------------

test_that("column.titles / row.titles render without error", {
  expect_no_error(
    ggplot2::ggplot_gtable(ggplot2::ggplot_build(
      annotate_figure(.fig,
        column.titles = c("Col 1", "Col 2"),
        row.titles = c("Row 1", "Row 2")
      )
    ))
  )
  # combined with a figure-wide top label
  expect_no_error(
    ggplot2::ggplot_gtable(ggplot2::ggplot_build(
      annotate_figure(.fig,
        top = text_grob("Whole figure", face = "bold"),
        column.titles = c("Col 1", "Col 2")
      )
    ))
  )
})

test_that("column.titles accepts a list of grobs", {
  expect_no_error(
    ggplot2::ggplot_gtable(ggplot2::ggplot_build(
      annotate_figure(.fig,
        column.titles = list(
          text_grob("A", color = "red"),
          text_grob("B", color = "blue")
        )
      )
    ))
  )
})

test_that("default (no titles) is unchanged: passing NULL equals omitting", {
  omitted <- annotate_figure(.fig, top = text_grob("T"))
  explicit_null <- annotate_figure(.fig, top = text_grob("T"),
    column.titles = NULL, row.titles = NULL)
  # same object structure (the titles block is skipped when both are NULL)
  expect_equal(
    ggplot2::ggplot_build(omitted)$data,
    ggplot2::ggplot_build(explicit_null)$data
  )
})

test_that("positional fig.lab still binds correctly (no signature-order break)", {
  # column.titles / row.titles are appended at the END of the signature, so the
  # 6th positional argument is still fig.lab (not column.titles). A positional
  # fig.lab call must still draw the figure label.
  lab_layer <- function(p) {
    i <- utils::tail(which(vapply(p$layers,
      function(l) inherits(l$geom, "GeomText"), logical(1))), 1)
    p$layers[[i]]
  }
  p <- annotate_figure(.fig, NULL, NULL, NULL, NULL, "Figure 1")
  expect_equal(as.character(lab_layer(p)$data$text), "Figure 1")
})

test_that("empty title vectors are handled gracefully (no crash)", {
  expect_no_error(
    ggplot2::ggplot_gtable(ggplot2::ggplot_build(
      annotate_figure(.fig, column.titles = character(0), row.titles = character(0))
    ))
  )
  # an empty vector behaves like NULL (no titles added)
  base <- ggplot2::ggplot_build(annotate_figure(.fig))$data
  empty <- ggplot2::ggplot_build(
    annotate_figure(.fig, column.titles = character(0)))$data
  expect_equal(base, empty)
})
