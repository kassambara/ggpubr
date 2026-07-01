# Regression tests for #535: table_cell_font()/table_cell_bg() can now style an
# individual header cell (row 1, "colhead-*"), not only body cells ("core-*").

.df <- data.frame(Group = c("a", "b", "c"), Value = c(1, 2, 3))

.cell_gp <- function(tab, row, column, names) {
  tg <- ggpubr:::get_tablegrob(tab)
  l <- tg$layout
  i <- which(l$t == row & l$l == column & l$name %in% names)
  tg$grobs[[i]]$gp
}

test_that("table_cell_font() styles a single header cell (#535)", {
  tab <- ggtexttable(.df, rows = NULL)
  tab <- table_cell_font(tab, row = 1, column = 2, face = "italic", color = "red")
  gp <- .cell_gp(tab, 1, 2, c("core-fg", "colhead-fg"))
  # grid normalises fontface = "italic" into font = c(italic = 3L)
  expect_equal(names(gp$font), "italic")
  expect_equal(gp$col, "red")
  # the other header cell is untouched
  gp1 <- .cell_gp(tab, 1, 1, c("core-fg", "colhead-fg"))
  expect_false(identical(gp1$col, "red"))
})

test_that("table_cell_bg() styles a single header cell background (#535)", {
  tab <- ggtexttable(.df, rows = NULL)
  tab <- table_cell_bg(tab, row = 1, column = 2, fill = "yellow")
  gp <- .cell_gp(tab, 1, 2, c("core-bg", "colhead-bg"))
  expect_equal(gp$fill, "yellow")
})

test_that("body-cell styling is unchanged (no regression, #535)", {
  tab <- ggtexttable(.df, rows = NULL)
  tab <- table_cell_font(tab, row = 2, column = 1, face = "bold", color = "blue")
  gp <- .cell_gp(tab, 2, 1, "core-fg")
  expect_equal(names(gp$font), "bold")
  expect_equal(gp$col, "blue")
  expect_no_error(print(tab))
})
