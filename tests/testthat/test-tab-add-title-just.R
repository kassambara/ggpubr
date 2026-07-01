# Regression tests for #302: tab_add_title()/tab_add_footnote() ignored `just`
# (the anchor x was hardcoded, so "center"/"right" only aligned the text around a
# fixed point) and clipped a title/footnote wider than the table.

.df <- tibble::tibble(X1 = c(1, 2), Y1 = c("a", "b"))

.grob_x <- function(tab, pattern) {
  tg <- ggpubr:::get_tablegrob(tab)
  i <- which(vapply(tg$grobs, function(g)
    inherits(g, "text") && grepl(pattern, g$label %||% ""), logical(1)))[1]
  as.numeric(tg$grobs[[i]]$x)
}
.grob_clip <- function(tab, pattern) {
  tg <- ggpubr:::get_tablegrob(tab)
  i <- which(vapply(tg$grobs, function(g)
    inherits(g, "text") && grepl(pattern, g$label %||% ""), logical(1)))[1]
  # find the layout row for that grob
  tg$layout$clip[which(vapply(tg$grobs, function(g) identical(g, tg$grobs[[i]]), logical(1)))[1]]
}

test_that("tab_add_title(just=) positions the title (#302)", {
  center <- ggtexttable(.df, rows = NULL) %>% tab_add_title("T", just = "center")
  right  <- ggtexttable(.df, rows = NULL) %>% tab_add_title("T", just = "right")
  left   <- ggtexttable(.df, rows = NULL) %>% tab_add_title("T", just = "left")
  expect_equal(.grob_x(center, "^T$"), 0.5)
  expect_equal(.grob_x(right, "^T$"), 0.98)
  expect_equal(.grob_x(left, "^T$"), 0.02)  # default, byte-identical
})

test_that("tab_add_footnote(just=) keeps its right default and honors center (#302)", {
  def <- ggtexttable(.df, rows = NULL) %>% tab_add_footnote("F")
  ctr <- ggtexttable(.df, rows = NULL) %>% tab_add_footnote("F", just = "center")
  expect_equal(.grob_x(def, "^F$"), 0.95)   # default right, byte-identical
  expect_equal(.grob_x(ctr, "^F$"), 0.5)
})

test_that("title/footnote are not clipped to the table width (#302)", {
  tab <- ggtexttable(.df, rows = NULL) %>%
    tab_add_title("A very wide sample table title", just = "center") %>%
    tab_add_footnote("A very wide footnote text here", just = "center")
  expect_equal(.grob_clip(tab, "wide sample"), "off")
  expect_equal(.grob_clip(tab, "wide footnote"), "off")
  expect_no_error(ggplot2::ggplot_gtable(ggplot2::ggplot_build(tab)))
})
