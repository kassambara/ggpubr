test_that(".brewerpal keeps distinct sequential palette names", {
  pals <- .brewerpal()

  expect_true("YlGnBu" %in% pals)
  expect_true("YlOrBr" %in% pals)
  expect_false("YlGnBu YlOrBr" %in% pals)
})

test_that(".get_brewer_pal supports YlOrBr", {
  skip_if_not_installed("RColorBrewer")
  cols <- .get_brewer_pal("YlOrBr", 5)
  expect_length(cols, 5)
  expect_type(cols, "character")
})

test_that(".get_pal routes YlOrBr to brewer palettes", {
  skip_if_not_installed("RColorBrewer")
  cols <- .get_pal("YlOrBr", 5)
  expect_length(cols, 5)
  expect_type(cols, "character")
})

test_that(".get_ggplot_ncolors counts mapped fills", {
  d <- data.frame(x = letters[1:3], y = 1:3)
  p <- ggplot2::ggplot(d, ggplot2::aes(x, y, fill = x)) + ggplot2::geom_col()
  expect_identical(.get_ggplot_ncolors(p), 3L)
})
