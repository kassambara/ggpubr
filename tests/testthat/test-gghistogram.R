context("gghistogram")

test_that("gghistogram works when using after_stat()", {
  p <- gghistogram(iris, x = "Sepal.Length", y = "after_stat(density * width)", bins = 30)
  p_build <- ggplot2::ggplot_build(p)
  df <- p_build$data[[1]]
  observed_count <- df$count
  expected_count <- c(4, 1, 4, 2, 11, 10, 9, 4, 7, 7, 6, 8, 7, 9, 6, 4, 9, 12, 2, 8, 3, 5, 1, 3, 1, 1, 1, 4, 0, 1)
  expect_equal(observed_count, expected_count)
})
