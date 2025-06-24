context("gghistogram")

test_that("gghistogram works when using after_stat()", {
  p <- gghistogram(iris, x = "Sepal.Length", y = "after_stat(density * width)", bins = 30)
  p_build <- ggplot2::ggplot_build(p)
  df <- p_build$data[[1]]
  observed_count <- df$count
  if (is_pkg_version_sup("ggplot2", "3.5.2")) {
    expected_count <- c(1, 3, 5, 2, 5, 6, 19, 4, 1, 6, 13, 8, 7, 3, 12, 4, 9, 7, 5, 10, 3, 4, 1, 4, 1, 1, 0, 5, 0, 1)
  } else {
    expected_count <- c(4, 1, 4, 2, 11, 10, 9, 4, 7, 7, 6, 8, 7, 9, 6, 4, 9, 12, 2, 8, 3, 5, 1, 3, 1, 1, 1, 4, 0, 1)
  }
  expect_equal(observed_count, expected_count)
})


test_that("gghistogram works when using after_stat() with trailing space inside parentheses", {
  p <- gghistogram(iris, x = "Sepal.Length", y = "after_stat(density  )", bins = 30)
  p_build <- ggplot2::ggplot_build(p)
  df <- p_build$data[[1]]
  observed_count <- df$count
  if (is_pkg_version_sup("ggplot2", "3.5.2")) {
    expected_count <- c(1, 3, 5, 2, 5, 6, 19, 4, 1, 6, 13, 8, 7, 3, 12, 4, 9, 7, 5, 10, 3, 4, 1, 4, 1, 1, 0, 5, 0, 1)
  } else {
    expected_count <- c(4, 1, 4, 2, 11, 10, 9, 4, 7, 7, 6, 8, 7, 9, 6, 4, 9, 12, 2, 8, 3, 5, 1, 3, 1, 1, 1, 4, 0, 1)
  }
  expect_equal(observed_count, expected_count)
})
