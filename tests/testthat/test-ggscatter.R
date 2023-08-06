context("ggscatter")

test_that("ggscatter works when there is spaces in variable names", {
  data("mtcars")
  df <- mtcars[, c("wt", "mpg")]
  colnames(df) <- c("Dimension 1", "Dimension 2")
  p <- ggscatter(df, x = "Dimension 1", y = "Dimension 2" )
  p_build <- ggplot2::ggplot_build(p)
  df_build <- p_build$data[[1]]
  expect_equal(df$`Dimension 1`, df_build$x)
})


test_that("ggscatter can handle non-standard column names when ggpubr.parse_aes global option is set to FALSE (#229)", {
  data("mtcars")
  df <- mtcars[, c("wt", "mpg")]
  colnames(df) <- c("A-A", "B-B")

  old_options <- options(ggpubr.parse_aes = FALSE)
  p <- ggscatter(df, x = "A-A", y = "B-B" )
  options(old_options)
  p_build <- ggplot2::ggplot_build(p)
  df_build <- p_build$data[[1]]
  expect_equal(df$`A-A`, df_build$x)
})
