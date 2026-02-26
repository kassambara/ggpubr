test_that("geom_exec converts size to linewidth for bar/col geoms", {
  df <- data.frame(x = 1, y = 1)
  layer <- geom_exec(ggplot2::geom_col, data = df, x = "x", y = "y", size = 0.6)
  expect_equal(layer[["aes_params"]][["linewidth"]], 0.6)
})

test_that("geom_exec keeps size for non-linewidth geoms", {
  df <- data.frame(x = 1, y = 1)
  layer <- geom_exec(ggplot2::geom_point, data = df, x = "x", y = "y", size = 2)
  expect_equal(layer[["aes_params"]][["size"]], 2)
})
