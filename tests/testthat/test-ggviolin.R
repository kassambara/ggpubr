test_that("ggviolin maps draw_quantiles to quantiles with default linetype", {
  df <- ToothGrowth

  expect_warning(
    p <- ggviolin(df, "dose", "len", add = "none", draw_quantiles = 0.5),
    "Mapped to `quantiles`"
  )

  layer <- p$layers[[1]]
  expect_equal(layer$stat_params$quantiles, 0.5)
  expect_null(layer$geom_params$draw_quantiles)
})

test_that("ggviolin warns when draw_quantiles is provided with quantiles", {
  df <- ToothGrowth

  expect_warning(
    p <- ggviolin(
      df,
      "dose",
      "len",
      add = "none",
      draw_quantiles = 0.5,
      quantiles = 0.25
    ),
    "ignored because `quantiles` was supplied"
  )

  layer <- p$layers[[1]]
  expect_equal(layer$stat_params$quantiles, 0.25)
})

test_that("ggviolin warns when draw_quantiles is used with explicit linetype", {
  df <- ToothGrowth

  expect_warning(
    p <- ggviolin(
      df,
      "dose",
      "len",
      add = "none",
      draw_quantiles = 0.5,
      quantile.linetype = "dashed"
    ),
    "Use `quantiles` instead"
  )

  layer <- p$layers[[1]]
  expect_equal(layer$stat_params$quantiles, 0.5)
})

test_that("ggviolin warns when quantiles are provided without linetype", {
  df <- ToothGrowth

  expect_warning(
    ggviolin(df, "dose", "len", add = "none", quantiles = 0.5),
    "quantile.linetype"
  )
})
