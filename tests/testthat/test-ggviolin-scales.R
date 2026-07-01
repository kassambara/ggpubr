# Regression tests for #398: ggviolin(scales = "free") crashed at draw time
# ("object 'violinwidth' not found") because .violin_params() used `$`, which
# partial-matched the facet argument `scales` to the violin `scale` parameter,
# setting an invalid scale = "free".

test_that("ggviolin() with scales='free' renders (no crash) (#398)", {
  expect_no_error(
    ggplot2::ggplot_gtable(ggplot2::ggplot_build(
      ggviolin(ToothGrowth, x = "dose", y = "len", scales = "free")
    ))
  )
  # violinwidth must be computed (its absence caused the crash)
  d <- ggplot2::ggplot_build(
    ggviolin(ToothGrowth, x = "dose", y = "len", scales = "free"))$data[[1]]
  expect_true("violinwidth" %in% names(d))
})

test_that("ggviolin() scales='free' reaches faceting, not the violin scale (#398)", {
  p <- ggviolin(iris, x = "Species", y = "Sepal.Length",
                facet.by = "Species", scales = "free")
  b <- ggplot2::ggplot_build(p)
  expect_true(isTRUE(b$layout$facet$params$free$x))
  expect_true(isTRUE(b$layout$facet$params$free$y))
  # the violin scale itself stays at its default, NOT "free"
  expect_equal(p$layers[[1]]$stat_params$scale, "area")
})

test_that("ggviolin() explicit scale= is still honored (no regression, #398)", {
  expect_equal(
    ggviolin(ToothGrowth, x = "dose", y = "len", scale = "count")$layers[[1]]$stat_params$scale,
    "count"
  )
  expect_equal(
    ggviolin(ToothGrowth, x = "dose", y = "len")$layers[[1]]$stat_params$scale,
    "area"
  )
})
