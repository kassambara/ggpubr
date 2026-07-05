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

# drop argument: keep sparse groups aligned with other geoms (#381) -----------

# Data with a single-point sub-group at dose = 1 (OJ), which geom_violin drops.
.viol_sparse <- rbind(
  subset(ToothGrowth, dose == 0.5),
  ToothGrowth[ToothGrowth$dose == 1 & ToothGrowth$supp == "VC", ][1:6, ],
  ToothGrowth[ToothGrowth$dose == 1 & ToothGrowth$supp == "OJ", ][1, ],
  subset(ToothGrowth, dose == 2)
)
.viol_sparse$dose <- factor(.viol_sparse$dose)

.violin_lanes <- function(p, layer = 1) {
  # geom_violin emits expected "fewer than two datapoints" warnings for the
  # sparse group; they are incidental to what these tests assert.
  b <- suppressWarnings(ggplot2::ggplot_build(p))
  d <- b$data[[layer]]
  s <- d[abs(d$x - 2) < 0.45, ]           # dose = 1 is the 2nd x position
  sort(unique(round(s$x, 3)))
}

test_that("ggviolin forwards an explicit drop to geom_violin", {
  p <- ggviolin(.viol_sparse, "dose", "len", color = "supp", add = "boxplot", drop = FALSE)
  expect_false(p$layers[[1]]$stat_params$drop)
  # An explicit drop = TRUE is respected (it turns the automatic alignment off).
  p2 <- ggviolin(.viol_sparse, "dose", "len", color = "supp", add = "boxplot", drop = TRUE)
  expect_true(p2$layers[[1]]$stat_params$drop)
})

test_that("ggviolin auto-aligns a sparse grouped violin by default (#381)", {
  # Omitting drop/position now reserves the empty dodge lane for the single-point
  # sub-group, so the sparse dose = 1 keeps TWO aligned lanes and the auto-applied
  # drop is FALSE.
  omitted <- ggviolin(.viol_sparse, "dose", "len", color = "supp", add = "boxplot")
  expect_length(.violin_lanes(omitted, 1), 2)
  expect_false(omitted$layers[[1]]$stat_params$drop)
  # Explicit drop = TRUE opts out -> historical behavior: the sparse lane is dropped
  # and the remaining violin re-centers (a single lane at dose = 1).
  explicit <- ggviolin(.viol_sparse, "dose", "len", color = "supp", add = "boxplot", drop = TRUE)
  expect_length(.violin_lanes(explicit, 1), 1)
})

test_that("omitting drop is byte-identical to drop = TRUE for balanced data (#381)", {
  # The automatic alignment only fires on a sparse one-point cell; for balanced
  # grouped data the default is unchanged, so omitting drop equals drop = TRUE.
  bal <- ToothGrowth
  bal$dose <- factor(bal$dose)
  omitted  <- ggviolin(bal, "dose", "len", color = "supp", add = "boxplot")
  explicit <- ggviolin(bal, "dose", "len", color = "supp", add = "boxplot", drop = TRUE)
  expect_equal(
    suppressWarnings(ggplot2::ggplot_build(omitted))$data[[1]],
    suppressWarnings(ggplot2::ggplot_build(explicit))$data[[1]]
  )
})

test_that("drop = FALSE + preserve = 'single' aligns violins with added boxplots (#381)", {
  p <- ggviolin(
    .viol_sparse, "dose", "len", color = "supp", add = "boxplot",
    drop = FALSE, position = position_dodge(0.8, preserve = "single")
  )
  violin_x <- .violin_lanes(p, 1)
  box_x    <- .violin_lanes(p, 2)
  expect_length(violin_x, 2)          # empty lane reserved -> two lanes
  expect_equal(violin_x, box_x, tolerance = 1e-6)  # violins line up with boxes
})
