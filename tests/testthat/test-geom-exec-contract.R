test_that("documented geom and stat parameters reach their target layers", {
  d <- data.frame(x = seq_len(16), y = seq_len(16))

  histogram <- gghistogram(
    d, x = "x", bins = 4, boundary = 0, closed = "left", pad = TRUE
  )$layers[[1]]
  density <- ggdensity(
    d, x = "x", kernel = "rectangular", n = 64, outline.type = "both"
  )$layers[[1]]
  repel <- geom_exec(
    ggrepel::geom_text_repel,
    data = d, x = "x", y = "y", label = "x",
    box.padding = grid::unit(0.35, "lines"),
    point.padding = grid::unit(0.3, "lines")
  )

  observed <- list(
    histogram = unname(histogram$stat_params[c("boundary", "closed", "pad")]),
    density.stat = unname(density$stat_params[c("kernel", "n")]),
    density.geom = unname(density$geom_params["outline.type"]),
    repel = c(
      box.padding = as.numeric(repel$geom_params$box.padding),
      point.padding = as.numeric(repel$geom_params$point.padding)
    )
  )
  expected <- list(
    histogram = list(0, "left", TRUE),
    density.stat = list("rectangular", 64),
    density.geom = list("both"),
    repel = c(box.padding = 0.35, point.padding = 0.3)
  )

  expect_identical(observed, expected)
})

test_that("a non-aesthetic option stays an option when its value names a column", {
  d <- data.frame(x = seq_len(10), y = seq_len(10))
  layer <- geom_exec(
    ggplot2::geom_dotplot,
    data = d, x = "x", y = "y", binaxis = "y"
  )

  observed <- list(
    binaxis = layer$stat_params$binaxis,
    mapped = names(layer$mapping)
  )
  expect_identical(observed, list(binaxis = "y", mapped = c("x", "y")))
})

test_that("a documented stat aesthetic maps through its column", {
  layer <- gghistogram(
    iris, x = "Sepal.Length", weight = "Petal.Length", bins = 4
  )$layers[[1]]

  observed <- list(
    weight = rlang::as_label(layer$mapping$weight),
    passed.as.parameter = "weight" %in% names(layer$stat_params)
  )
  expect_identical(observed, list(
    weight = "Petal.Length",
    passed.as.parameter = FALSE
  ))
})

test_that("group stays an aesthetic for both constants and columns", {
  d <- data.frame(
    x = seq_len(4), y = seq_len(4),
    g = rep(c("a", "b"), each = 2)
  )
  constant <- geom_exec(
    ggplot2::geom_point, data = d, x = "x", y = "y", group = 1
  )
  column <- geom_exec(
    ggplot2::geom_point, data = d, x = "x", y = "y", group = "g"
  )

  observed <- list(
    constant = rlang::as_label(constant$mapping$group),
    constant.parameter = "group" %in% names(constant$aes_params),
    column = rlang::as_label(column$mapping$group)
  )
  expect_identical(observed, list(
    constant = "1", constant.parameter = FALSE, column = "g"
  ))
})

test_that("layer selectors determine target-specific aesthetic routing", {
  d <- data.frame(
    x = rep(1:2, each = 3), y = seq_len(6),
    w = rep(c(0.2, 0.4), each = 3)
  )
  layer <- geom_exec(
    geomfunc = ggplot2::stat_summary,
    data = d, x = "x", y = "y", geom = "errorbar", width = "w",
    fun.data = ggplot2::mean_se
  )
  built <- ggplot2::ggplot_build(ggplot2::ggplot() + layer)$data[[1]]

  observed <- list(
    width = rlang::as_label(layer$mapping$width),
    width.parameter = "width" %in% names(layer$aes_params),
    fun.data = layer$stat_params$fun.data,
    built.rows = nrow(built),
    built.limits.finite = all(is.finite(built$xmin), is.finite(built$xmax))
  )
  expect_identical(observed$width, "w")
  expect_false(observed$width.parameter)
  expect_identical(observed$fun.data, ggplot2::mean_se)
  expect_identical(observed$built.rows, 2L)
  expect_true(observed$built.limits.finite)
})
