test_that("ggtext preserves a mapped label.xx column with vector labels", {
  d <- data.frame(
    x = 1:4, y = 1:4,
    label.xx = rep(c("G1", "G2"), each = 2),
    stringsAsFactors = FALSE
  )
  p <- ggtext(d, "x", "y", label = letters[1:4], color = "label.xx")
  layer <- p$layers[[1]]
  built <- ggplot2::ggplot_build(p)$data[[1]]

  expect_identical(
    list(
      user_values = sort(unique(as.character(layer$data$label.xx))),
      label = rlang::as_label(layer$mapping$label),
      colours = length(unique(built$colour)),
      drawn = as.character(built$label)
    ),
    list(user_values = c("G1", "G2"), label = "label.xx1", colours = 2L, drawn = letters[1:4])
  )
})
