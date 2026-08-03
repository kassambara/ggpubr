test_that("histogram labels preserve a user column named lab.y", {
  d <- data.frame(
    x = c(1, 1.2, 1.4, 1.6, 6, 6.2, 6.4, 6.6),
    lab.y = paste0("row-", seq_len(8))
  )
  p <- gghistogram(d, x = "x", label = "lab.y", bins = 4)
  built <- ggplot2::ggplot_build(p)
  labels <- as.character(built$data[[length(built$data)]]$label)

  expect_identical(labels, d$lab.y)
})

test_that("histogram facets preserve a user column named data", {
  d <- data.frame(
    x = c(1, 1.2, 1.4, 1.6, 6, 6.2, 6.4, 6.6),
    data = rep(c("F1", "F2"), each = 4),
    label = paste0("row-", seq_len(8))
  )
  p <- gghistogram(d, x = "x", facet.by = "data", label = "label", bins = 4)
  built <- ggplot2::ggplot_build(p)
  labels <- as.character(built$data[[length(built$data)]]$label)

  expect_identical(
    list(panels = nrow(built$layout$layout), labels = labels),
    list(panels = 2L, labels = d$label)
  )
})
