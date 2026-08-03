test_that("ggpaired preserves a user id column when id is omitted", {
  d <- data.frame(
    condition = factor(rep(c("before", "after"), each = 3)),
    value = c(1, 2, 3, 2, 3, 4),
    id = c("s3", "s1", "s2", "s3", "s1", "s2")
  )
  p <- ggpaired(d, x = "condition", y = "value")
  line <- p$layers[[2]]

  expect_identical(
    list(id = line$data$id, group = rlang::as_label(line$mapping$group)),
    list(id = d$id, group = ".ggpubr.pair.id.")
  )
})

test_that("ggpaired jitter preserves a same-named facet column", {
  d <- data.frame(
    condition = factor(rep(c("before", "after"), each = 4)),
    value = c(1, 2, 3, 4, 2, 3, 4, 5),
    .ggpubr.x.jitter. = rep(c("F1", "F2"), times = 4),
    check.names = FALSE
  )
  p <- ggpaired(
    d, x = "condition", y = "value",
    facet.by = ".ggpubr.x.jitter.", jitter = 0.08
  )
  built <- ggplot2::ggplot_build(p)
  line <- p$layers[[2]]

  expect_identical(
    list(
      facets = sort(unique(as.character(built$layout$layout[[".ggpubr.x.jitter."]]))),
      values = line$data[[".ggpubr.x.jitter."]],
      x = rlang::as_label(line$mapping$x)
    ),
    list(
      facets = c("F1", "F2"), values = d[[".ggpubr.x.jitter."]],
      x = ".ggpubr.x.jitter.1"
    )
  )
})
