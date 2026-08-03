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

test_that("ggtext label selection tolerates a user column named label", {
  d <- data.frame(
    x = 1:4, y = 1:4, lab = letters[1:4],
    label = paste0("user", 1:4)
  )
  drawn <- ggplot2::ggplot_build(
    ggtext(d, "x", "y", label = "lab", label.select = c("b", "d"))
  )$data[[1]]$label

  expect_identical(as.character(drawn), c("b", "d"))
})

test_that("ggscatter label selection tolerates a user column named label", {
  d <- data.frame(
    x = 1:4, y = 4:1, lab = letters[1:4],
    label = paste0("user", 1:4)
  )
  p <- ggscatter(d, "x", "y", label = "lab", label.select = c("a", "c"))
  text_layer <- which(vapply(p$layers, function(layer) {
    inherits(layer$geom, "GeomText") || inherits(layer$geom, "GeomTextRepel")
  }, logical(1)))
  drawn <- ggplot2::ggplot_build(p)$data[[text_layer[[1]]]]$label

  expect_identical(as.character(drawn), c("a", "c"))
})
