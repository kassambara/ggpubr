test_that("ggscatter rug accepts a point-size mapping", {
  d <- data.frame(x = 1:4, y = c(4, 1, 3, 2), s = c(1, 2, 3, 4))
  p <- ggscatter(d, "x", "y", size = "s", rug = TRUE)
  expect_silent(ggplot2::ggplot_build(p))
})

test_that("ggscatter assigns shapes to seven character groups", {
  d <- data.frame(x = 1:7, y = 7:1, group = letters[1:7])
  built <- ggplot2::ggplot_build(ggscatter(d, "x", "y", shape = "group"))
  expect_identical(sort(unique(built$data[[1]]$shape)), 1:7)
})
