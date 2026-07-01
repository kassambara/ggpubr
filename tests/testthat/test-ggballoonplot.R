test_that("ggballoonplot honors xlab/ylab and blanks them by default (#639)", {
  dat <- as.data.frame(matrix(1:6, nrow = 2,
    dimnames = list(c("r1", "r2"), c("c1", "c2", "c3"))))

  # Default: axis titles are blanked (unchanged behavior)
  th_default <- ggplot2::ggplot_build(ggballoonplot(dat))$plot$theme
  expect_s3_class(th_default$axis.title.x, "element_blank")
  expect_s3_class(th_default$axis.title.y, "element_blank")

  # When supplied, xlab/ylab are honored (not blanked, and label text set)
  p <- ggballoonplot(dat, xlab = "My X", ylab = "My Y")
  built <- ggplot2::ggplot_build(p)
  expect_false(inherits(built$plot$theme$axis.title.x, "element_blank"))
  expect_false(inherits(built$plot$theme$axis.title.y, "element_blank"))
  expect_equal(built$plot$labels$x, "My X")
  expect_equal(built$plot$labels$y, "My Y")

  # Only one label supplied: it shows, the other stays blank
  p_x <- ggballoonplot(dat, xlab = "Only X")
  th_x <- ggplot2::ggplot_build(p_x)$plot$theme
  expect_false(inherits(th_x$axis.title.x, "element_blank"))
  expect_s3_class(th_x$axis.title.y, "element_blank")

  # xlab = FALSE keeps the ggpar hide behavior (title blank)
  th_false <- ggplot2::ggplot_build(ggballoonplot(dat, xlab = FALSE))$plot$theme
  expect_s3_class(th_false$axis.title.x, "element_blank")
})
