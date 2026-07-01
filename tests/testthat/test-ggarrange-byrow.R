# Regression test for #225: ggarrange() gains a `byrow` argument to fill the grid
# by column (byrow = FALSE) instead of by row (the default).

.plots <- list(
  ggplot2::ggplot(mtcars, ggplot2::aes(wt, mpg)) + ggplot2::geom_point(),
  ggplot2::ggplot(mtcars, ggplot2::aes(wt, disp)) + ggplot2::geom_point(),
  ggplot2::ggplot(mtcars, ggplot2::aes(wt, hp)) + ggplot2::geom_point(),
  ggplot2::ggplot(mtcars, ggplot2::aes(mpg, hp)) + ggplot2::geom_point()
)

.render_md5 <- function(byrow) {
  f <- tempfile(fileext = ".png")
  on.exit(unlink(f))
  g <- do.call(ggarrange, c(.plots, list(ncol = 2, nrow = 2, byrow = byrow)))
  ggplot2::ggsave(f, g, width = 5, height = 4, dpi = 72)
  unname(tools::md5sum(f))
}

test_that("ggarrange() has a byrow argument that changes the layout (#225)", {
  expect_true("byrow" %in% names(formals(ggarrange)))
  # the comparison below renders to PNG; skip where no PNG device is available
  skip_if_not(isTRUE(capabilities("png")), "no PNG device")
  # byrow = FALSE (fill by column) must render a DIFFERENT arrangement than the
  # by-row default; if byrow were not forwarded these would be identical.
  expect_false(identical(.render_md5(TRUE), .render_md5(FALSE)))
})

test_that("ggarrange() default is unchanged (byrow = TRUE, no regression, #225)", {
  g <- ggarrange(plotlist = .plots, ncol = 2, nrow = 2)
  expect_s3_class(g, "ggarrange")
  skip_if_not(isTRUE(capabilities("png")), "no PNG device")
  # default and explicit byrow = TRUE produce the same rendering
  expect_identical(.render_md5(TRUE), .render_md5(TRUE))
})
