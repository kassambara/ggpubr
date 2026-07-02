context("test-colour-alias")

# #317: accept the British spelling `colour` as an alias for `color` in the
# core plotting functions (those routed through .plotter). Previously `colour`
# was silently ignored.

ng <- function(p) length(unique(ggplot2::ggplot_build(p)$data[[1]]$colour))

test_that("colour is an alias for color (grouping) across core functions (#317)", {
  tg <- ToothGrowth
  tg$dose <- factor(tg$dose)
  expect_equal(ng(ggscatter(mtcars, "wt", "mpg", colour = "cyl")),
               ng(ggscatter(mtcars, "wt", "mpg", color = "cyl")))
  expect_equal(ng(ggboxplot(tg, "dose", "len", colour = "dose")),
               ng(ggboxplot(tg, "dose", "len", color = "dose")))
  expect_equal(ng(ggviolin(tg, "dose", "len", colour = "dose")), 3L)
  expect_equal(ng(ggstripchart(tg, "dose", "len", colour = "dose")), 3L)
})

test_that("colour works as a static color and does not break color (#317)", {
  d <- ggplot2::ggplot_build(ggscatter(mtcars, "wt", "mpg", colour = "red"))$data[[1]]
  expect_true(all(d$colour == "red"))
  # color = still works and default is unchanged
  d2 <- ggplot2::ggplot_build(ggscatter(mtcars, "wt", "mpg"))$data[[1]]
  expect_true(all(d2$colour == "black"))
  d3 <- ggplot2::ggplot_build(ggscatter(mtcars, "wt", "mpg", color = "blue"))$data[[1]]
  expect_true(all(d3$colour == "blue"))
})
