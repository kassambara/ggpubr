context("test-themeable-composites")

# ---- ggscatterhist ----------------------------------------------------------
test_that("style_scatterhist() restyles sub-plots and keeps the class", {
  p <- ggscatterhist(iris, x = "Sepal.Length", y = "Sepal.Width",
    color = "Species", margin.plot = "boxplot", print = FALSE)
  styled <- style_scatterhist(p,
    main = list(theme_bw(), labs(title = "Iris")),
    margin = theme_void())
  expect_s3_class(styled, "ggscatterhist")
  expect_equal(names(styled), c("sp", "xplot", "yplot"))
  # The title was added to the main scatter plot.
  expect_identical(styled$sp$labels$title, "Iris")
  # A single (non-list) component works too, and only touches what is asked.
  only.x <- style_scatterhist(p, xplot = labs(title = "x-margin"))
  expect_identical(only.x$xplot$labels$title, "x-margin")
  expect_null(only.x$yplot$labels$title)
  expect_silent(ggplot2::ggplotGrob(ggplot2::ggplot_build(styled$sp)))
})

test_that("editing $sp directly still restyles the composite", {
  p <- ggscatterhist(iris, x = "Sepal.Length", y = "Sepal.Width", print = FALSE)
  p$sp <- p$sp + theme_bw() + labs(title = "via $sp")
  expect_identical(p$sp$labels$title, "via $sp")
  expect_s3_class(p, "ggscatterhist")
})

test_that("`+` on a ggscatterhist gives a helpful error", {
  p <- ggscatterhist(iris, x = "Sepal.Length", y = "Sepal.Width", print = FALSE)
  # A non-ggplot right-hand side reaches the guard directly.
  expect_error(p + 1, "list of plots, not a single ggplot")
  expect_error(p + 1, "style_scatterhist")
})

test_that("style_scatterhist() rejects a non-ggscatterhist input", {
  expect_error(style_scatterhist(ggplot2::ggplot(iris), main = theme_bw()),
    "must be a `ggscatterhist`")
})

# ---- ggsummarystats ---------------------------------------------------------
test_that("style_summarystats() restyles sub-plots and keeps the class", {
  df <- ToothGrowth
  df$dose <- as.factor(df$dose)
  p <- ggsummarystats(df, x = "dose", y = "len", print = FALSE)
  styled <- style_summarystats(p,
    main = list(theme_bw(), labs(title = "Tooth growth")),
    table = theme_void())
  expect_s3_class(styled, "ggsummarystats")
  expect_equal(names(styled), c("main.plot", "summary.plot"))
  expect_identical(styled$main.plot$labels$title, "Tooth growth")
  expect_silent(ggplot2::ggplotGrob(ggplot2::ggplot_build(styled$main.plot)))
})

test_that("`+` on a ggsummarystats gives a helpful error", {
  df <- ToothGrowth
  df$dose <- as.factor(df$dose)
  p <- ggsummarystats(df, x = "dose", y = "len", print = FALSE)
  expect_error(p + 1, "list of plots, not a single ggplot")
  expect_error(p + 1, "style_summarystats")
})
