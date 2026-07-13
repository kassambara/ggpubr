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

test_that("`+` does not restyle a ggscatterhist; use style_scatterhist()/$sp", {
  p <- ggscatterhist(iris, x = "Sepal.Length", y = "Sepal.Width", print = FALSE)
  # A ggscatterhist is a list of plots, not a single ggplot. ggplot2's own `+`
  # intercepts a ggplot right-hand side (theme/geom/scale) via the RHS and yields
  # NULL for a non-ggplot left side, so `+` cannot restyle the composite. This
  # documents that limitation honestly (the supported paths are the helper and
  # editing the sub-plots, tested above).
  expect_null(suppressWarnings(p + theme_bw()))
  expect_null(suppressWarnings(p + geom_point()))
  # The supported path returns a themed composite, not NULL.
  expect_s3_class(style_scatterhist(p, main = theme_bw()), "ggscatterhist")
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

test_that("`+` does not restyle a ggsummarystats; use style_summarystats()", {
  df <- ToothGrowth
  df$dose <- as.factor(df$dose)
  p <- ggsummarystats(df, x = "dose", y = "len", print = FALSE)
  # As for ggscatterhist: a composite is a list of plots, so `+` cannot restyle
  # it (ggplot2's `+` yields NULL for a non-ggplot left side). Use the helper or
  # edit $main.plot / $summary.plot.
  expect_null(suppressWarnings(p + theme_bw()))
  expect_s3_class(style_summarystats(p, main = theme_bw()), "ggsummarystats")
})
