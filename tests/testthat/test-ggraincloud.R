context("test-ggraincloud")

df <- ToothGrowth
df$dose <- as.factor(df$dose)

# ---- GeomViolinHalf ---------------------------------------------------------
test_that("geom_violin_half draws only one side of the density", {
  br <- ggplot2::ggplot_build(
    ggplot2::ggplot(df, ggplot2::aes(dose, len)) + geom_violin_half(side = "right")
  )$data[[1]]
  bl <- ggplot2::ggplot_build(
    ggplot2::ggplot(df, ggplot2::aes(dose, len)) + geom_violin_half(side = "left")
  )$data[[1]]
  # side = "right": the left edge is collapsed to the group centre (xmin == x)
  expect_true(all(abs(br$xmin - br$x) < 1e-9))
  # side = "left": the right edge is collapsed to the group centre (xmax == x)
  expect_true(all(abs(bl$xmax - bl$x) < 1e-9))
})

test_that("geom_violin_half renders at draw time", {
  p <- ggplot2::ggplot(df, ggplot2::aes(dose, len)) +
    geom_violin_half(side = "right", fill = "#00AFBB")
  expect_silent(ggplot2::ggplotGrob(p))
})

test_that("geom_violin_half validates the side argument", {
  expect_error(geom_violin_half(side = "up"))
})

# ---- ggraincloud ------------------------------------------------------------
test_that("ggraincloud composes cloud + box + rain into one ggplot", {
  p <- ggraincloud(df, x = "dose", y = "len")
  expect_s3_class(p, "ggplot")
  geoms <- unname(vapply(p$layers, function(l) class(l$geom)[1], character(1)))
  expect_equal(geoms, c("GeomViolinHalf", "GeomBoxplot", "GeomPoint"))
  expect_silent(ggplot2::ggplotGrob(p))
})

test_that("ggraincloud flips to the horizontal orientation by default", {
  expect_true(inherits(ggraincloud(df, "dose", "len")$coordinates, "CoordFlip"))
  expect_false(inherits(ggraincloud(df, "dose", "len", flip = FALSE)$coordinates, "CoordFlip"))
})

test_that("ggraincloud rain points are offset opposite the cloud", {
  # side = "right" -> cloud to the right, rain offset to the left (negative)
  pr <- ggraincloud(df, "dose", "len", side = "right")
  rain.r <- ggplot2::ggplot_build(pr)$data[[3]]
  # side = "left" -> rain offset to the right (positive)
  pl <- ggraincloud(df, "dose", "len", side = "left")
  rain.l <- ggplot2::ggplot_build(pl)$data[[3]]
  # side="right" cloud -> rain sits just LEFT of each group centre (x < centre)
  expect_true(all(rain.r$x < round(rain.r$x)))
  # side="left" cloud -> rain sits just RIGHT of each group centre (x > centre)
  expect_true(all(rain.l$x > round(rain.l$x)))
})

test_that("ggraincloud supports a single fill and by-group fill", {
  expect_silent(ggplot2::ggplotGrob(ggraincloud(df, "dose", "len", fill = "#00AFBB")))
  expect_silent(ggplot2::ggplotGrob(ggraincloud(df, "dose", "len", palette = "jco")))
})

test_that("ggraincloud honors the 'x' sentinel and NULL for color/fill", {
  # "x" and NULL both mean "by the x group" and must not crash.
  expect_silent(ggplot2::ggplotGrob(ggraincloud(df, "dose", "len", fill = "x")))
  expect_silent(ggplot2::ggplotGrob(ggraincloud(df, "dose", "len", color = "x")))
  # default (NULL) colors the rain by group
  d3 <- ggplot2::ggplot_build(ggraincloud(df, "dose", "len"))$data[[3]]
  expect_gt(length(unique(d3$colour)), 1)
})

test_that("ggraincloud applies a single color to the rain and a single fill to the cloud", {
  pr <- ggraincloud(df, "dose", "len", color = "red")
  rain <- ggplot2::ggplot_build(pr)$data[[3]]
  expect_equal(unique(rain$colour), "red")

  pf <- ggraincloud(df, "dose", "len", fill = "#00AFBB")
  cloud <- ggplot2::ggplot_build(pf)$data[[1]]
  expect_equal(unique(cloud$fill), "#00AFBB")
})

test_that("ggraincloud supports faceting", {
  p <- ggraincloud(df, "supp", "len", facet.by = "dose")
  expect_silent(ggplot2::ggplotGrob(p))
})

test_that("ggraincloud coerces a non-factor x", {
  d2 <- df
  d2$dose <- as.character(d2$dose)
  expect_silent(ggplot2::ggplotGrob(ggraincloud(d2, "dose", "len")))
})
