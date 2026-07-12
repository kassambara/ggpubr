context("test-ggestimates")

or_data <- function() {
  data.frame(
    term = c("Age", "Sex", "BMI", "Smoker", "Trt"),
    estimate = c(1.03, 1.45, 0.98, 2.10, 0.62),
    conf.low = c(0.99, 1.05, 0.94, 1.40, 0.45),
    conf.high = c(1.07, 2.00, 1.02, 3.15, 0.85),
    stringsAsFactors = FALSE
  )
}
layer_of <- function(p, geom) {
  i <- which(vapply(p$layers, function(l) inherits(l$geom, geom), logical(1)))
  ggplot2::layer_data(p, i[1])
}
ylabs_top_bottom <- function(p) {
  rev(ggplot2::ggplot_build(p)$layout$panel_params[[1]]$y$get_labels())
}

test_that("ggestimates() draws points and intervals at the data values", {
  d <- or_data()
  p <- ggestimates(d, label = "term")
  expect_s3_class(p, "ggplot")
  pt <- layer_of(p, "GeomPoint")
  eb <- layer_of(p, "GeomErrorbar")
  # Points sit at the estimates and bars span conf.low..conf.high (data order,
  # first row on top -> highest y).
  expect_equal(pt$x[order(-pt$y)], d$estimate)
  expect_equal(eb$xmin[order(-eb$y)], d$conf.low)
  expect_equal(eb$xmax[order(-eb$y)], d$conf.high)
  expect_silent(ggplot2::ggplotGrob(ggplot2::ggplot_build(p)))
})

test_that("log.scale places the geometry in log space and guards non-positive values", {
  d <- or_data()
  p <- ggestimates(d, label = "term", ref.line = 1, log.scale = TRUE)
  pt <- layer_of(p, "GeomPoint")
  expect_equal(10^pt$x[order(-pt$y)], d$estimate)
  # A non-positive value cannot be log-scaled.
  neg <- data.frame(g = c("a", "b"), estimate = c(-0.5, 0.3),
    conf.low = c(-1.2, -0.1), conf.high = c(0.2, 0.7))
  expect_error(ggestimates(neg, label = "g", log.scale = TRUE), "positive")
})

test_that("log.scale uses ref.line = 1 by default and rejects a non-positive one", {
  d <- or_data()
  vline_raw <- function(p) {
    i <- which(vapply(p$layers, function(l) inherits(l$geom, "GeomVline"), logical(1)))
    ggplot2::layer_data(p, i[1])$xintercept[1]
  }
  # Default ref.line under log.scale becomes 1 (log10(1) = 0 in built coords),
  # so the plot renders cleanly instead of a blank panel from log10(0) = -Inf.
  p <- ggestimates(d, label = "term", log.scale = TRUE)
  expect_equal(10^vline_raw(p), 1)
  expect_silent(ggplot2::ggplotGrob(ggplot2::ggplot_build(p)))
  pt <- layer_of(p, "GeomPoint")
  expect_true(all(is.finite(pt$x)))
  # An explicit ref.line = 0 cannot sit on a log axis -> clear error.
  expect_error(ggestimates(d, label = "term", log.scale = TRUE, ref.line = 0),
    "must be positive")
  # An explicitly supplied positive ref.line is respected.
  expect_equal(10^vline_raw(ggestimates(d, label = "term", log.scale = TRUE, ref.line = 1.5)), 1.5)
})

test_that("a fixed color is applied to the points even when size is mapped", {
  d <- or_data()
  d$w <- c(5, 20, 8, 15, 10)
  p <- ggestimates(d, label = "term", color = "blue", size = "w")
  pt <- layer_of(p, "GeomPoint")
  eb <- layer_of(p, "GeomErrorbar")
  expect_equal(unique(pt$colour), "blue")   # points honor the color
  expect_equal(unique(eb$colour), "blue")   # bars too
})

test_that("the reference line is drawn at ref.line and can be omitted", {
  d <- or_data()
  p0 <- ggestimates(d, label = "term")                 # default 0
  p1 <- ggestimates(d, label = "term", ref.line = 1)
  pna <- ggestimates(d, label = "term", ref.line = NA)
  vline <- function(p) {
    i <- which(vapply(p$layers, function(l) inherits(l$geom, "GeomVline"), logical(1)))
    if (length(i) == 0) return(NA_real_)
    ggplot2::layer_data(p, i[1])$xintercept[1]
  }
  expect_equal(vline(p0), 0)
  expect_equal(vline(p1), 1)
  expect_true(is.na(vline(pna)))  # no vline layer
})

test_that("the ci.text column is present by default and can be turned off", {
  d <- or_data()
  p <- ggestimates(d, label = "term", digits = 2)
  txt <- layer_of(p, "GeomText")
  # Every row's "est (low, high)" string appears.
  expect_true(all(paste0(
    formatC(d$estimate, format = "f", digits = 2), " (",
    formatC(d$conf.low, format = "f", digits = 2), ", ",
    formatC(d$conf.high, format = "f", digits = 2), ")"
  ) %in% txt$label))
  p_off <- ggestimates(d, label = "term", ci.text = FALSE)
  no_text <- !any(vapply(p_off$layers, function(l) inherits(l$geom, "GeomText"), logical(1)))
  expect_true(no_text)
})

test_that("sort orders the rows", {
  d <- or_data()
  expect_equal(ylabs_top_bottom(ggestimates(d, label = "term", sort = "none")),
    d$term)
  expect_equal(ylabs_top_bottom(ggestimates(d, label = "term", sort = "estimate")),
    d$term[order(d$estimate)])
  expect_equal(
    ylabs_top_bottom(ggestimates(d, label = "term", sort = "estimate", descending = TRUE)),
    d$term[order(-d$estimate)])
  expect_equal(ylabs_top_bottom(ggestimates(d, label = "term", sort = "label")),
    sort(d$term))
})

test_that("labels default to row names; missing columns and NA rows are handled", {
  d <- or_data()
  rn <- d[, c("estimate", "conf.low", "conf.high")]
  rownames(rn) <- d$term
  expect_equal(ylabs_top_bottom(ggestimates(rn)), d$term)   # from rownames
  expect_error(ggestimates(d[, 1:2], label = "term"), "not found")
  d_na <- d
  d_na$estimate[2] <- NA
  expect_warning(p <- ggestimates(d_na, label = "term"), "dropped")
  expect_equal(nrow(layer_of(p, "GeomPoint")), 4)
})

test_that("label.hjust controls the row-label justification (right by default)", {
  d <- or_data()
  hjust_of <- function(p) {
    g <- ggplot2::ggplot_gtable(ggplot2::ggplot_build(p))
    p$theme$axis.text.y$hjust
  }
  expect_equal(hjust_of(ggestimates(d, label = "term")), 1)             # default right
  expect_equal(hjust_of(ggestimates(d, label = "term", label.hjust = 0)), 0)
  expect_silent(ggplot2::ggplotGrob(ggplot2::ggplot_build(
    ggestimates(d, label = "term", label.hjust = 0))))
})

test_that("color maps a grouping column and applies the palette", {
  d <- or_data()
  p <- ggestimates(d, label = "term", color = "term", palette = "jco")
  pt <- layer_of(p, "GeomPoint")
  # One color per group (5 distinct), not a single constant.
  expect_equal(length(unique(pt$colour)), 5)
})
