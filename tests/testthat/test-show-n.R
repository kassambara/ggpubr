context("test-show-n")

# #598 / #627: ggboxplot(), ggviolin() and ggstripchart() gain an opt-in
# `show.n` argument that prints the per-group number of observations at the top
# of each group. Default (show.n = FALSE) adds nothing (byte-identical output).

df <- ToothGrowth
df$dose <- factor(df$dose)

.text_layer <- function(p) {
  which(vapply(p$layers, function(l) inherits(l$geom, "GeomText"), logical(1)))
}
.n_labels <- function(p) {
  b <- ggplot2::ggplot_build(p)
  i <- .text_layer(p)[1]
  if (is.na(i)) return(character(0))
  as.character(b$data[[i]]$label)
}

test_that("default show.n = FALSE adds no text layer (unchanged output)", {
  for (fn in list(ggboxplot, ggviolin, ggstripchart)) {
    p <- fn(df, "dose", "len")
    expect_length(.text_layer(p), 0)
  }
})

test_that("show.n = TRUE shows one count per x tick when ungrouped", {
  for (fn in list(ggboxplot, ggviolin, ggstripchart)) {
    p <- fn(df, "dose", "len", show.n = TRUE)
    expect_equal(sort(.n_labels(p)), c("n = 20", "n = 20", "n = 20"))
    expect_silent(ggplot2::ggplotGrob(p))
  }
})

test_that("show.n = TRUE shows per-group counts for dodged box/violin", {
  for (fn in list(ggboxplot, ggviolin)) {
    p <- fn(df, "dose", "len", color = "supp", show.n = TRUE)
    labs <- .n_labels(p)
    expect_length(labs, 6)                 # 3 doses x 2 supp
    expect_true(all(labs == "n = 10"))
  }
})

test_that("stripchart adapts to its position (total when not dodged, per-group when jitterdodged)", {
  # default position_jitter (points not dodged) -> total per x
  p1 <- ggstripchart(df, "dose", "len", color = "supp", show.n = TRUE)
  expect_equal(sort(.n_labels(p1)), c("n = 20", "n = 20", "n = 20"))
  # position_jitterdodge -> per-group
  p2 <- ggstripchart(df, "dose", "len", color = "supp",
                     position = ggplot2::position_jitterdodge(), show.n = TRUE)
  expect_length(.n_labels(p2), 6)
  expect_true(all(.n_labels(p2) == "n = 10"))
})

test_that("counts respect select/remove (taken from the plotted data)", {
  p <- ggboxplot(df, "dose", "len", select = c("0.5", "1"), show.n = TRUE)
  expect_equal(sort(.n_labels(p)), c("n = 20", "n = 20"))

  d2 <- rbind(df, data.frame(len = c(1, 2), supp = "OJ", dose = factor("0.5")))
  p2 <- ggboxplot(d2, "dose", "len", show.n = TRUE)
  # dose 0.5 now has 22 observations
  b <- ggplot2::ggplot_build(p2)
  labs <- as.character(b$data[[.text_layer(p2)[1]]]$label)
  expect_true("n = 22" %in% labs)
})

test_that("show.n works with facets (one set of counts per panel)", {
  p <- ggboxplot(df, "dose", "len", color = "supp", facet.by = "supp", show.n = TRUE)
  expect_length(.n_labels(p), 6)
  expect_silent(ggplot2::ggplotGrob(p))
})

test_that("show.n handles combine = TRUE per response variable (not double-counted)", {
  df2 <- df
  df2$len2 <- df2$len * 1.1
  p <- ggboxplot(df2, "supp", c("len", "len2"), combine = TRUE, show.n = TRUE)
  labs <- .n_labels(p)
  expect_length(labs, 4)                       # 2 supp x 2 response panels
  expect_true(all(labs == "n = 30"))           # not n = 60
  expect_silent(ggplot2::ggplotGrob(p))
})

test_that("show.n with an empty selection does not error and adds no labels", {
  p <- ggboxplot(df, "dose", "len", select = "does-not-exist", show.n = TRUE)
  expect_length(.text_layer(p), 0)
  expect_error(ggplot2::ggplot_build(p), NA)
})

test_that("show.n counts per box when color and fill map to different columns", {
  d <- data.frame(
    x = rep(c("A", "B"), each = 8),
    s = rep(c("p", "q"), 8),
    b = rep(rep(c("m", "n"), each = 2), 4),
    y = 1:16
  )
  p <- ggboxplot(d, "x", "y", color = "s", fill = "b", show.n = TRUE)
  labs <- .n_labels(p)
  expect_length(labs, 8)                        # 2 x * (2 s * 2 b)
  expect_true(all(labs == "n = 2"))
})

test_that("show.n counts each group correctly on an unbalanced dataset", {
  d <- data.frame(
    g = c("a", "a", "a", "b", "b"),
    y = c(1, 2, 3, 4, 5)
  )
  p <- ggboxplot(d, "g", "y", show.n = TRUE)
  expect_equal(sort(.n_labels(p)), c("n = 2", "n = 3"))
})
