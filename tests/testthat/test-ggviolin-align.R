context("test-ggviolin-align")

# #381: grouped ggviolin() violins failed to line up with their box/dot layers
# when a sub-group was too sparse for geom_violin() to compute a density
# (ggplot2 >= 4.0 drops groups with fewer than two points from BOTH the plot and
# the dodge, so the remaining violin re-centers). ggviolin() now reserves the
# empty dodge lane automatically - but ONLY when the user set neither `drop` nor
# `position` AND the data actually has a grouped one-point cell. Every other input
# keeps the historical default (drop = TRUE, position = position_dodge(0.8)), so
# balanced / ungrouped / faceted / legitimately-unbalanced plots are byte-identical.

suppressPackageStartupMessages(library(ggplot2))

# sorted violin lane centers (npc-free, in data x units)
.violin_lane_centers <- function(p) {
  b <- suppressWarnings(suppressMessages(ggplot2::ggplot_build(p)))
  i <- which(vapply(p$layers, function(l) inherits(l$geom, "GeomViolin"), logical(1)))[1]
  d <- b$data[[i]]
  sort(unname(vapply(split(d, d$group), function(g) mean(range(g$x)), numeric(1))))
}

tg <- ToothGrowth; tg$dose <- factor(tg$dose)

# data from the issue: dose = 1 / OJ has a single point (no density)
sparse <- rbind(
  subset(ToothGrowth, dose == 0.5),
  ToothGrowth[ToothGrowth$dose == 1 & ToothGrowth$supp == "VC", ][1:6, ],
  ToothGrowth[ToothGrowth$dose == 1 & ToothGrowth$supp == "OJ", ][1, ],
  subset(ToothGrowth, dose == 2)
)
sparse$dose <- factor(sparse$dose)

# ---- The helper: precise trigger ----

test_that(".violin_has_singleton_group triggers only on an observed 1-point cell", {
  # sparse: dose=1/OJ has exactly one point -> TRUE
  expect_true(.violin_has_singleton_group(sparse, "dose", color = "supp", y = "len"))
  # balanced ToothGrowth -> FALSE
  expect_false(.violin_has_singleton_group(tg, "dose", color = "supp", y = "len"))
  # legitimately-unbalanced (dose=2 has ZERO OJ, all present cells >= 2) -> FALSE
  d2 <- tg[!(tg$dose == "2" & tg$supp == "OJ"), ]
  expect_false(.violin_has_singleton_group(d2, "dose", color = "supp", y = "len"))
  # no grouping aesthetic (color is a constant, not a column) -> FALSE
  expect_false(.violin_has_singleton_group(tg, "dose", color = "black", y = "len"))
})

# ---- The fix: sparse grouped violin aligns by default ----

test_that("grouped violin with a sparse sub-group aligns by default (no manual args)", {
  centers <- .violin_lane_centers(ggviolin(sparse, "dose", "len", color = "supp", add = "boxplot"))
  # all six dodge lanes present and aligned like a fully balanced design
  expect_equal(centers, c(0.8, 1.2, 1.8, 2.2, 2.8, 3.2))
})

test_that("the auto-fix renders without error", {
  p <- ggviolin(sparse, "dose", "len", color = "supp", add = "boxplot")
  # geom_violin still emits its usual "cannot compute density for < 2 points"
  # message for the single-point sub-group; we only assert it does not ERROR.
  expect_error(
    suppressWarnings(ggplot2::ggplot_gtable(ggplot2::ggplot_build(p))), NA
  )
})

# ---- No trigger: unchanged behavior ----

test_that("balanced and ungrouped violins keep their default positions", {
  expect_equal(.violin_lane_centers(ggviolin(tg, "dose", "len", color = "supp")),
               c(0.8, 1.2, 1.8, 2.2, 2.8, 3.2))
  expect_equal(.violin_lane_centers(ggviolin(tg, "dose", "len")), c(1, 2, 3))
})

test_that("a legitimately-unbalanced design keeps the lone violin centered (ggplot2 default)", {
  d2 <- tg[!(tg$dose == "2" & tg$supp == "OJ"), ]
  centers <- .violin_lane_centers(ggviolin(d2, "dose", "len", color = "supp"))
  # dose=2 has only VC -> its violin stays at the slot center (3.0), NOT the lane (2.8)
  expect_true(3.0 %in% centers)
  expect_false(2.8 %in% centers)
})

# ---- Explicit user arguments turn the auto-handling off ----

test_that("an explicit position= is respected (auto-handling disabled)", {
  centers <- .violin_lane_centers(
    ggviolin(sparse, "dose", "len", color = "supp", add = "boxplot",
             position = position_dodge(0.8))
  )
  # historical behavior: the sparse group is dropped from the dodge -> NOT all six aligned
  expect_false(isTRUE(all.equal(centers, c(0.8, 1.2, 1.8, 2.2, 2.8, 3.2))))
})

test_that("an explicit drop= is respected (auto-handling disabled)", {
  centers <- .violin_lane_centers(
    ggviolin(sparse, "dose", "len", color = "supp", add = "boxplot", drop = TRUE)
  )
  expect_false(isTRUE(all.equal(centers, c(0.8, 1.2, 1.8, 2.2, 2.8, 3.2))))
})
