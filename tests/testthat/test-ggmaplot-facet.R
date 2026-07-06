context("test-ggmaplot-facet")

# #498: ggmaplot() gains an opt-in `facet.by`. ggmaplot() rebuilds its own plotting
# data (dropping the user's columns) and selects the top genes / Up-Down counts
# globally, so faceting used to fail ("At least one layer must contain all faceting
# variables"). With facet.by set, the grouping column is carried into the plotted
# layers and the top genes are selected PER panel. Without facet.by, output is
# unchanged.

suppressPackageStartupMessages({
  library(ggplot2)
})

# deterministic 2-group DE-like data
set.seed(42)
mk <- function(sp, shift) data.frame(
  baseMean = 10^runif(600, 0, 5),
  log2FoldChange = rnorm(600, shift, 1.2),
  padj = runif(600)^3,
  gene = paste0(sp, "_g", seq_len(600)),
  species = sp
)
df <- rbind(mk("A", 0), mk("B", 0.4))

.label_layer_index <- function(p) {
  which(vapply(p$layers, function(l) inherits(l$geom, "GeomTextRepel") ||
                inherits(l$geom, "GeomLabelRepel"), logical(1)))[1]
}

test_that("faceted ggmaplot renders at draw time (#498)", {
  p <- ggmaplot(df, fdr = 0.1, fc = 2, top = 8, genenames = df$gene,
                facet.by = "species")
  expect_true(inherits(p$facet, "FacetWrap"))
  # the crash was at draw time, not build time -> render the full gtable
  expect_error(ggplot2::ggplot_gtable(ggplot2::ggplot_build(p)), NA)
})

test_that("top genes are selected PER facet, not globally (#498)", {
  top <- 8
  p <- ggmaplot(df, fdr = 0.1, fc = 2, top = top, genenames = df$gene,
                facet.by = "species")
  li <- .label_layer_index(p)
  ld <- p$layers[[li]]$data
  expect_true("species" %in% names(ld))
  # each panel gets up to `top` labels, and its labels belong to that species
  per <- table(ld$species)
  expect_true(all(per <= top))
  expect_true(all(per > 0))
  expect_true(all(grepl("^A_", ld$gene[ld$species == "A"])))
  expect_true(all(grepl("^B_", ld$gene[ld$species == "B"])))
})

test_that("facet.by naming a non-existent column errors informatively (#498)", {
  expect_error(
    ggmaplot(df, genenames = df$gene, facet.by = "nope"),
    "facet.by column"
  )
})

test_that("facet.by = NULL leaves ggmaplot output unchanged (no-regression, #498)", {
  p <- ggmaplot(df, fdr = 0.1, fc = 2, top = 10, genenames = df$gene)
  # unchanged structure: single vectorised threshold-line layer, counted legend
  b <- suppressWarnings(ggplot2::ggplot_build(p))
  # the hline layer keeps its 3 rows (0 and +/- log2(fc)) as a single layer
  hl <- which(vapply(p$layers, function(l) inherits(l$geom, "GeomHline"), logical(1)))
  expect_length(hl, 1)
  # legend keeps the "Up: n" / "Down: n" counted labels
  labs <- levels(b$plot$data$sig)
  expect_true(any(grepl("^Up: [0-9]", labs)))
  expect_true(any(grepl("^Down: [0-9]", labs)))
  expect_false(inherits(p$facet, "FacetWrap"))
})

test_that("faceted legend uses plain Up/Down/NS labels (no global counts) (#498)", {
  p <- ggmaplot(df, fdr = 0.1, fc = 2, top = 8, genenames = df$gene,
                facet.by = "species")
  labs <- levels(p$data$sig)
  expect_true(all(labs %in% c("Up", "Down", "NS")))
  expect_false(any(grepl(":", labs)))   # no "Up: 123" counts under faceting
})
