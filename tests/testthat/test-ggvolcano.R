context("test-ggvolcano")

# Synthetic DE results with one clear case of each class.
d <- data.frame(
  name = c("up", "down", "ns_p", "ns_fc"),
  log2FoldChange = c(2, -2, 2, 0.1),
  padj = c(0.001, 0.001, 0.5, 0.001),
  stringsAsFactors = FALSE
)

sig_of <- function(p, gene) {
  as.character(p$data$sig[match(gene, p$data$name)])
}

test_that("ggvolcano classifies Up / Down / NS by fold change and significance", {
  p <- ggvolcano(d, fdr = 0.05, fc = 2, genenames = d$name, top = 0)
  expect_s3_class(p, "ggplot")
  expect_true(grepl("Up", sig_of(p, "up")))
  expect_true(grepl("Down", sig_of(p, "down")))
  expect_equal(sig_of(p, "ns_p"), "NS") # significant fc but p > fdr
  expect_equal(sig_of(p, "ns_fc"), "NS") # significant p but |lfc| < log2(fc)
})

test_that("ggvolcano plots -log10(p) on y and caps p == 0 to a finite value", {
  d0 <- data.frame(
    log2FoldChange = c(-3, 3, 0.1),
    padj = c(0, 1e-50, 0.5)
  )
  p <- ggvolcano(d0, top = 0)
  yb <- ggplot2::ggplot_build(p)$data[[1]]
  expect_true(all(is.finite(yb$y)))
  # the p == 0 point is capped at the largest finite -log10(p) = -log10(1e-50)
  expect_equal(max(yb$y), 50)
})

test_that("ggvolcano draws points, repel labels, and threshold lines", {
  p <- ggvolcano(d, fdr = 0.05, fc = 2, genenames = d$name)
  geoms <- vapply(p$layers, function(l) class(l$geom)[1], character(1))
  expect_true("GeomPoint" %in% geoms)
  expect_true("GeomVline" %in% geoms) # vertical fold-change lines
  expect_true("GeomHline" %in% geoms) # horizontal significance line
  expect_true(any(grepl("Repel", geoms))) # ggrepel labels
  expect_silent(ggplot2::ggplotGrob(p))
})

test_that("ggvolcano drops rows with a missing fold change or p-value", {
  dna <- rbind(d, data.frame(name = "na", log2FoldChange = NA, padj = NA))
  # No 'Removed rows' warning, and the NA gene is not in the plotted data
  p <- ggvolcano(dna, genenames = dna$name, top = 0)
  expect_false("na" %in% p$data$name)
  expect_silent(ggplot2::ggplotGrob(p))
})

test_that("ggvolcano labels only the selected genes with top = 0", {
  p <- ggvolcano(d, fdr = 0.05, fc = 2, genenames = d$name,
    top = 0, label.select = c("up")
  )
  # the repel layer's data holds the labelled genes
  repel.idx <- which(vapply(p$layers, function(l) grepl("Repel", class(l$geom)[1]), logical(1)))
  labelled <- p$layers[[repel.idx]]$data$name
  expect_equal(labelled, "up")
})

test_that("ggvolcano supports custom fold-change / p-value column names", {
  d2 <- data.frame(logFC = c(2, -2), adj.P.Val = c(0.001, 0.001), gene = c("a", "b"))
  p <- ggvolcano(d2, x = "logFC", y = "adj.P.Val", genenames = d2$gene, top = 0)
  expect_s3_class(p, "ggplot")
  expect_true(grepl("Up", sig_of(p, "a")))
  expect_true(grepl("Down", sig_of(p, "b")))
})

test_that("ggvolcano supports faceting", {
  d3 <- d
  d3$grp <- c("A", "A", "B", "B")
  p <- ggvolcano(d3, fdr = 0.05, fc = 2, genenames = d3$name, facet.by = "grp", top = 2)
  expect_silent(ggplot2::ggplotGrob(p))
})

test_that("ggvolcano errors when the required columns are missing", {
  expect_error(ggvolcano(data.frame(a = 1, b = 2)), "must contain")
})
