context("test-de-top-gene-ranking")

# Extract the labeled gene names (the ggrepel layer's data).
labelled <- function(p) {
  i <- which(vapply(p$layers, function(l) grepl("Repel", class(l$geom)[1]), logical(1)))
  as.character(p$layers[[i[1]]]$data$name)
}

# ---- .select_top_de_labels() port fidelity ---------------------------------
test_that(".select_top_de_labels ranks by rank-sum of |lfc| and padj", {
  # g1: biggest effect but weakest p; g4: strongest p but smallest effect; g2/g3
  # are moderate on both. Rank-sum should favor the both-strong genes.
  df <- data.frame(
    name = c("g1", "g2", "g3", "g4"),
    lfc = c(5.0, 3.0, 2.5, 1.1),
    padj = c(1e-3, 1e-8, 1e-7, 1e-30)
  )
  out <- ggpubr:::.select_top_de_labels(df, top = 2, method = "rank.sum")
  # rank-sum scores: g1 fc1+padj4=5; g2 fc2+padj2=4; g3 fc3+padj3=6; g4 fc4+padj1=5
  # -> g2 (4) best, then g1/g4 tie at 5 broken by larger |lfc| -> g1
  expect_equal(out$name, c("g2", "g1"))
})

test_that(".select_top_de_labels balances direction with spillover", {
  df <- data.frame(
    name = c("u1", "u2", "u3", "d1"),
    lfc = c(2, 3, 4, -2),
    padj = c(1e-4, 1e-5, 1e-6, 1e-4)
  )
  # top = 4, only 1 down gene -> 3 up + 1 down (spillover into up)
  out <- ggpubr:::.select_top_de_labels(df, top = 4, method = "padj", balanced = TRUE)
  expect_equal(sum(out$lfc > 0), 3)
  expect_equal(sum(out$lfc < 0), 1)
})

test_that(".select_top_de_labels selects within each facet group", {
  df <- data.frame(
    name = c("a1", "a2", "b1", "b2"),
    lfc = c(3, 2, 3, 2),
    padj = c(1e-5, 1e-4, 1e-5, 1e-4),
    grp = c("A", "A", "B", "B")
  )
  out <- ggpubr:::.select_top_de_labels(df, top = 1, method = "padj", facet.by = "grp")
  expect_equal(nrow(out), 2) # one per group
  expect_setequal(out$grp, c("A", "B"))
})

# ---- integration in ggmaplot / ggvolcano -----------------------------------
de <- data.frame(
  name = paste0("g", 1:6),
  baseMean = 100,
  log2FoldChange = c(6, 4, 3, -5, -4, 1.2),
  padj = c(1e-2, 1e-20, 1e-10, 1e-3, 1e-15, 1e-40)
)

test_that("select.top.method = 'rank.sum' changes the labeled set", {
  p_padj <- ggmaplot(de, fdr = 0.05, fc = 2, genenames = de$name, top = 3)
  p_rs <- ggmaplot(de, fdr = 0.05, fc = 2, genenames = de$name, top = 3,
    select.top.method = "rank.sum"
  )
  expect_false(identical(sort(labelled(p_padj)), sort(labelled(p_rs))))
  expect_silent(ggplot2::ggplotGrob(p_rs))
})

test_that("top.balanced labels both directions in ggmaplot and ggvolcano", {
  pm <- ggmaplot(de, fdr = 0.05, fc = 2, genenames = de$name, top = 4,
    select.top.method = "rank.sum", top.balanced = TRUE
  )
  lm <- pm$layers[[which(vapply(pm$layers, function(l) grepl("Repel", class(l$geom)[1]), logical(1)))[1]]]$data
  expect_true(any(lm$lfc > 0) && any(lm$lfc < 0))

  pv <- ggvolcano(de, fdr = 0.05, fc = 2, genenames = de$name, top = 4,
    select.top.method = "rank.sum", top.balanced = TRUE
  )
  expect_silent(ggplot2::ggplotGrob(pv))
})

test_that("the default selection is unchanged when the new options are not used", {
  # padj-ranked head(top) still labels the smallest-padj significant genes.
  p <- ggvolcano(de, fdr = 0.05, fc = 2, genenames = de$name, top = 2)
  # g6 (padj 1e-40) and g2 (1e-20) are the two smallest-padj significant hits
  expect_setequal(labelled(p), c("g6", "g2"))
})
