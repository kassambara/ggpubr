
data("gene_expression", package = "ggpubr")
data("ToothGrowth")


test_that("compare_means works for One-sample test", {
  results <- compare_means(len ~ 1, ToothGrowth, mu = 0)
  results <- results %>%
    dplyr::select(.y., group1, group2, p.format, p.signif, method)
  expected <- tibble::tribble(
      ~.y., ~group1,      ~group2, ~p.format, ~p.signif,    ~method,
     "len",       1, "null model", "1.7e-11",    "****", "Wilcoxon"
     )
  expect_equal(results, expected)
})


test_that("compare_means works for Two-samples unpaired test", {
  results <- compare_means(len ~ supp, ToothGrowth)
  results <- results %>%
    dplyr::select(.y., group1, group2, p.format, p.signif, method)
  expected <- tibble::tribble(
      ~.y., ~group1, ~group2, ~p.format, ~p.signif,    ~method,
     "len",    "OJ",    "VC",   "0.064",      "ns", "Wilcoxon"
     )
  expect_equal(results, expected)
})


test_that("compare_means works for Two-samples paired test", {
  results <- compare_means(len ~ supp, ToothGrowth, paired = TRUE)
  results <- results %>%
    dplyr::select(.y., group1, group2, p.format, p.signif, method)
  expected <- tibble::tribble(
      ~.y., ~group1, ~group2, ~p.format, ~p.signif,    ~method,
     "len",    "OJ",    "VC",  "0.0043",      "**", "Wilcoxon"
     )
  expect_equal(results, expected)
})


test_that("compare_means works for pairwise comparisons", {
  results <- compare_means(len ~ dose, ToothGrowth)
  results <- results %>%
    dplyr::select(.y., group1, group2, p.format, p.signif, method)
  expected <- tibble::tribble(
      ~.y., ~group1, ~group2, ~p.format, ~p.signif,    ~method,
     "len",   "0.5",     "1", "7.0e-06",    "****", "Wilcoxon",
     "len",   "0.5",     "2", "8.4e-08",    "****", "Wilcoxon",
     "len",     "1",     "2", "0.00018",     "***", "Wilcoxon"
     )
  expect_equal(results, expected)
})

test_that("compare_means works for comparison against reference groups", {
  results <- compare_means(len ~ dose, ToothGrowth, ref.group = "0.5")
  results <- results %>%
    dplyr::select(.y., group1, group2, p.format, p.signif, method)
  expected <- tibble::tribble(
      ~.y., ~group1, ~group2, ~p.format, ~p.signif,    ~method,
     "len",   "0.5",     "1", "7.0e-06",    "****", "Wilcoxon",
     "len",   "0.5",     "2", "8.4e-08",    "****", "Wilcoxon"
     )
  expect_equal(results, expected)
})



test_that("compare_means works when using formula with multiple variables", {
  results <- compare_means(c(GATA3, XBP1, PTEN) ~ dataset, data = gene_expression)
  results <- results %>%
    dplyr::select(.y., group1, group2, p.format, p.signif, method)
  expected <- tibble::tribble(
        ~.y., ~group1, ~group2, ~p.format, ~p.signif,    ~method,
     "GATA3",  "BRCA",    "OV", "< 2e-16",    "****", "Wilcoxon",
     "GATA3",  "BRCA",  "LUSC", "< 2e-16",    "****", "Wilcoxon",
     "GATA3",    "OV",  "LUSC", "3.0e-08",    "****", "Wilcoxon",
      "XBP1",  "BRCA",    "OV", "< 2e-16",    "****", "Wilcoxon",
      "XBP1",  "BRCA",  "LUSC", "< 2e-16",    "****", "Wilcoxon",
      "XBP1",    "OV",  "LUSC", "4.2e-11",    "****", "Wilcoxon",
      "PTEN",  "BRCA",    "OV", "6.8e-05",    "****", "Wilcoxon",
      "PTEN",  "BRCA",  "LUSC", "< 2e-16",    "****", "Wilcoxon",
      "PTEN",    "OV",  "LUSC", "1.3e-07",    "****", "Wilcoxon"
     ) %>%
    dplyr::mutate(.y. = factor(.y., levels = c("GATA3", "XBP1", "PTEN")))
  expect_equal(results, expected)
})


test_that("compare_means works when using ref.group = '.all.'", {
  results <- compare_means(len ~ dose, ToothGrowth, ref.group = ".all.")
  results <- results %>%
    dplyr::select(.y., group1, group2, p.format, p.signif, method)
  expected <- tibble::tribble(
      ~.y., ~group1, ~group2, ~p.format, ~p.signif,    ~method,
     "len", ".all.",   "0.5", "5.1e-05",    "****", "Wilcoxon",
     "len", ".all.",     "1", "0.76404",      "ns", "Wilcoxon",
     "len", ".all.",     "2", "0.00018",     "***", "Wilcoxon"
     )
  expect_equal(results, expected)
})


test_that("compare_means works for Anova", {
  results <- compare_means(len ~ dose, ToothGrowth, method = "anova")
  results <- results %>%
    dplyr::select(.y., p.format, p.signif, method)
  expected <- tibble::tribble(
      ~.y., ~p.format, ~p.signif, ~method,
     "len", "9.5e-16",    "****", "Anova"
     )
  expect_equal(results, expected)
})


test_that("compare_means works for kruskal.test", {
  results <- compare_means(len ~ dose, ToothGrowth, method = "kruskal.test")
  results <- results %>%
    dplyr::select(.y., p.format, p.signif, method)
  expected <- tibble::tribble(
      ~.y., ~p.format, ~p.signif,          ~method,
     "len", "1.5e-09",    "****", "Kruskal-Wallis"
     )
  expect_equal(results, expected)
})

