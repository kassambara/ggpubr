
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
  # Accept either 0.064 (exact=FALSE) or 0.063 (R-devel exact conditional inference)
  expect_true(results$p.format %in% c("0.064", "0.063"),
              info = paste("Observed p.format =", results$p.format))
  expect_equal(results$.y., "len")
  expect_equal(results$group1, "OJ")
  expect_equal(results$group2, "VC")
  expect_equal(results$p.signif, "ns")
  expect_equal(results$method, "Wilcoxon")
})


test_that("compare_means works for Two-samples paired test", {
  results <- compare_means(len ~ supp, ToothGrowth, paired = TRUE)
  results <- results %>%
    dplyr::select(.y., group1, group2, p.format, p.signif, method)
  # Accept either 0.0043 (exact=FALSE) or 0.0038 (R-devel exact conditional inference)
  expect_true(results$p.format %in% c("0.0043", "0.0038"),
              info = paste("Observed p.format =", results$p.format))
  expect_equal(results$.y., "len")
  expect_equal(results$group1, "OJ")
  expect_equal(results$group2, "VC")
  expect_equal(results$p.signif, "**")
  expect_equal(results$method, "Wilcoxon")
})


test_that("compare_means works for pairwise comparisons", {
  results <- compare_means(len ~ dose, ToothGrowth)
  results <- results %>%
    dplyr::select(.y., group1, group2, p.format, p.signif, method)
  # Accept both exact=FALSE and R-devel exact conditional inference p-values
  legacy_p <- c("7.0e-06", "8.4e-08", "0.00018")
  rdevel_p <- c("7.7e-07", "4.4e-11", "7.6e-05")
  observed_p <- results$p.format
  expect_true(all(observed_p %in% c(legacy_p, rdevel_p)),
              info = paste("Observed p.format =", paste(observed_p, collapse=", ")))
  expect_equal(results$.y., rep("len", 3))
  expect_equal(results$group1, c("0.5", "0.5", "1"))
  expect_equal(results$group2, c("1", "2", "2"))
  expect_true(all(results$p.signif %in% c("***", "****")),
              info = paste("Observed p.signif =", paste(results$p.signif, collapse=", ")))
  expect_equal(results$method, rep("Wilcoxon", 3))
})

test_that("compare_means works for comparison against reference groups", {
  results <- compare_means(len ~ dose, ToothGrowth, ref.group = "0.5")
  results <- results %>%
    dplyr::select(.y., group1, group2, p.format, p.signif, method)
  # Accept both exact=FALSE and R-devel exact conditional inference p-values
  legacy_p <- c("7.0e-06", "8.4e-08")
  rdevel_p <- c("7.7e-07", "4.4e-11")
  observed_p <- results$p.format
  expect_true(all(observed_p %in% c(legacy_p, rdevel_p)),
              info = paste("Observed p.format =", paste(observed_p, collapse=", ")))
  expect_equal(results$.y., rep("len", 2))
  expect_equal(results$group1, c("0.5", "0.5"))
  expect_equal(results$group2, c("1", "2"))
  expect_equal(results$p.signif, rep("****", 2))
  expect_equal(results$method, rep("Wilcoxon", 2))
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


test_that("compare_means works when grouping variable levels contain group2", {
  tibble::tribble(
    ~val,    ~gp,  ~by,
    923 , 'group4','V1',
    1252, 'group4','V1',
    1442, 'group4','V1',
    1398, 'group2','V1',
    1858, 'group2','V1',
    1330, 'group2','V1',
    2593, 'group2','V1',
    23 ,  'group4','V2',
    252,  'group4','V2',
    442,  'group4','V2',
    398,  'group2','V2',
    858,  'group2','V2',
    330,  'group2','V2',
    593,  'group2','V2'
  ) %>% dplyr::mutate(gp=factor(gp, levels = c('group2','group4'))) -> dat

  results <- compare_means(val ~ gp, data = dat, group.by = 'by', paired = F)
  results <- results %>%
    dplyr::select(.y., group1, group2, p.format, p.signif, method)
  # Accept either 0.23 (exact=FALSE) or 0.22 (R-devel exact conditional inference)
  expect_true(all(results$p.format %in% c("0.23", "0.22")),
              info = paste("Observed p.format =", paste(results$p.format, collapse=", ")))
  expect_equal(results$.y., rep("val", 2))
  expect_equal(results$group1, c("group2", "group2"))
  expect_equal(results$group2, c("group4", "group4"))
  expect_equal(results$p.signif, rep("ns", 2))
  expect_equal(results$method, rep("Wilcoxon", 2))
})

