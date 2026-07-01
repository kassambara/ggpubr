# Regression tests for #200: compare_means() adjusted p-values across ALL groups
# pooled together instead of within each group.by level, so a grouped adjustment
# did not match filtering to one group and adjusting there.

suppressPackageStartupMessages(library(dplyr))

test_that("grouped p.adjust matches filter-then-adjust within a group (#200)", {
  grouped <- compare_means(len ~ dose, ToothGrowth, method = "t.test",
                           p.adjust.method = "bonferroni", group.by = "supp") %>%
    as.data.frame()
  filtered <- ToothGrowth %>% filter(supp == "OJ") %>%
    compare_means(len ~ dose, ., method = "t.test",
                  p.adjust.method = "bonferroni", group.by = "supp") %>%
    as.data.frame()
  oj <- grouped[grouped$supp == "OJ", ]
  oj <- oj[order(oj$group1, oj$group2), ]
  filtered <- filtered[order(filtered$group1, filtered$group2), ]
  expect_equal(oj$p.adj, filtered$p.adj)
})

test_that("grouped bonferroni adjusts by within-group comparison count (#200)", {
  grouped <- compare_means(len ~ dose, ToothGrowth, method = "t.test",
                           p.adjust.method = "bonferroni", group.by = "supp") %>%
    as.data.frame()
  # 3 comparisons per supp group -> p.adj ~= min(1, p * 3), NOT p * 6
  # (tolerance covers the 2-sig-fig rounding but still rules out the old x6)
  expect_equal(grouped$p.adj, pmin(1, grouped$p * 3), tolerance = 0.05)
})

test_that("ungrouped p.adjust is unchanged: adjusts across all comparisons (no regression, #200)", {
  ungrouped <- compare_means(len ~ dose, ToothGrowth, method = "t.test",
                             p.adjust.method = "bonferroni") %>%
    as.data.frame()
  # 3 comparisons total -> p * 3 (same as before this change)
  expect_equal(ungrouped$p.adj, pmin(1, ungrouped$p * 3), tolerance = 0.05)
})

test_that("p.adj stays aligned with p / group1 / group2 rows (#200)", {
  r <- compare_means(len ~ dose, ToothGrowth, method = "t.test",
                     p.adjust.method = "bonferroni", group.by = "supp") %>%
    as.data.frame()
  # p.adj must be >= p for every row (adjustment never decreases a p-value here)
  expect_true(all(r$p.adj >= r$p - 1e-12))
  expect_equal(nrow(r), 6L)
})
