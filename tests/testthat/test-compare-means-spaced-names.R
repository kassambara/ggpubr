# Regression tests for #385: compare_means() errored ("Can't extract columns that
# don't exist") when a formula variable name contains a space, because
# term.labels wraps such names in backticks, which don't match a data column in
# dplyr / `[[`.

.df <- ToothGrowth
.df[["spa ced"]] <- .df$supp
.df$dose <- factor(.df$dose)

test_that("compare_means() works with a spaced group name (#385)", {
  expect_no_error(res <- compare_means(len ~ `spa ced`, data = .df))
  # same result as the equivalent unspaced column (spaced is a copy of supp)
  ref <- compare_means(len ~ supp, data = ToothGrowth)
  expect_equal(res$p, ref$p)
  expect_equal(as.character(res$group1), as.character(ref$group1))
  expect_equal(as.character(res$group2), as.character(ref$group2))
})

test_that("spaced group name works for anova and with group.by (#385)", {
  expect_no_error(compare_means(len ~ `spa ced`, data = .df, method = "anova"))
  expect_no_error(compare_means(len ~ `spa ced`, data = .df, group.by = "dose"))
})

test_that("normal (unspaced) formulas are unchanged (no regression, #385)", {
  a <- compare_means(len ~ supp, data = ToothGrowth)
  expect_equal(a$method, "Wilcoxon")
  expect_equal(nrow(a), 1L)
  # multi-group + group.by path still works
  expect_no_error(compare_means(len ~ supp, data = ToothGrowth, group.by = "dose"))
})

test_that(".strip_backticks removes backticks only (#385)", {
  expect_equal(ggpubr:::.strip_backticks("`spa ced`"), "spa ced")
  expect_equal(ggpubr:::.strip_backticks("supp"), "supp")
})
