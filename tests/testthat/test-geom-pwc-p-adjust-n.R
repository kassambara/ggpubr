context("test-geom-pwc-p-adjust-n")

# geom_pwc() / stat_pwc() gain p.adjust.n: an optional number of comparisons for
# the p-value adjustment, passed as `n` to stats::p.adjust (#612). Default NULL
# keeps the standard behavior (adjust by the number of computed p-values), so
# existing output is byte-identical.

df <- ToothGrowth
df$dose <- as.factor(df$dose)

.panel_padj <- function(layer) {
  p <- ggboxplot(df, "dose", "len") + layer
  b <- ggplot2::ggplot_build(p)
  for (d in b$data) {
    if ("p.adj" %in% names(d)) return(sort(unique(d$p.adj)))
  }
  stop("no p.adj found")
}

.raw_p <- sort(rstatix::wilcox_test(df, len ~ dose)$p)

test_that("default p.adjust.n = NULL is unchanged (byte-identical to holm on the computed p-values)", {
  got <- .panel_padj(geom_pwc(method = "wilcox_test", p.adjust.by = "panel"))
  expect_equal(got, sort(stats::p.adjust(.raw_p, method = "holm")))
})

test_that(".pwc_adjust_pvalue with n = NULL equals rstatix::adjust_pvalue", {
  st <- data.frame(group1 = c("a", "a", "b"), group2 = c("b", "c", "c"),
                   p = c(0.01, 0.04, 0.2))
  expect_equal(
    ggpubr:::.pwc_adjust_pvalue(st, "holm", NULL),
    rstatix::adjust_pvalue(st, method = "holm")
  )
})

test_that("p.adjust.n adjusts against the supplied family size", {
  got <- .panel_padj(geom_pwc(method = "wilcox_test", p.adjust.by = "panel", p.adjust.n = 10))
  expect_equal(got, sort(stats::p.adjust(.raw_p, method = "holm", n = 10)))
  # bonferroni is easy to reason about: p.adj = min(1, p * n)
  got_bonf <- .panel_padj(
    geom_pwc(method = "wilcox_test", p.adjust.by = "panel",
             p.adjust.method = "bonferroni", p.adjust.n = 100)
  )
  expect_equal(got_bonf, sort(pmin(1, .raw_p * 100)))
})

test_that("p.adjust.n below the number of p-values is rejected", {
  # 3 dose comparisons; n = 2 is invalid. The stat compute fails.
  expect_error(
    ggpubr:::.pwc_adjust_pvalue(
      data.frame(p = c(0.01, 0.02, 0.03)), "holm", 2
    ),
    "must be >= the number of p-values"
  )
})

test_that("p.adjust.n validates its input type", {
  st <- data.frame(p = c(0.01, 0.02))
  expect_error(ggpubr:::.pwc_adjust_pvalue(st, "holm", c(3, 4)),
               "single positive number")
  expect_error(ggpubr:::.pwc_adjust_pvalue(st, "holm", -1),
               "single positive number")
})
