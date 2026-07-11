context("test-geom-pwc-effsize")

df <- ToothGrowth
df$dose <- as.factor(df$dose)

# Helper: extract the unique non-NA bracket labels from a pwc plot
pwc_labels <- function(p) {
  b <- ggplot2::ggplot_build(p)
  idx <- which(vapply(
    b$data, function(d) all(c("x", "xend", "y", "yend", "label") %in% names(d)),
    logical(1)
  ))
  d <- b$data[[idx[1]]]
  unique(d$label[!is.na(d$label)])
}

test_that("{effsize} token equals rstatix cohens.d for t_test", {
  p <- ggboxplot(df, x = "dose", y = "len") +
    geom_pwc(method = "t_test", label = "d={effsize}")
  labs <- pwc_labels(p)
  truth <- rstatix::t_test(df, len ~ dose, effect.size = TRUE)
  expected <- paste0("d=", round(truth$cohens.d, 2))
  expect_setequal(labs, expected)
})

test_that("{effsize} token maps to the per-method effect-size column", {
  # wilcox_test -> cliff.delta
  lw <- pwc_labels(ggboxplot(df, x = "dose", y = "len") +
    geom_pwc(method = "wilcox_test", label = "d={effsize}"))
  ew <- paste0("d=", round(rstatix::wilcox_test(df, len ~ dose, effect.size = TRUE)$cliff.delta, 2))
  expect_setequal(lw, ew)

  # dunn_test -> r
  ld <- pwc_labels(ggboxplot(df, x = "dose", y = "len") +
    geom_pwc(method = "dunn_test", label = "d={effsize}"))
  ed <- paste0("d=", round(rstatix::dunn_test(df, len ~ dose, effect.size = TRUE)$r, 2))
  expect_setequal(ld, ed)

  # games_howell_test -> cohens.d
  lg <- pwc_labels(ggboxplot(df, x = "dose", y = "len") +
    geom_pwc(method = "games_howell_test", label = "d={effsize}"))
  eg <- paste0("d=", round(rstatix::games_howell_test(df, len ~ dose, effect.size = TRUE)$cohens.d, 2))
  expect_setequal(lg, eg)
})

test_that("{effsize} combines with significance tokens", {
  p <- ggboxplot(df, x = "dose", y = "len") +
    geom_pwc(method = "t_test", label = "{p.adj.signif}, d={effsize}")
  labs <- pwc_labels(p)
  truth <- rstatix::t_test(df, len ~ dose, effect.size = TRUE)
  expected <- paste0(truth$p.adj.signif, ", d=", round(truth$cohens.d, 2))
  expect_setequal(labs, expected)
})

test_that("{effsize} warns and shows NA for a method without effect size", {
  p <- ggboxplot(df, x = "dose", y = "len") +
    geom_pwc(method = "tukey_hsd", label = "d={effsize}")
  expect_warning(labs <- pwc_labels(p), "effsize")
  expect_true(all(labs == "d=NA"))
})

test_that("a label without {effsize} does not request an effect size", {
  # No effect-size column should be computed; label matches the plain p.adj.format
  p <- ggboxplot(df, x = "dose", y = "len") +
    geom_pwc(method = "t_test", label = "{p.adj.format}")
  labs <- pwc_labels(p)
  truth <- rstatix::t_test(df, len ~ dose) %>%
    rstatix::adjust_pvalue(method = "holm")
  # labels are the formatted adjusted p-values (no effect size appended)
  expect_false(any(grepl("d=", labs)))
})
