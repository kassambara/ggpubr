# Data preparation
#:::::::::::::::::::::::::::::::::::::::
df <- ToothGrowth
df$dose <- as.factor(df$dose)
# Add a random grouping variable
df$group <- factor(rep(c("grp1", "grp2"), 30))


test_that("ggadjust_pvalue works for geom_pwc() pairwise comparison using facet", {
  p <- ggboxplot(df, x = "supp", y = "len", facet.by = "dose") +
    geom_pwc(method = "t_test")
  # Adjust all p-values together after
  stat_test <- ggadjust_pvalue(
    p, p.adjust.method = "bonferroni",
    label = "{p.adj.format}{p.adj.signif}",
    output = "stat_test"
  )
  stat_test <- stat_test %>%
    dplyr::select(PANEL, x, y, group, group1, group2, label) %>%
    mutate(x = as.numeric(x), label = as.character(label))
  expected <- tibble::tribble(
     ~PANEL, ~x,     ~y, ~group, ~group1, ~group2,     ~label,
        "1",  1, 34.494,      1,     "1",     "2",  "0.0191*",
        "1",  1, 35.385,      1,     "1",     "2",  "0.0191*",
        "1",  2, 35.385,      1,     "1",     "2",  "0.0191*",
        "2",  1, 34.494,      1,     "1",     "2", "0.0031**",
        "2",  1, 35.385,      1,     "1",     "2", "0.0031**",
        "2",  2, 35.385,      1,     "1",     "2", "0.0031**",
        "3",  1, 34.494,      1,     "1",     "2",      "1ns",
        "3",  1, 35.385,      1,     "1",     "2",      "1ns",
        "3",  2, 35.385,      1,     "1",     "2",      "1ns"
     ) %>%
    dplyr::mutate(PANEL = as.factor(PANEL)) %>%
    as.data.frame(stringAsFactor = FALSE)
  expect_equal(stat_test, expected)
})
