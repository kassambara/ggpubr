context("test-add_stat_label")

stat.test <- data.frame(
  stringsAsFactors = FALSE,
  .y. = c("y", "y", "y"),
  group1 = c("1", "1", "2"),
  group2 = c("2", "3", "3"),
  n1 = c(20L, 20L, 20L),
  n2 = c(20L, 20L, 20L),
  statistic = c(33.5, 1.5, 61),
  p = c(7.02e-06, 8.41e-08, 0.000177),
  p.adj = c(1.4e-05, 2.52e-07, 0.000177),
  p.adj.signif = c("****", "****", "***"),
  method = c("Wilcoxon test", "Wilcoxon test", "Wilcoxon test"),
  xmin = c(1, 1, 2),
  xmax = c(2, 3, 3),
  p.signif = c("****", "****", "***"),
  p.format = c("<0.0001", "<0.0001", "0.00018"),
  p.adj.format = c("<0.0001", "<0.0001", "0.00018"),
  n = c(40L, 40L, 40L)
)


test_that("add_stat_label works with basic label", {
  formatted_p <- add_stat_label(stat.test, label = "p.format")
  adjusted_p <- add_stat_label(stat.test, label = "p.adj")
  expect_equal(formatted_p$label, c("<0.0001", "<0.0001", "0.00018"))
  expect_equal(adjusted_p$label, c("1.4e-05", "2.52e-07", "0.000177"))
})

test_that("add_stat_label works with glue expression", {
  glueed <- add_stat_label(stat.test, label = "p = {p.format} {p.signif}")
  expect_equal(glueed$label, c("p < 0.0001 ****", "p < 0.0001 ****", "p = 0.00018 ***"))
})

test_that("add_stat_label supports p.format.signif label", {
  combined <- add_stat_label(stat.test, label = "p.format.signif")
  expect_equal(combined$label, c("p < 0.0001 ****", "p < 0.0001 ****", "p = 0.00018 ***"))
})

test_that("add_stat_label falls back to adjusted p-values when raw p is missing", {
  stat.test.adj <- data.frame(
    stringsAsFactors = FALSE,
    p.format = c(NA_character_),
    p.signif = c(NA_character_),
    p.adj.format = c("<0.001"),
    p.adj.signif = c("***")
  )
  combined <- add_stat_label(stat.test.adj, label = "p.format.signif")
  expect_equal(combined$label, "p < 0.001 ***")
})

test_that("add_stat_label works with plotmath: Usage of simple equal '='", {
  simple_equals <- add_stat_label(stat.test, label = "italic(p)={p.format}{p.signif}")
  simple_equals_space <- add_stat_label(stat.test, label = "italic(p) = {p.format}{p.signif}")
  simple_equals_tilde <- add_stat_label(stat.test, label = "italic(p)~=~{p.format}{p.signif}")
  expect_equal(
    simple_equals$label,
    c("list(italic(p)<'0.0001'*`****`)", "list(italic(p)<'0.0001'*`****`)", "list(italic(p)=='0.00018'*`***`)")
  )
  expect_equal(
    simple_equals_space$label,
    c("list(italic(p)~`<`~'0.0001'*`****`)", "list(italic(p)~`<`~'0.0001'*`****`)", "list(italic(p)~`=`~'0.00018'*`***`)")
  )
  expect_equal(
    simple_equals_tilde$label,
    c("list(italic(p)~`<`~'0.0001'*`****`)", "list(italic(p)~`<`~'0.0001'*`****`)", "list(italic(p)~`=`~'0.00018'*`***`)")
  )
})


test_that("add_stat_label works with plotmath: Escaping stars in p.signif/p.adj.signif", {
  # datapasta::dpasta(as.character(res$label))
  stars <- add_stat_label(stat.test, label = "italic(p)={p.format}{p.signif}")
  stars_space <- add_stat_label(stat.test, label = "italic(p)={p.format} {p.signif}")
  stars_tilde <- add_stat_label(stat.test, label = "italic(p)={p.format}~{p.signif}")
  stars_equals <- add_stat_label(stat.test, label = "italic(p)={p.signif}")
  stars_equals_space <- add_stat_label(stat.test, label = "italic(p) = {p.signif}")
  expect_equal(
    stars$label,
    c("list(italic(p)<'0.0001'*`****`)", "list(italic(p)<'0.0001'*`****`)", "list(italic(p)=='0.00018'*`***`)")
  )
  expect_equal(
    stars_space$label,
    c("list(italic(p)<'0.0001'~`****`)", "list(italic(p)<'0.0001'~`****`)", "list(italic(p)=='0.00018'~`***`)")
  )
  expect_equal(
    stars_tilde$label,
    c("list(italic(p)<'0.0001'~`****`)", "list(italic(p)<'0.0001'~`****`)", "list(italic(p)=='0.00018'~`***`)")
  )
  expect_equal(
    stars_equals$label,
    c("list(italic(p)==`****`)", "list(italic(p)==`****`)", "list(italic(p)==`***`)")
  )
  expect_equal(
    stars_equals_space$label,
    c("list(italic(p)~`=`~`****`)", "list(italic(p)~`=`~`****`)", "list(italic(p)~`=`~`***`)")
  )
})


test_that("add_stat_label works with ANOVA stats label formats", {
  res.aov <- data.frame(
    stringsAsFactors = FALSE,
    check.names = FALSE,
    Effect = c("x"),
    DFn = c(2),
    DFd = c(57),
    F = c(29.543),
    p = c(1.57e-09),
    `p<.05` = c("*"),
    ges = c(0.509),
    p.adj = c(1.57e-09),
    p.signif = c("****"),
    p.adj.signif = c("****"),
    p.format = c("<0.0001"),
    p.adj.format = c("<0.0001"),
    n = c(60L),
    method = c("Anova")
  )
  res.italic <- add_stat_label(res.aov, label = get_anova_test_label_template("as_italic"))
  res.detailed <- add_stat_label(res.aov, label = get_anova_test_label_template("as_detailed"))
  res.detailed.italic <- add_stat_label(res.aov, label = get_anova_test_label_template("as_detailed_italic"))
  res.detailed.expression <- add_stat_label(res.aov, label = get_anova_test_label_template("as_detailed_expression"))
  res.plotmath <- add_stat_label(res.aov, label = "bold(Anova), italic(F)({DFn}, {DFd}) = {F}, eta2[g] = {ges}, italic(p) = {p.format}{p.signif}, italic(n) = {n}")
  res.rounding <- add_stat_label(res.aov, label = "Anova, italic(F)({DFn}, {DFd}) = {round(F, 1)}, eta2[g] = {round(ges, 1)}, italic(p) = {p.format}{p.signif}, italic(n) = {n}")
  # Custom p format
  res.customp <- add_stat_label(res.aov, label = "Anova, italic(p) = {rstatix::p_format(p,  accuracy = 0.0001, digits = 3, leading.zero = FALSE)}{p.signif}")

  expect_equal(res.italic$label, "list(Anova,~italic(p)~`<`~'0.0001')")
  expect_equal(res.detailed$label, "Anova, F(2, 57) = 29.543, eta2[g] = 0.509, p < 0.0001, n = 60")
  expect_equal(res.detailed.italic$label, "list(Anova,~italic(F)('2',~'57')~`=`~'29.543',~eta[g]^'2'~`=`~'0.509',~italic(p)~`<`~'0.0001',~italic(n)~`=`~'60')")
  expect_equal(res.detailed.expression$label, "list(Anova,~italic(F)('2',~'57')~`=`~'29.543',~eta[g]^'2'~`=`~'0.509',~italic(p)~`<`~'0.0001',~italic(n)~`=`~'60')")
  expect_equal(res.plotmath$label, "list(bold(Anova),~italic(F)('2',~'57')~`=`~'29.543',~eta[g]^'2'~`=`~'0.509',~italic(p)~`<`~'0.0001'*`****`,~italic(n)~`=`~'60')")
  expect_equal(res.rounding$label, "list(Anova,~italic(F)('2',~'57')~`=`~'29.5',~eta[g]^'2'~`=`~'0.5',~italic(p)~`<`~'0.0001'*`****`,~italic(n)~`=`~'60')")
  expect_equal(res.customp$label, "list(Anova,~italic(p)~`<`~'.0001'*`****`)")
})


test_that("add_stat_label works with Kruskal-Wallis stats label formats", {
  res <- data.frame(
    stringsAsFactors = FALSE,
    x = c("1"),
    y = c(36.4),
    n = c(60L),
    statistic = c(31.21),
    df = c(2L),
    p = c(1.67e-07),
    method = c("Kruskal-Wallis"),
    p.adj = c(1.67e-07),
    p.signif = c("****"),
    p.adj.signif = c("****"),
    p.format = c("<0.0001"),
    p.adj.format = c("<0.0001")
  )
  res.italic <- add_stat_label(res, label = get_kruskal_test_label_template("as_italic"))
  res.detailed <- add_stat_label(res, label = get_kruskal_test_label_template("as_detailed"))
  res.detailed.italic <- add_stat_label(res, label = get_kruskal_test_label_template("as_detailed_italic"))
  res.detailed.expression <- add_stat_label(res, label = get_kruskal_test_label_template("as_detailed_expression"))

  expect_equal(res.italic$label, "list('Kruskal-Wallis',~italic(p)~`<`~'0.0001')")
  expect_equal(res.detailed$label, "Kruskal-Wallis, X2(2) = 31.21, p < 0.0001, n = 60")
  expect_equal(res.detailed.italic$label, "list('Kruskal-Wallis',~italic(chi)^'2'~('2')~`=`~'31.21',~italic(p)~`<`~'0.0001',~italic(n)~`=`~'60')")
  expect_equal(res.detailed.expression$label, "list('Kruskal-Wallis',~italic(chi)^'2'('2')~`=`~'31.21',~italic(p)~`<`~'0.0001',~italic(n)~`=`~'60')")
})


# Tests for build_symnum_args helper function
test_that("build_symnum_args uses symnum.args when provided (backward compatibility)", {
  result <- build_symnum_args(
    symnum.args = list(cutpoints = c(0, 0.01, 0.05, 1), symbols = c("**", "*", "ns"))
  )
  expect_equal(result$cutpoints, c(0, 0.01, 0.05, 1))
  expect_equal(result$symbols, c("**", "*", "ns"))
})

test_that("build_symnum_args builds from signif.cutoffs (3 levels)", {
  result <- build_symnum_args(
    signif.cutoffs = c(0.10, 0.05, 0.01)
  )
  # Cutoffs should be sorted ascending and wrapped with 0 and Inf
  expect_equal(result$cutpoints, c(0, 0.01, 0.05, 0.10, Inf))
  # Symbols should be: most significant first, then less, then ns
  expect_equal(result$symbols, c("***", "**", "*", "ns"))
})

test_that("build_symnum_args builds from signif.cutoffs (4 levels with use.four.stars)", {
  result <- build_symnum_args(
    signif.cutoffs = c(0.10, 0.05, 0.01, 0.001),
    use.four.stars = TRUE
  )
  expect_equal(result$cutpoints, c(0, 0.001, 0.01, 0.05, 0.10, Inf))
  expect_equal(result$symbols, c("****", "***", "**", "*", "ns"))
})

test_that("build_symnum_args uses custom signif.symbols", {
  result <- build_symnum_args(
    signif.cutoffs = c(0.10, 0.05, 0.01),
    signif.symbols = c("+", "++", "+++")
  )
  expect_equal(result$symbols, c("+++", "++", "+", "ns"))
})

test_that("build_symnum_args uses custom ns.symbol", {
  result <- build_symnum_args(
    signif.cutoffs = c(0.10, 0.05, 0.01),
    ns.symbol = "N.S."
  )
  expect_equal(result$symbols[4], "N.S.")
})

test_that("build_symnum_args uses empty ns.symbol", {
  result <- build_symnum_args(
    signif.cutoffs = c(0.10, 0.05, 0.01),
    ns.symbol = ""
  )
  expect_equal(result$symbols[4], "")
})

test_that("build_symnum_args errors when signif.cutoffs has 4+ levels without use.four.stars", {
  expect_error(
    build_symnum_args(signif.cutoffs = c(0.10, 0.05, 0.01, 0.001)),
    "use.four.stars"
  )
})

test_that("build_symnum_args errors when signif.symbols length doesn't match", {
  expect_error(
    build_symnum_args(
      signif.cutoffs = c(0.10, 0.05, 0.01),
      signif.symbols = c("+", "++") # Only 2, but need 3
    ),
    "same length"
  )
})

test_that("build_symnum_args uses defaults when nothing provided", {
  result <- build_symnum_args()
  # Should use package defaults
  expect_equal(result$cutpoints, c(0, 1e-04, 0.001, 0.01, 0.05, Inf))
  expect_equal(result$symbols, c("****", "***", "**", "*", "ns"))
})
