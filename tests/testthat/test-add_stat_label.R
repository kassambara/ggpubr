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
  expect_equal(adjusted_p$label, c("1.4e-05", "2.52e-07","0.000177"))
})

test_that("add_stat_label works with glue expression", {
  glueed <- add_stat_label(stat.test, label = "p = {p.format} {p.signif}")
  expect_equal(glueed$label, c("p < 0.0001 ****", "p < 0.0001 ****", "p = 0.00018 ***"))
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
  expect_equal(res.detailed.expression$label,"list(Anova,~italic(F)('2',~'57')~`=`~'29.543',~eta[g]^'2'~`=`~'0.509',~italic(p)~`<`~'0.0001',~italic(n)~`=`~'60')")
  expect_equal(res.plotmath$label, "list(bold(Anova),~italic(F)('2',~'57')~`=`~'29.543',~eta[g]^'2'~`=`~'0.509',~italic(p)~`<`~'0.0001'*`****`,~italic(n)~`=`~'60')")
  expect_equal(res.rounding$label, "list(Anova,~italic(F)('2',~'57')~`=`~'29.5',~eta[g]^'2'~`=`~'0.5',~italic(p)~`<`~'0.0001'*`****`,~italic(n)~`=`~'60')")
  expect_equal(res.customp$label, "list(Anova,~italic(p)~`<`~'.0001'*`****`)")
})





