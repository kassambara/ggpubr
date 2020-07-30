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
    c("list(italic(p)<0.0001*`****`)", "list(italic(p)<0.0001*`****`)", "list(italic(p)==0.00018*`***`)")
    )
  expect_equal(
    simple_equals_space$label,
    c("list(italic(p)~`<`~0.0001*`****`)", "list(italic(p)~`<`~0.0001*`****`)", "list(italic(p)~`=`~0.00018*`***`)")
  )
  expect_equal(
    simple_equals_tilde$label,
    c("list(italic(p)~`<`~0.0001*`****`)", "list(italic(p)~`<`~0.0001*`****`)", "list(italic(p)~`=`~0.00018*`***`)")
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
    c("list(italic(p)<0.0001*`****`)", "list(italic(p)<0.0001*`****`)", "list(italic(p)==0.00018*`***`)")
  )
  expect_equal(
    stars_space$label,
    c("list(italic(p)<0.0001~`****`)", "list(italic(p)<0.0001~`****`)", "list(italic(p)==0.00018~`***`)")
  )
  expect_equal(
    stars_tilde$label,
    c("list(italic(p)<0.0001~`****`)", "list(italic(p)<0.0001~`****`)", "list(italic(p)==0.00018~`***`)")
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






