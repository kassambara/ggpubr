test_that("test labels accept legal non-syntactic column names", {
  d <- ToothGrowth
  d$dose <- factor(d$dose)
  names(d)[names(d) == "dose"] <- "treatment group"
  names(d)[names(d) == "len"] <- "outcome value"
  expect_silent({
    p <- ggboxplot(d, "treatment group", "outcome value")
    add_test_label(p, method = "anova")
    ggcompare(
      d, "treatment group", "outcome value",
      add = "none", omnibus = "anova"
    )
  })
})
