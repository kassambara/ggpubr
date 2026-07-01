test_that(".create_labeller honors named panel.labs, else maps positionally (#643)", {
  df <- ToothGrowth
  df$dose <- factor(df$dose)  # levels "0.5","1","2"

  # Named labels covering all levels (given in a different order) are matched by name
  lab <- .create_labeller(df, list(dose = c("2" = "High", "0.5" = "Low", "1" = "Mid")))
  res <- lab(data.frame(dose = factor(c("0.5", "1", "2"))))$dose
  expect_equal(res, c("Low", "Mid", "High"))

  # No-regression: UNnamed labels still map positionally
  lab2 <- .create_labeller(df, list(dose = c("Low", "Mid", "High")))
  res2 <- lab2(data.frame(dose = factor(c("0.5", "1", "2"))))$dose
  expect_equal(res2, c("Low", "Mid", "High"))

  # No-regression: names that do NOT cover the data levels fall back to positional
  # (same as the previous behavior, which discarded the names)
  lab3 <- .create_labeller(df, list(dose = c(a = "Low", b = "Mid", c = "High")))
  res3 <- lab3(data.frame(dose = factor(c("0.5", "1", "2"))))$dose
  expect_equal(res3, c("Low", "Mid", "High"))
})
