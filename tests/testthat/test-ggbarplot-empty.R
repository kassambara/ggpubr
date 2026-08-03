test_that("ggbarplot handles zero-row data without a max warning", {
  d <- data.frame(group = character(), value = numeric())
  expect_silent(ggbarplot(d, "group", "value"))
})
