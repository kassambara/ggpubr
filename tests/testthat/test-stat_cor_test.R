context("test-stat_cor_test")



# Basic plots -----------------------------------
test_that("stat_cor_test rmse label", {
  expect_no_error(get_rmse_label(10))
  expect_no_error({
    a <- get_rmse_label(10, rmse.unit = "d")
    eval(parse(text = a))
  })
})

