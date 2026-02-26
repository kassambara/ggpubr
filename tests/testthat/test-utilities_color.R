test_that(".brewerpal keeps distinct sequential palette names", {
  pals <- .brewerpal()

  expect_true("YlGnBu" %in% pals)
  expect_true("YlOrBr" %in% pals)
  expect_false("YlGnBu YlOrBr" %in% pals)
})
