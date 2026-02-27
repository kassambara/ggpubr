test_that(".brewerpal keeps distinct sequential palette names", {
  pals <- .brewerpal()

  expect_true("YlGnBu" %in% pals)
  expect_true("YlOrBr" %in% pals)
  expect_false("YlGnBu YlOrBr" %in% pals)
})

test_that(".get_brewer_pal supports YlOrBr", {
  skip_if_not_installed("RColorBrewer")
  cols <- .get_brewer_pal("YlOrBr", 5)
  expect_length(cols, 5)
  expect_type(cols, "character")
})

test_that(".get_pal routes YlOrBr to brewer palettes", {
  skip_if_not_installed("RColorBrewer")
  cols <- .get_pal("YlOrBr", 5)
  expect_length(cols, 5)
  expect_type(cols, "character")
})
