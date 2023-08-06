context("test-as_npc")

test_that("as_npc returns the same value as the input when the input is.numeric", {
  expect_equal(as_npc(0.5), 0.5)
})


test_that("as_npc works correctly for character coordinates", {
  res <- as_npc(c('right', 'left', 'bottom', 'top', 'center', 'centre', 'middle'))
  expect_equal(res, c(0.95, 0.05, 0.05, 0.95, 0.5, 0.5, 0.5))
})
