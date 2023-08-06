context("test-create_aes")

test_that("parse_expression works for a valid variable name", {
  observed <- parse_expression("a")
  expected <- parse(text = "a")[[1]]
  expect_equal(observed, expected)
})


test_that("parse_expression works for a variable name containing space", {
  observed <- parse_expression("a b")
  expected <- as.name("a b")
  expect_equal(observed, expected)
})

test_that("parse_expression works for a mathetical expression", {
  observed <- parse_expression("log2( a + b )")
  expected <- parse(text = "log2( a + b )")[[1]]
  expect_equal(observed, expected)
})

