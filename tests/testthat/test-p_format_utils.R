test_that("format_p_value returns correct format for default style", {
  # Default style should use format.pval with 2 significant digits
  expect_equal(format_p_value(0.001234), "0.0012")
  expect_equal(format_p_value(0.000001234), "1.2e-06")
  expect_equal(format_p_value(0.05), "0.05")
  expect_equal(format_p_value(0.5), "0.5")
})

test_that("format_p_value returns correct format for APA style", {
  # APA: 3 decimals, no leading zero, < .001 threshold
  p <- format_p_value(0.0456, style = "apa")
  expect_true(grepl("046", p))
  expect_false(grepl("^0\\.", p))

  # Very small p-values should use < .001
  p_small <- format_p_value(0.0001, style = "apa")
  expect_true(grepl("<", p_small))
  expect_true(grepl("\\.001", p_small))
})

test_that("format_p_value returns correct format for NEJM style", {
  # NEJM: 3 decimals, leading zero, < 0.001 threshold
  p <- format_p_value(0.0456, style = "nejm")
  expect_true(grepl("0\\.046", p))

  # Very small p-values should use < 0.001
  p_small <- format_p_value(0.0001, style = "nejm")
  expect_true(grepl("<", p_small))
  expect_true(grepl("0\\.001", p_small))
})

test_that("format_p_value returns correct format for Lancet style", {
  # Lancet: 4 decimals, leading zero
  p <- format_p_value(0.04567, style = "lancet")
  expect_true(grepl("0\\.0457", p))
})

test_that("format_p_value respects custom digits parameter", {
  # Custom digits should override style defaults
  # Using a non-scientific style to test digits more clearly
  p <- format_p_value(0.04567, style = "nejm", digits = 4)
  expect_true(grepl("0457", p))
})

test_that("format_p_value respects custom leading.zero parameter", {
  # Override leading zero setting - use non-scientific style
  p_no_zero <- format_p_value(0.05, style = "nejm", leading.zero = FALSE)
  expect_false(grepl("^0\\.", p_no_zero))
  expect_true(grepl("^\\.05", p_no_zero))

  p_with_zero <- format_p_value(0.05, style = "apa", leading.zero = TRUE)
  expect_true(grepl("^0\\.", p_with_zero))
})

test_that("format_p_value respects custom min.threshold parameter", {
  # Custom minimum threshold with non-scientific style
  p <- format_p_value(0.00001, style = "nejm", min.threshold = 0.0001)
  expect_true(grepl("<", p))
  expect_true(grepl("0\\.0001", p))
})

test_that("format_p_value respects decimal.mark parameter", {
  p <- format_p_value(0.0047, style = "nejm", decimal.mark = ",")
  expect_true(grepl(",", p))
  expect_false(grepl("\\.", p))
})

test_that("get_p_format_style returns correct style parameters", {
  apa <- get_p_format_style("apa")
  expect_equal(apa$digits, 3)
  expect_false(apa$leading.zero)
  expect_equal(apa$min.threshold, 0.001)

  nejm <- get_p_format_style("nejm")
  expect_equal(nejm$digits, 3)
  expect_true(nejm$leading.zero)
  expect_equal(nejm$min.threshold, 0.001)

  lancet <- get_p_format_style("lancet")
  expect_equal(lancet$digits, 4)
  expect_true(lancet$leading.zero)
  expect_equal(lancet$min.threshold, 0.0001)
})

test_that("list_p_format_styles returns all available styles", {
  styles <- list_p_format_styles()
  expect_true(is.data.frame(styles))
  expect_true("default" %in% styles$style)
  expect_true("apa" %in% styles$style)
  expect_true("nejm" %in% styles$style)
  expect_true("lancet" %in% styles$style)
  expect_true("ama" %in% styles$style)
  expect_true("graphpad" %in% styles$style)
  expect_true("scientific" %in% styles$style)
})

test_that("format_p_value handles NA values", {
  expect_true(is.na(format_p_value(NA)))
})

test_that("format_p_value handles vector input", {
  p_values <- c(0.001, 0.05, 0.5)
  results <- format_p_value(p_values)
  expect_length(results, 3)
})

test_that("format_p_value returns scientific style correctly", {
  # Scientific style: uses format.pval which may use scientific notation for very small values
  p <- format_p_value(0.0000001234, style = "scientific")
  expect_true(grepl("e-", p))
})

test_that("format_p_value for GraphPad style", {
  # GraphPad: 4 decimals, leading zero, < 0.0001 threshold
  p <- format_p_value(0.04567, style = "graphpad")
  expect_true(grepl("0\\.0457", p))
})

test_that("format_p_value handles edge cases", {
  # P-value of exactly 1
  expect_equal(format_p_value(1), "1")

  # Very small p-value with scientific style
  p_tiny <- format_p_value(1e-20, style = "scientific")
  expect_true(grepl("e-", p_tiny) || grepl("<", p_tiny))

  # P-value of exactly 0 (edge case)
  p_zero <- format_p_value(0)
  expect_true(grepl("0", p_zero) || grepl("<", p_zero))
})

test_that("format_p_value leading.zero works with scientific styles", {
  # Test leading zero removal with default style
  p <- format_p_value(0.05, style = "default", leading.zero = FALSE)
  expect_true(grepl("^\\.05", p))
})

test_that("format_p_value min.threshold works with scientific styles", {
  # Test min.threshold with default style
  p <- format_p_value(0.00001, style = "default", min.threshold = 0.0001)
  expect_true(grepl("<", p))
})
