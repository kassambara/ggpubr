test_that("convert_label_dotdot_notation_to_after_stat handles NULL inputs", {
  # NULL mapping
  expect_null(convert_label_dotdot_notation_to_after_stat(NULL))
  
  # mapping with NULL label (simulate aes without label)
  mapping <- aes(x = var1, y = var2)
  mapping$label <- NULL
  result <- convert_label_dotdot_notation_to_after_stat(mapping)
  expect_equal(result, mapping)
  
  # Empty mapping
  mapping <- aes()
  result <- convert_label_dotdot_notation_to_after_stat(mapping)
  expect_equal(result, mapping)
})


test_that("convert_label_dotdot_notation_to_after_stat converts dot-dot notation", {
  # Single dot-dot variable
  mapping <- aes(label = ..eq.label..)
  result <- convert_label_dotdot_notation_to_after_stat(mapping)
  observed_label <- rlang::as_label(result$label)
  expected_label <-  "ggplot2::after_stat(eq.label)"
  expect_equal(observed_label, expected_label)

  # Multiple dot-dot variables in paste
  mapping <- aes(label = paste(..eq.label.., ..adj.rr.label.., sep = "~~~~"))
  observed_mapping <- convert_label_dotdot_notation_to_after_stat(mapping)
  expected_mapping <- aes(label = paste(ggplot2::after_stat(eq.label), 
                               ggplot2::after_stat(adj.rr.label), sep = "~~~~"))
  expect_equal(observed_mapping$label, expected_mapping$label)

  
  # Statistical test variables
  mapping <- aes(label = ..p.signif..)
  observed_mapping <- convert_label_dotdot_notation_to_after_stat(mapping)
  expected_mapping <- aes(label = ggplot2::after_stat(p.signif))
  expect_equal(observed_mapping$label, expected_mapping$label)

})


test_that("convert_label_dotdot_notation_to_after_stat qualifies unqualified after_stat", {
  # Basic after_stat
  mapping <- aes(label = after_stat(eq.label))
  observed_mapping <- convert_label_dotdot_notation_to_after_stat(mapping)
  expected_mapping <- aes(label = ggplot2::after_stat(eq.label))
  expect_equal(observed_mapping$label, expected_mapping$label)
  
  # after_stat with spaces (using aes_ for flexibility)
  mapping <- aes_(label = parse(text = "after_stat (eq.label)")[[1]])
  observed_mapping <- convert_label_dotdot_notation_to_after_stat(mapping)
  expected_mapping <- aes(label = ggplot2::after_stat(eq.label))
  expect_equal(observed_mapping$label, expected_mapping$label)
  
  # after_stat with multiple spaces
  mapping <- aes_(label = parse(text = "after_stat  (eq.label)")[[1]])
  observed_mapping <- convert_label_dotdot_notation_to_after_stat(mapping)
  expected_mapping <- aes(label = ggplot2::after_stat(eq.label))
  expect_equal(observed_mapping$label, expected_mapping$label)
  
  # Multiple after_stat calls
  mapping <- aes_(label = parse(text = "paste(after_stat(eq.label), after_stat(rr.label))")[[1]])
  observed_mapping <- convert_label_dotdot_notation_to_after_stat(mapping)
  expected_mapping <- aes(label = paste(ggplot2::after_stat(eq.label), ggplot2::after_stat(rr.label)))
  expect_equal(observed_mapping$label, expected_mapping$label)
})




test_that("convert_label_dotdot_notation_to_after_stat preserves qualified after_stat", {
  # Already qualified - should not change
  mapping <- aes(label = ggplot2::after_stat(eq.label))
  observed_mapping <- convert_label_dotdot_notation_to_after_stat(mapping)
  expected_mapping <- aes(label = ggplot2::after_stat(eq.label))
  expect_equal(observed_mapping$label, expected_mapping$label)
  
  # Already qualified with spaces - should not change
  # Note: This test case requires programmatic construction due to spaces
  # In practice, this would be created by the ggplot2 parser
  mapping <- list(label = quote(ggplot2::after_stat(eq.label)))
  observed_mapping <- convert_label_dotdot_notation_to_after_stat(mapping)
  expected_mapping <- list(label = quote(ggplot2::after_stat(eq.label)))
  expect_equal(observed_mapping$label, expected_mapping$label)
  
  # Mixed qualified and unqualified
  mapping <- aes(label = paste(ggplot2::after_stat(eq.label), after_stat(rr.label)))
  observed_mapping <- convert_label_dotdot_notation_to_after_stat(mapping)
  expected_mapping <- aes(label = paste(ggplot2::after_stat(eq.label), ggplot2::after_stat(rr.label)))
  expect_equal(observed_mapping$label, expected_mapping$label)
})

test_that("convert_label_dotdot_notation_to_after_stat handles complex expressions", {
  # Nested function calls
  mapping <- list(label = parse(text = 'format(..p.format.., digits = 3)')[[1]])
  observed_mapping <- convert_label_dotdot_notation_to_after_stat(mapping)
  expected_mapping <- list(label = parse(text = 'format(ggplot2::after_stat(p.format), digits = 3)')[[1]])
  expect_equal(observed_mapping$label, expected_mapping$label)
  
  # Mathematical operations
  mapping <- list(label = parse(text = '..eq.label.. * 2')[[1]])
  observed_mapping <- convert_label_dotdot_notation_to_after_stat(mapping)
  expected_mapping <- list(label = parse(text = 'ggplot2::after_stat(eq.label) * 2')[[1]])
  expect_equal(observed_mapping$label, expected_mapping$label)
  
  # Conditional expressions
  mapping <- list(label = parse(text = 'ifelse(..p.. < 0.05, ..p.signif.., "ns")')[[1]])
  observed_mapping <- convert_label_dotdot_notation_to_after_stat(mapping)
  expected_mapping <- list(label = parse(text = 'ifelse(ggplot2::after_stat(p) < 0.05, ggplot2::after_stat(p.signif), "ns")')[[1]])
  expect_equal(observed_mapping$label, expected_mapping$label)
})

test_that("convert_label_dotdot_notation_to_after_stat preserves other aesthetics", {
  # Multiple aesthetics - only label should be modified
  mapping <- aes(x = var1, y = var2, color = group, label = ..eq.label..)
  observed_mapping <- convert_label_dotdot_notation_to_after_stat(mapping)
  expected_mapping <- aes(x = var1, y = var2, color = group, label = ggplot2::after_stat(eq.label))
  
  # Other aesthetics should be unchanged
  expect_equal(observed_mapping$x, expected_mapping$x)
  expect_equal(observed_mapping$y, expected_mapping$y)
  expect_equal(observed_mapping$colour, expected_mapping$colour)  # Note: aes() converts 'color' to 'colour'
  
  # Only label should be converted
  expect_equal(observed_mapping$label, expected_mapping$label)
})

test_that("convert_label_dotdot_notation_to_after_stat handles edge cases", {
  # String containing after_stat but not as function call (should not change)
  mapping <- list(label = quote("This is after_stat text"))
  observed_mapping <- convert_label_dotdot_notation_to_after_stat(mapping)
  expected_mapping <- list(label = quote("This is after_stat text"))
  expect_equal(observed_mapping$label, expected_mapping$label)
  
  # Variable names containing dot-dot pattern but not exact match
  mapping <- list(label = quote(my..eq.label..var))
  observed_mapping <- convert_label_dotdot_notation_to_after_stat(mapping)
  expected_mapping <- list(label = quote(my..eq.label..var))
  # This should not be converted as it's not an exact ..eq.label.. match
  expect_equal(observed_mapping$label, expected_mapping$label)
  
  # Empty label expression
mapping <- list(label = NULL)
observed_mapping <- convert_label_dotdot_notation_to_after_stat(mapping)
expected_mapping <- list(label = NULL)
expect_equal(observed_mapping$label, expected_mapping$label)
})


test_that("convert_label_dotdot_notation_to_after_stat handles real-world examples", {
  # Example from bSi package
  mapping <- aes(label = after_stat(eq.label))
  observed_mapping <- convert_label_dotdot_notation_to_after_stat(mapping)
  expected_mapping <- aes(label = ggplot2::after_stat(eq.label))
  expect_equal(observed_mapping$label, expected_mapping$label)
  
  # ggpubr documentation example
  mapping <- aes(label = paste(..eq.label.., ..adj.rr.label.., sep = "~~~~"))
  observed_mapping <- convert_label_dotdot_notation_to_after_stat(mapping)
  expected_mapping <- aes(label = paste(ggplot2::after_stat(eq.label), ggplot2::after_stat(adj.rr.label), sep = "~~~~"))
  expect_equal(observed_mapping$label, expected_mapping$label)
  
  # Statistical significance example
  mapping <- aes(label = paste("p =", ..p.format.., ..p.signif..))
  observed_mapping <- convert_label_dotdot_notation_to_after_stat(mapping)
  expected_mapping <- aes(label = paste("p =", ggplot2::after_stat(p.format), ggplot2::after_stat(p.signif)))
  expect_equal(observed_mapping$label, expected_mapping$label)
})


test_that("convert_label_dotdot_notation_to_after_stat correctly uses rlang::as_label", {
  # Test that the function correctly extracts labels as character strings
  # This tests the internal behavior that the function uses rlang::as_label()
  
  # Create a mapping with a complex expression
  mapping <- aes(label = paste(..eq.label.., "R² =", ..adj.rr.label..))
  
  # Extract what rlang::as_label would produce
  label_as_char <- rlang::as_label(mapping$label)
  expected_char <- 'paste(..eq.label.., "R² =", ..adj.rr.label..)'
  expect_equal(label_as_char, expected_char)
  
  # Test that the conversion works correctly
  observed_mapping <- convert_label_dotdot_notation_to_after_stat(mapping)
  expected_mapping <- aes(label = paste(ggplot2::after_stat(eq.label), "R² =", ggplot2::after_stat(adj.rr.label)))
  expect_equal(observed_mapping$label, expected_mapping$label)
  
  # Test with after_stat expression
  mapping2 <- aes(label = after_stat(eq.label))
  label_as_char2 <- rlang::as_label(mapping2$label)
  expected_char2 <- "after_stat(eq.label)"
  expect_equal(label_as_char2, expected_char2)
  
  # Test the conversion
  observed_mapping2 <- convert_label_dotdot_notation_to_after_stat(mapping2)
  expected_mapping2 <- aes(label = ggplot2::after_stat(eq.label))
  expect_equal(observed_mapping2$label, expected_mapping2$label)
})