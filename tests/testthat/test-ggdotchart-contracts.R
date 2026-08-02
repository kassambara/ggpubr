test_that("ggdotchart add has one exact documented vocabulary", {
  d <- data.frame(item = c("a", "b"), value = c(1, 2))
  layer_counts <- vapply(c("none", "segment"), function(add) {
    length(ggdotchart(d, "item", "value", add = add)$layers)
  }, integer(1))
  partial_error <- tryCatch(
    ggdotchart(d, "item", "value", add = "segments"),
    error = conditionMessage
  )
  expect_identical(
    list(
      counts = unname(layer_counts),
      rejected_partial = grepl("exactly one of", partial_error, fixed = TRUE)
    ),
    list(counts = c(1L, 2L), rejected_partial = TRUE)
  )
})

test_that("ggdotchart omitted sorting is descending as documented", {
  d <- data.frame(item = c("low", "high", "mid"), value = c(1, 3, 2))
  p <- ggdotchart(d, "item", "value")
  expect_identical(levels(p$data$item), c("high", "mid", "low"))
})

test_that("ggdotchart colors axis text from points and honors its TRUE default", {
  d <- data.frame(
    item = c("a", "b", "c", "d"), value = 1:4,
    grp = c("one", "two", "one", "two")
  )
  make <- function(...) ggdotchart(
    d, "item", "value", color = "grp", add = "segment",
    sorting = "none", ...
  )
  omitted <- make()
  explicit <- make(x.text.col = TRUE)
  disabled <- make(x.text.col = FALSE)
  observed <- list(
    omitted = omitted$theme$axis.text.x$colour,
    explicit = explicit$theme$axis.text.x$colour,
    disabled = disabled$theme$axis.text.x$colour
  )
  expect_identical(
    observed,
    list(
      omitted = c(a = "#F8766D", b = "#00BFC4", c = "#F8766D", d = "#00BFC4"),
      explicit = c(a = "#F8766D", b = "#00BFC4", c = "#F8766D", d = "#00BFC4"),
      disabled = NULL
    )
  )
})
