test_that("faceted interaction labels keep carriage-return tuples distinct", {
  d <- expand.grid(
    panel = 1:2, x = c("A", "B"), group = c("U", "V"), replicate = 1:5,
    KEEP.OUT.ATTRS = FALSE, stringsAsFactors = FALSE
  )
  d$facet1 <- ifelse(d$panel == 1, "a\rb", "a")
  d$facet2 <- ifelse(d$panel == 1, "c", "b\rc")
  d$y <- d$replicate + ifelse(d$x == "B", 2, 0) +
    ifelse(d$group == "V", 1, 0) + ifelse(d$x == "B" & d$group == "V", 3, 0)
  p <- ggplot2::ggplot(d, ggplot2::aes(x = x, y = y)) +
    ggplot2::facet_grid(facet1 ~ facet2)
  result <- ggpubr:::.add_faceted_interaction_labels(
    p, d, "x", "y", "group", c("facet1", "facet2")
  )
  labels <- result$layers[[1]]$data

  expect_identical(
    list(rows = nrow(labels), tuples = sort(paste(labels$facet1, labels$facet2, sep = "|"))),
    list(rows = 2L, tuples = sort(c("a\rb|c", "a|b\rc")))
  )
})

test_that("simple-effect labels keep x keys outside statistic columns", {
  check_name <- function(xname) {
    d <- expand.grid(
      x = c("A", "B"), group = c("U", "V"), replicate = 1:8,
      KEEP.OUT.ATTRS = FALSE, stringsAsFactors = FALSE
    )
    d$y <- d$replicate + ifelse(d$x == "B", 2, 0) +
      ifelse(d$group == "V", 1, 0) + ifelse(d$x == "B" & d$group == "V", 3, 0)
    names(d)[names(d) == "x"] <- xname
    p <- ggplot2::ggplot(d, ggplot2::aes(x = .data[[xname]], y = y))
    pwc <- data.frame(xmin = c(0.8, 1.8), xmax = c(1.2, 2.2), y.position = c(15, 15))
    result <- ggpubr:::.add_simple_effect_labels(
      p, d, xname, "y", "group", xname, "group", pwc
    )
    label.data <- result$layers[[1]]$data
    c(rows = nrow(label.data), finite_x = sum(is.finite(label.data$.x)))
  }

  observed <- lapply(c("F", "Effect", "DFn", "DFd", "ges", "p"), check_name)
  expect_identical(observed, rep(list(c(rows = 2L, finite_x = 2L)), 6))
})

test_that("faceted two-way ggcompare isolates computed statistic names", {
  check_name <- function(facet.name) {
    d <- expand.grid(
      xvar = c("A", "B"), group = c("U", "V"), facet = c("F1", "F2"),
      replicate = 1:6, KEEP.OUT.ATTRS = FALSE, stringsAsFactors = FALSE
    )
    d$value <- d$replicate + ifelse(d$xvar == "B", 2, 0) +
      ifelse(d$group == "V", 1, 0) + ifelse(d$xvar == "B" & d$group == "V", 3, 0)
    names(d)[names(d) == "facet"] <- facet.name
    p <- ggcompare(
      d, "xvar", "value", color = "group", facet.by = facet.name,
      method = "t_test", hide.ns = FALSE, label = "{p}", omnibus = "anova"
    )
    built <- ggplot2::ggplot_build(p)
    c(
      panels = nrow(built$layout$layout),
      user_values = length(unique(p$data[[facet.name]]))
    )
  }

  names <- c(".label", ".x", "p.adj", "x", "p", "group1", "group2", "y.position")
  expect_identical(lapply(names, check_name), rep(list(c(panels = 2L, user_values = 2L)), length(names)))
})
