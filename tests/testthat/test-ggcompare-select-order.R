# ggcompare() computed its statistics from the raw data while `select`, `remove`
# and `order` -- which are not its formals, and reach the base plot through
# `...` -- filtered and reordered the plot underneath. The bracket coordinates
# came out byte-identical whether or not the plot had been filtered, so a
# p-value was drawn above a pair of groups it did not describe. Nothing errored
# and nothing looked wrong.
#
# These lock the PROPERTY rather than three instances: a comparison must stay
# attached to its own group no matter how the display is filtered or reordered.
# Asserting the specific coordinates of specific brackets would pass again the
# moment someone changed the dodge width, while the defect returned.

# displayed group -> the significance label drawn over it, read off the built
# plot. Bracket midpoints land on integer axis positions, and the axis labels
# give the group at each position, so the two compose into the mapping that
# actually reaches a reader's eye.
label_by_group <- function(p) {
  b <- ggplot2::ggplot_build(p)
  # panel_scales_x is the original discrete scale even after coord_flip(); the
  # transformed panel x scale is then the continuous outcome axis.
  ticks <- b$layout$panel_scales_x[[1]]$get_labels()
  ticks <- ticks[!is.na(ticks)]
  base.centers <- sort(unique(as.numeric(b$data[[1]]$x)))
  out <- character(0)
  for (layer in b$data) {
    if (!all(c("xmin", "xmax", "label") %in% names(layer))) next
    u <- unique(layer[, c("xmin", "xmax", "label")])
    for (i in seq_len(nrow(u))) {
      endpoints <- as.numeric(c(u$xmin[i], u$xmax[i]))
      aligned <- vapply(
        endpoints,
        function(endpoint) any(abs(base.centers - endpoint) < 1e-7),
        logical(1)
      )
      if (!all(aligned)) {
        stop("Bracket endpoints do not align with the drawn group lanes.", call. = FALSE)
      }
      pos <- round((u$xmin[i] + u$xmax[i]) / 2)
      if (pos >= 1 && pos <= length(ticks)) out[ticks[pos]] <- as.character(u$label[i])
    }
  }
  out
}

td <- ToothGrowth
td$dose <- factor(td$dose)

test_that("ggcompare() statistics stay with their own group under select/remove/order", {
  reference <- label_by_group(ggcompare(td, x = "dose", y = "len", color = "supp"))

  # The reference must actually distinguish the groups, or every assertion below
  # would hold vacuously on an empty or constant mapping.
  expect_gt(length(reference), 1L)
  expect_gt(length(unique(reference)), 1L)

  for (case in list(
    list(nm = "select", args = list(select = c("1", "2"))),
    list(nm = "remove", args = list(remove = "0.5")),
    list(nm = "order",  args = list(order = c("2", "1", "0.5")))
  )) {
    got <- do.call(
      ggcompare,
      c(list(td, x = "dose", y = "len", color = "supp"), case$args)
    )
    got <- label_by_group(got)

    # Every group still drawn must carry the label it carried before the display
    # was disturbed. Groups filtered away simply drop out; the survivors must not
    # inherit a neighbour's statistic.
    for (g in names(got)) {
      expect_true(g %in% names(reference), info = paste(case$nm, "unknown group", g))
      expect_identical(
        got[[g]], reference[[g]],
        info = paste0(case$nm, ": group ", g, " shows ", got[[g]],
                      " but its own result is ", reference[[g]])
      )
    }
  }
})

test_that("the group-label oracle handles flipping and rejects dodge misalignment", {
  flipped <- label_by_group(ggcompare(
    td, "dose", "len", color = "supp",
    orientation = "horizontal", hide.ns = FALSE
  ))
  expect_true(all(names(flipped) %in% levels(td$dose)))

  misaligned <- ggcompare(
    td, "dose", "len", color = "supp",
    position = ggplot2::position_dodge(0.4), hide.ns = FALSE
  )
  expect_error(
    label_by_group(misaligned),
    "Bracket endpoints do not align with the drawn group lanes.",
    fixed = TRUE
  )
})

test_that("ggcompare(select=, comparisons=) brackets the pair that was requested", {
  set.seed(1)
  d <- data.frame(
    g = rep(c("A", "B", "C", "D"), each = 10),
    v = c(stats::rnorm(10, 5), stats::rnorm(10, 6),
          stats::rnorm(10, 9), stats::rnorm(10, 12))
  )

  p <- ggcompare(d, x = "g", y = "v",
                 select = c("B", "C", "D"), comparisons = list(c("B", "C")))
  b <- ggplot2::ggplot_build(p)
  ticks <- b$layout$panel_params[[1]]$x$get_labels()
  ticks <- ticks[!is.na(ticks)]
  expect_identical(ticks, c("B", "C", "D"))

  # The comparison bracket is the short one: exactly two endpoints on integer
  # positions. Its ends must name the requested pair on the axis as displayed --
  # the levels were translated against the unfiltered A|B|C|D, which put "B-C"
  # at positions 2-3, and on this axis those positions are C and D.
  spans <- NULL
  for (layer in b$data) {
    if (!all(c("xmin", "xmax") %in% names(layer))) next
    ends <- unique(c(layer$xmin, layer$xmax))
    if (length(ends) == 2L && all(ends == round(ends))) spans <- sort(ends)
  }
  expect_false(is.null(spans))
  expect_identical(ticks[spans], c("B", "C"))
})
