# The histogram/density label path puts intermediate list-columns into the same
# frame it groups by, and `facet.by` is user-controlled. Any FIXED internal name
# is therefore a name some user may already have: plain `hist.data`/`lab.data`
# collided, and renaming them to `.hist.data.`/`.lab.data.` only moved the
# collision rather than removing it. These lock the property that matters -- the
# built plot is unaffected by what the facet column happens to be called.
#
# `y = "..count.."` remains explicit because these tests isolate the facet-name
# collision from the separately repaired default-y label path.

label_panels <- function(p) {
  built <- ggplot2::ggplot_build(p)
  text_layers <- which(vapply(
    p$layers,
    function(l) inherits(l$geom, "GeomText") || inherits(l$geom, "GeomLabel"),
    logical(1)
  ))
  expect_length(text_layers, 1)
  length(unique(built$data[[text_layers]]$PANEL))
}

# Every name here is a legal facet column and two of them are the package's own
# internal names, which is exactly the case the dotted rename failed to cover.
facet_names <- c("grp", "lab.data", "hist.data", ".lab.data.", ".hist.data.")

test_that("gghistogram label layer survives any facet column name", {
  for (nm in facet_names) {
    d <- data.frame(v = c(1, 2, 3, 4, 5, 6, 7, 8), lab = letters[1:8])
    d[[nm]] <- rep(c("A", "B"), each = 4)

    p <- gghistogram(d, x = "v", y = "..count..", label = "lab", facet.by = nm, bins = 4)
    built <- ggplot2::ggplot_build(p)

    expect_equal(length(unique(built$data[[1]]$PANEL)), 2, info = nm)
    expect_equal(label_panels(p), 2, info = nm)
  }
})

test_that("ggdensity label layer survives any facet column name", {
  for (nm in facet_names) {
    d <- data.frame(v = c(1, 2, 3, 4, 5, 6, 7, 8), lab = letters[1:8])
    d[[nm]] <- rep(c("A", "B"), each = 4)

    p <- ggdensity(d, x = "v", y = "..density..", label = "lab", facet.by = nm)
    built <- ggplot2::ggplot_build(p)

    expect_equal(length(unique(built$data[[1]]$PANEL)), 2, info = nm)
    expect_equal(label_panels(p), 2, info = nm)
  }
})

test_that("a colliding name still works alongside a second facet variable", {
  d <- data.frame(
    v = c(1, 2, 3, 4, 5, 6, 7, 8),
    lab = letters[1:8],
    .hist.data. = rep(c("A", "B"), each = 4),
    other = rep(c("x", "y"), times = 4),
    check.names = FALSE
  )

  p <- gghistogram(d,
    x = "v", y = "..count..", label = "lab",
    facet.by = c(".hist.data.", "other"), bins = 4
  )
  built <- ggplot2::ggplot_build(p)

  expect_equal(length(unique(built$data[[1]]$PANEL)), 4)
  expect_equal(label_panels(p), 4)
})

test_that("the facet column keeps its own values rather than the internal list", {
  # The failure mode was the internal list-column overwriting the grouping key,
  # so assert the built panel labels are the user's factor levels -- a panel
  # count alone would not distinguish that.
  d <- data.frame(v = c(1, 2, 3, 4, 5, 6, 7, 8), lab = letters[1:8])
  d[[".hist.data."]] <- rep(c("A", "B"), each = 4)

  p <- gghistogram(d, x = "v", y = "..count..", label = "lab",
                   facet.by = ".hist.data.", bins = 4)
  layout <- ggplot2::ggplot_build(p)$layout$layout

  expect_true(".hist.data." %in% names(layout))
  expect_setequal(as.character(layout[[".hist.data."]]), c("A", "B"))
})
