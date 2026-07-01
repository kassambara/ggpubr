# Regression tests for #646: with orientation = "horizontal", xlim/ylim were
# dropped because coord_flip() replaced the coord_cartesian() that carried the
# limits (also emitting "Coordinate system already present"). Limits are now
# passed into coord_flip().

.df <- data.frame(x = c("a", "b", "c"), y = c(10, 20, 30))

.range_of <- function(p, ax) {
  ggplot2::ggplot_build(p)$layout$panel_params[[1]][[ax]]
}
# Capture both messages and warnings emitted while CONSTRUCTING and building the
# plot. The "Coordinate system already present" note is emitted as a message at
# construction time (when coord_flip replaces coord_cartesian), so we must build
# the plot inside the handlers, not pass in a finished plot.
.construct_notes <- function(expr) {
  notes <- character(0)
  withCallingHandlers(
    {
      p <- force(expr)
      ggplot2::ggplot_gtable(ggplot2::ggplot_build(p))
    },
    message = function(m) { notes[[length(notes) + 1]] <<- conditionMessage(m); invokeRestart("muffleMessage") },
    warning = function(w) { notes[[length(notes) + 1]] <<- conditionMessage(w); invokeRestart("muffleWarning") }
  )
  notes
}

test_that("ylim is honored with orientation='horizontal' (#646)", {
  p <- ggbarplot(.df, x = "x", y = "y", fill = "x",
                 orientation = "horizontal", ylim = c(0, 50))
  # After coord_flip the value (y) axis is the horizontal one: x.range
  r <- .range_of(p, "x.range")
  expect_true(min(r) <= 0 + 1e-8 && max(r) >= 50 - 1e-8)
  # no "Coordinate system already present" note (message or warning)
  expect_false(any(grepl(
    "already present",
    .construct_notes(ggbarplot(.df, x = "x", y = "y", fill = "x",
                               orientation = "horizontal", ylim = c(0, 50)))
  )))
})

test_that("vertical orientation ylim is unchanged (no regression, #646)", {
  p <- ggbarplot(.df, x = "x", y = "y", fill = "x", ylim = c(0, 50))
  r <- .range_of(p, "y.range")
  expect_true(min(r) <= 0 + 1e-8 && max(r) >= 50 - 1e-8)
  expect_false(any(grepl(
    "already present",
    .construct_notes(ggbarplot(.df, x = "x", y = "y", fill = "x", ylim = c(0, 50)))
  )))
})

test_that("horizontal without limits, and reverse, still render (no regression, #646)", {
  expect_no_error(ggplot2::ggplot_gtable(ggplot2::ggplot_build(
    ggbarplot(.df, x = "x", y = "y", fill = "x", orientation = "horizontal"))))
  expect_no_error(ggplot2::ggplot_gtable(ggplot2::ggplot_build(
    ggbarplot(.df, x = "x", y = "y", fill = "x", orientation = "reverse"))))
})
