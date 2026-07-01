# Regression tests for #642: a NAMED palette vector triggered a spurious
# "No shared levels found between `names(values)` ..." warning at draw time,
# because ggpar() applied BOTH scale_color_manual and scale_fill_manual even
# when only one of color/fill was mapped to a variable.

.build_warnings <- function(p) {
  ws <- character(0)
  withCallingHandlers(
    ggplot2::ggplot_gtable(ggplot2::ggplot_build(p)),
    warning = function(w) {
      ws[[length(ws) + 1]] <<- conditionMessage(w)
      invokeRestart("muffleWarning")
    }
  )
  ws
}
.no_shared <- "No shared levels"

test_that("named palette on a fill-only plot does not warn (#642)", {
  df <- data.frame(prop = c(0.4, 0.6),
                   my_categories = c("X", "Y"), label = c("40%", "60%"))
  pal <- c("X" = "red", "Y" = "blue")
  p <- ggpie(df, "prop", fill = "my_categories", label = "label", palette = pal)
  expect_false(any(grepl(.no_shared, .build_warnings(p))))
  # fill colors are still applied by name
  fills <- unique(ggplot2::ggplot_build(p)$data[[1]]$fill)
  expect_setequal(fills, c("red", "blue"))
})

test_that("named palette on a color-only plot does not warn (#642)", {
  p <- ggline(ToothGrowth, "dose", "len", color = "supp",
              palette = c("OJ" = "red", "VC" = "blue"))
  expect_false(any(grepl(.no_shared, .build_warnings(p))))
})

test_that("unnamed palette keeps applying both color and fill (no regression, #642)", {
  # boxplot with color mapped: unnamed palette must still color it
  p <- ggboxplot(ToothGrowth, "dose", "len", color = "supp",
                 palette = c("red", "blue"))
  expect_false(any(grepl(.no_shared, .build_warnings(p))))
  cols <- unique(ggplot2::ggplot_build(p)$data[[1]]$colour)
  expect_setequal(cols, c("red", "blue"))
})

test_that("named palette with non-matching names still warns (#642)", {
  # user-supplied names genuinely don't match the data -> ggplot2's warning is
  # legitimate and must be preserved.
  p <- ggboxplot(ToothGrowth, "dose", "len", fill = "supp",
                 palette = c("A" = "red", "B" = "blue"))
  expect_true(any(grepl(.no_shared, .build_warnings(p))))
})
