context("test-ggpar-element-markdown")

# #382 / survminer #520: ggpar() re-themes a plot by adding element_text() to theme
# slots. When a slot already holds a ggtext::element_markdown() (survminer sets one
# on the risk-table strata labels), ggplot2 errored "Only elements of the same class
# can be merged" / "Can't merge the `<slot>` theme element". ggpar() now leaves such
# markdown slots untouched instead of crashing; normal element_text slots are handled
# exactly as before.
#
# The tests build a markdown-classed element WITHOUT depending on ggtext, so they run
# everywhere: an element carrying the same "element_markdown" class signature triggers
# the identical merge path (dispatch/assertion in ggplot2 is class-based).

suppressPackageStartupMessages(library(ggplot2))

# Synthetic ggtext::element_markdown()-classed element (no ggtext dependency).
.fake_markdown_element <- function() {
  el <- ggplot2::element_text()
  class(el) <- c("element_markdown", class(el))
  el
}

test_that("ggpar() does not crash and renders when a slot is element_markdown (#382)", {
  p <- ggboxplot(ToothGrowth, "dose", "len") +
    theme(axis.text.y = .fake_markdown_element())
  expect_error(
    q <- ggpar(p, font.tickslab = c(14, "bold"), font.x = c(16), font.main = c(18)),
    NA
  )
  # full render must also succeed (draw-time, not just build)
  expect_error(ggplot2::ggplot_gtable(ggplot2::ggplot_build(q)), NA)
})

test_that("ggpar() leaves the markdown slot intact but restyles other slots (#382)", {
  p <- ggboxplot(ToothGrowth, "dose", "len") +
    theme(axis.text.y = .fake_markdown_element())
  q <- ggpar(p, font.tickslab = c(14, "bold"), font.x = c(16))
  # markdown slot preserved (not overwritten with element_text)
  expect_true(inherits(q$theme[["axis.text.y"]], "element_markdown"))
  # a normal slot is still restyled as requested
  expect_true(inherits(q$theme[["axis.text.x"]], "element_text"))
  expect_false(inherits(q$theme[["axis.text.x"]], "element_markdown"))
  expect_equal(q$theme[["axis.text.x"]]$size, 14)
  expect_equal(q$theme[["axis.title.x"]]$size, 16)
})

test_that("ggpar() handles a list of ggplots, guarding each markdown slot (#382, survminer #520)", {
  # Mimic a ggsurvplot: a list holding two ggplots, one with a markdown element on
  # the risk-table-like component (as survminer sets on its strata labels). ggpar()
  # must iterate the list, not crash, and preserve the markdown element.
  p_plot <- ggboxplot(ToothGrowth, "dose", "len")
  p_tab <- ggboxplot(ToothGrowth, "dose", "len") +
    theme(axis.text.y = .fake_markdown_element())
  lst <- list(plot = p_plot, table = p_tab)
  expect_error(res <- ggpar(lst, font.title = c(16, "bold"), font.x = c(14),
                            font.tickslab = c(12)), NA)
  expect_true(is.list(res))
  # markdown slot on the "table" is preserved, curve component restyled
  expect_true(inherits(res$table$theme[["axis.text.y"]], "element_markdown"))
  expect_equal(res$plot$theme[["axis.title.x"]]$size, 14)
  # both components render
  expect_error(ggplot2::ggplot_gtable(ggplot2::ggplot_build(res$plot)), NA)
  expect_error(ggplot2::ggplot_gtable(ggplot2::ggplot_build(res$table)), NA)
})

test_that("ggpar() output is unchanged for a normal plot (no-regression, #382)", {
  # For a plot with no markdown element, ggpar() must behave exactly as before:
  # every requested font slot is applied as an element_text with the given values.
  p <- ggboxplot(ToothGrowth, "dose", "len", color = "supp")
  q <- ggpar(p, main = "M", xlab = "X", ylab = "Y",
             font.main = c(20, "bold", "red"), font.x = c(14, "bold", "blue"),
             font.y = c(14), font.tickslab = c(11, "plain", "darkgreen"),
             font.legend = c(9), legend.title = "Supp",
             xtickslab.rt = 45, ytickslab.rt = 90)
  expect_equal(q$theme[["plot.title"]]$size, 20)
  expect_equal(q$theme[["plot.title"]]$colour, "red")
  expect_equal(q$theme[["axis.title.x"]]$size, 14)
  expect_equal(q$theme[["axis.title.x"]]$colour, "blue")
  expect_equal(q$theme[["axis.text.x"]]$size, 11)
  expect_equal(q$theme[["axis.text.x"]]$colour, "darkgreen")
  expect_equal(q$theme[["axis.text.x"]]$angle, 45)
  expect_equal(q$theme[["axis.text.y"]]$angle, 90)
  expect_equal(q$theme[["legend.text"]]$size, 9)
  # ticks still applied
  expect_true(inherits(q$theme[["axis.ticks"]], "element_line"))
})

test_that("ggpar(tickslab = FALSE) still blanks the tick labels (#382)", {
  p <- ggboxplot(ToothGrowth, "dose", "len")
  q <- ggpar(p, tickslab = FALSE)
  expect_true(inherits(q$theme[["axis.text.x"]], "element_blank"))
  expect_true(inherits(q$theme[["axis.text.y"]], "element_blank"))
})
