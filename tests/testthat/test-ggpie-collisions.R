test_that("ggpie preserves mapped columns named like its label internals", {
  make_plot <- function(collision) {
    d <- data.frame(value = c(1, 2, 3, 4), group = c("A", "A", "B", "B"))
    d[[collision]] <- rep(c("G1", "G2"), each = 2)
    label <- if (collision == ".label.") paste0("row", seq_len(nrow(d))) else "group"
    p <- ggpie(d, "value", label = label, lab.pos = "in", fill = collision)
    text <- p$layers[[2]]
    list(
      values = sort(unique(as.character(p$data[[collision]]))),
      fill = rlang::as_label(p$layers[[1]]$mapping$fill),
      label = rlang::as_label(text$mapping$label),
      y = rlang::as_label(text$mapping$y)
    )
  }

  expect_identical(
    list(label = make_plot(".label."), ypos = make_plot(".lab.ypos.")),
    list(
      label = list(values = c("G1", "G2"), fill = ".label.", label = ".label.1", y = ".lab.ypos."),
      ypos = list(values = c("G1", "G2"), fill = ".lab.ypos.", label = "group", y = ".lab.ypos.1")
    )
  )
})

test_that("ggdonutchart keeps vector labels attached through grouping sort", {
  d <- data.frame(value = 1:4, group = c("b", "a", "b", "a"))
  p <- ggdonutchart(d, "value", label = paste0("row", 1:4), fill = "group")

  expect_identical(
    paste(p$data$value, p$data$.label., sep = "="),
    c("1=row1", "3=row3", "2=row2", "4=row4")
  )
})

test_that("ggdonutchart preserves mapped columns named like its label internals", {
  make_plot <- function(collision) {
    d <- data.frame(value = c(1, 2, 3, 4), group = c("A", "A", "B", "B"))
    d[[collision]] <- rep(c("G1", "G2"), each = 2)
    label <- if (collision == ".label.") paste0("row", seq_len(nrow(d))) else "group"
    p <- ggdonutchart(d, "value", label = label, lab.pos = "in", fill = collision)
    text <- p$layers[[2]]
    list(
      values = sort(unique(as.character(p$data[[collision]]))),
      fill = rlang::as_label(p$layers[[1]]$mapping$fill),
      label = rlang::as_label(text$mapping$label),
      y = rlang::as_label(text$mapping$y)
    )
  }

  expect_identical(
    list(label = make_plot(".label."), ypos = make_plot(".lab.ypos.")),
    list(
      label = list(values = c("G1", "G2"), fill = ".label.", label = ".label.1", y = ".lab.ypos."),
      ypos = list(values = c("G1", "G2"), fill = ".lab.ypos.", label = "group", y = ".lab.ypos.1")
    )
  )
})
