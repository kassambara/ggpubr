# Regression test for #365: ggmaplot() drew the non-significant ("NS") points
# last, so they covered the significant Up/Down points. NS points are now drawn
# first (behind) and the significant points remain visible on top.

test_that("ggmaplot() draws NS points before the significant ones (#365)", {
  data(diff_express, package = "ggpubr", envir = environment())
  p <- ggmaplot(diff_express, fdr = 0.05, fc = 2, size = 0.5, top = 0)
  d <- p$data   # the (reordered) data passed to ggplot / geom_point
  is_ns <- as.character(d$sig) == "NS"
  # every NS row comes before every significant row (drawn behind)
  if (any(is_ns) && any(!is_ns)) {
    expect_lt(max(which(is_ns)), min(which(!is_ns)))
  }
  expect_no_error(ggplot2::ggplot_gtable(ggplot2::ggplot_build(p)))
})

test_that("ggmaplot() legend keeps the Up / Down / NS order (no regression, #365)", {
  data(diff_express, package = "ggpubr", envir = environment())
  p <- ggmaplot(diff_express, fdr = 0.05, fc = 2, top = 0)
  levs <- levels(p$data$sig)
  # NS remains the last legend entry; Up/Down first
  expect_match(levs[1], "^Up")
  expect_match(levs[2], "^Down")
  expect_equal(levs[length(levs)], "NS")
})
