context("test-ggarrange-common-legend")

# #347: common.legend = TRUE keeps only the FIRST plot's legend (it is not merged
# or validated). A numeric common.legend now selects WHICH plot's legend to use as
# the shared one, e.g. common.legend = 2 uses the second plot's legend. Logical
# TRUE/FALSE are unchanged.

suppressPackageStartupMessages(library(ggplot2))

# p1 has 2 species, p2 has all 3 -> their legends genuinely differ.
p1 <- ggboxplot(subset(iris, Species != "virginica"), "Species", "Sepal.Length", fill = "Species")
p2 <- ggboxplot(iris, "Species", "Sepal.Width", fill = "Species")

# md5 of the rendered arrangement (same machine -> deterministic; fonts identical).
.arr_hash <- function(p) {
  f <- tempfile(fileext = ".png")
  on.exit(unlink(f))
  suppressMessages(ggplot2::ggsave(f, p, width = 6, height = 3, dpi = 72, device = "png"))
  unname(tools::md5sum(f))
}

test_that("numeric common.legend selects that plot's legend", {
  h_true <- .arr_hash(ggarrange(p1, p2, ncol = 2, common.legend = TRUE))
  h_i1   <- .arr_hash(ggarrange(p1, p2, ncol = 2, common.legend = 1))
  h_i2   <- .arr_hash(ggarrange(p1, p2, ncol = 2, common.legend = 2))
  # index 1 is exactly the historical behavior (first plot's legend)
  expect_identical(h_i1, h_true)
  # index 2 picks a DIFFERENT (representative) legend -> different output
  expect_false(identical(h_i2, h_i1))
  # equivalent to supplying that legend via legend.grob
  h_grob <- .arr_hash(ggarrange(p1, p2, ncol = 2, common.legend = TRUE,
                                legend.grob = get_legend(p2)))
  expect_identical(h_i2, h_grob)
})

test_that("common.legend = TRUE / FALSE are unchanged and render", {
  expect_error(cowplot::as_grob(ggarrange(p1, p2, ncol = 2, common.legend = TRUE)), NA)
  expect_error(cowplot::as_grob(ggarrange(p1, p2, ncol = 2, common.legend = FALSE)), NA)
})

test_that("invalid numeric common.legend is rejected with a clear error", {
  expect_error(ggarrange(p1, p2, common.legend = 5), "between 1 and")   # out of range
  expect_error(ggarrange(p1, p2, common.legend = 0), "between 1 and")   # < 1
  expect_error(ggarrange(p1, p2, common.legend = 1.5), "whole number")  # non-integer
  expect_error(ggarrange(p1, p2, common.legend = c(1, 2)))              # length > 1
  expect_error(ggarrange(p1, p2, common.legend = NA_real_))            # NA
})

test_that("common.legend index pointing to a NULL plot errors clearly", {
  expect_error(ggarrange(p1, NULL, p2, common.legend = 2), "NULL")
})
