context("re-exports")

test_that("rstatix annotation helpers are re-exported and identical to rstatix", {
  helpers <- c(
    "get_test_label", "get_pwc_label", "create_test_label",
    "add_cld", "add_xy_position"
  )
  ggpubr_ns <- asNamespace("ggpubr")
  rstatix_ns <- asNamespace("rstatix")
  for (fn in helpers) {
    expect_true(fn %in% getNamespaceExports("ggpubr"),
                info = paste(fn, "should be exported by ggpubr"))
    expect_identical(
      get(fn, envir = ggpubr_ns),
      get(fn, envir = rstatix_ns),
      info = paste(fn, "should be the same object as rstatix's")
    )
  }
})
