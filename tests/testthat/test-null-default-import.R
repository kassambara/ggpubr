# Regression test for #665: geom_bracket()/geom_pwc() use the `%||%` operator,
# which base R only provides since R 4.4, while DESCRIPTION allows R (>= 4.1).
# It must be imported from rlang so it resolves on older R too.

test_that("`%||%` is imported into the ggpubr namespace (#665)", {
  imports_env <- parent.env(asNamespace("ggpubr"))
  # revert-sensitive: this binding only exists because of `importFrom(rlang, "%||%")`
  expect_true("%||%" %in% ls(imports_env, all.names = TRUE))
  expect_error(get("%||%", envir = imports_env, inherits = FALSE), NA)
  # rlang genuinely exports it (the import source)
  expect_true("%||%" %in% getNamespaceExports("rlang"))
})

test_that("geom_bracket()/geom_pwc() (which use %||%) still render (#665)", {
  p1 <- ggboxplot(ToothGrowth, "dose", "len") +
    geom_bracket(xmin = "0.5", xmax = "1", y.position = 35, label = "p < 0.05")
  expect_no_error(ggplot2::ggplot_gtable(ggplot2::ggplot_build(p1)))
  p2 <- ggboxplot(ToothGrowth, "dose", "len") + geom_pwc(method = "t_test")
  expect_no_error(suppressWarnings(ggplot2::ggplot_gtable(ggplot2::ggplot_build(p2))))
})
