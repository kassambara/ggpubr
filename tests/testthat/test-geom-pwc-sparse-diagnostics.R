.pwc_messages <- function(expr) {
  seen <- character()
  withCallingHandlers(
    force(expr),
    message = function(cnd) {
      seen <<- c(seen, conditionMessage(cnd))
      invokeRestart("muffleMessage")
    }
  )
  seen
}

test_that("geom_pwc diagnoses panels where every comparison is untestable", {
  explicit <- data.frame(
    Cond = factor(rep(c("B", "D"), each = 3)),
    Value = c(NA, 11, NA, NA, NA, NA)
  )
  default <- data.frame(
    x = factor(rep(c("A", "B", "C"), times = c(4, 1, 4))),
    y = c(1:4, 5, 6:9)
  )
  explicit.msg <- suppressWarnings(.pwc_messages(ggplot2::ggplot_build(
    ggboxplot(explicit, "Cond", "Value") +
      geom_pwc(method = "t_test", method.args = list(comparisons = list(c(1, 2))))
  )))
  default.msg <- .pwc_messages(ggplot2::ggplot_build(
    ggboxplot(default, "x", "y") + geom_pwc(method = "t_test")
  ))
  expect_identical(
    c(
      explicit = any(grepl("skipped 1 untestable comparison(s): 1 vs 2", explicit.msg, fixed = TRUE)),
      default = any(grepl("no pairwise comparison could be tested", default.msg, fixed = TRUE))
    ),
    c(explicit = TRUE, default = TRUE)
  )
})
