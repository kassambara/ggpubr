context("test-ggbarplot-alpha-errorbar")

# #404: ggbarplot(alpha = <discrete var>, add = "mean_ci", position = position_dodge())
# used to error at draw time ("alpha * 255": non-numeric argument) and, once that was
# worked around, collapsed the summary across the alpha subgroup. The summary now groups
# by the alpha variable too, so the bars are drawn per subgroup with the alpha aesthetic
# preserved, and the error layer dodges by the same subgroup so error bars stay aligned.

suppressPackageStartupMessages({
  library(ggplot2)
  library(dplyr)
})

# small, deterministic 2x2 grouped data
df <- ToothGrowth
df$dose <- factor(df$dose)
df2 <- subset(df, dose %in% c("0.5", "1"))
df2$dose <- droplevels(df2$dose)

test_that("ggbarplot(alpha=var, add='mean_ci', dodge) renders without the alpha*255 error (#404)", {
  p <- ggbarplot(df2, x = "dose", y = "len", fill = "dose", alpha = "supp",
                 add = "mean_ci", position = position_dodge())
  # full render (the crash was draw-time, not build-time)
  expect_error(ggplot2::ggplot_gtable(ggplot2::ggplot_build(p)), NA)
})

test_that("alpha subgroup gives per-subgroup bars, aligned error bars, and a real alpha aesthetic (#404)", {
  p <- ggbarplot(df2, x = "dose", y = "len", fill = "dose", alpha = "supp",
                 add = "mean_ci", position = position_dodge())
  b <- suppressWarnings(ggplot2::ggplot_build(p))
  bar <- b$data[[1]]
  err <- b$data[[2]]
  # 2 dose x 2 supp = 4 bars and 4 error bars
  expect_equal(nrow(bar), 4)
  expect_equal(nrow(err), 4)
  # error bars sit exactly on the bar x-positions (aligned, not centered)
  expect_equal(sort(round(bar$x, 6)), sort(round(err$x, 6)))
  # x is genuinely dodged (not all at integer centers)
  expect_false(all(bar$x %in% c(1, 2)))
  # alpha is a mapped aesthetic (two distinct levels), not a static/NA value
  expect_equal(length(unique(round(bar$alpha, 4))), 2)
})

test_that("bar heights equal the per-subgroup means and CIs bracket them (#404)", {
  p <- ggbarplot(df2, x = "dose", y = "len", fill = "dose", alpha = "supp",
                 add = "mean_ci", position = position_dodge())
  b <- suppressWarnings(ggplot2::ggplot_build(p))
  bar <- b$data[[1]][order(b$data[[1]]$x), ]
  err <- b$data[[2]][order(b$data[[2]]$x), ]
  expected <- df2 %>%
    group_by(dose, supp) %>%
    summarise(m = mean(len), .groups = "drop")
  expect_equal(sort(round(bar$y, 6)), sort(round(expected$m, 6)))
  # each CI brackets its bar's mean
  expect_true(all(err$ymin < bar$y & bar$y < err$ymax))
})

test_that("no-regression: ggbarplot summaries without alpha are unchanged (#404)", {
  # A plain grouped summary bar (no alpha) must be untouched by the fix: 2 dose x 2 supp
  # bars, dodged, mean_se error bars centered on each bar as before.
  p <- ggbarplot(df, x = "dose", y = "len", fill = "supp",
                 add = "mean_se", position = position_dodge())
  b <- suppressWarnings(ggplot2::ggplot_build(p))
  expect_equal(nrow(b$data[[1]]), 6)                 # 3 dose x 2 supp
  # bar heights are the per-(dose,supp) means
  expected <- df %>% group_by(dose, supp) %>% summarise(m = mean(len), .groups = "drop")
  expect_equal(sort(round(b$data[[1]]$y, 6)), sort(round(expected$m, 6)))
  # error x aligns with bar x (this always worked for the single fill dimension)
  expect_equal(sort(round(b$data[[1]]$x, 6)), sort(round(b$data[[2]]$x, 6)))
})

test_that("the alpha subgroup also aligns under options(ggpubr.parse_aes = FALSE) (#404)", {
  # The error-layer dodge group is a real, safe-named column, so it works even when
  # aes parsing is disabled (used for column names with special characters).
  old <- options(ggpubr.parse_aes = FALSE)
  on.exit(options(old), add = TRUE)
  p <- ggbarplot(df2, x = "dose", y = "len", fill = "dose", alpha = "supp",
                 add = "mean_ci", position = position_dodge())
  b <- suppressWarnings(ggplot2::ggplot_build(p))
  expect_error(ggplot2::ggplot_gtable(b), NA)
  expect_equal(nrow(b$data[[1]]), 4)
  expect_equal(sort(round(b$data[[1]]$x, 6)), sort(round(b$data[[2]]$x, 6)))
})

test_that("a static numeric alpha is still treated as a constant, not a grouping var (#404)", {
  # alpha = 0.5 is a fixed transparency, not a column -> must NOT trigger subgrouping.
  p <- ggbarplot(df, x = "dose", y = "len", fill = "supp", alpha = 0.5,
                 add = "mean_se", position = position_dodge())
  b <- suppressWarnings(ggplot2::ggplot_build(p))
  expect_equal(nrow(b$data[[1]]), 6)                 # unchanged: 3 dose x 2 supp
  expect_true(all(abs(b$data[[1]]$alpha - 0.5) < 1e-9))
})
