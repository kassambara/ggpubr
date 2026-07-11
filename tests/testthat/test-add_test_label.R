context("test-add_test_label")

df <- ToothGrowth
df$dose <- as.factor(df$dose)

test_that("add_test_label adds the omnibus ANOVA subtitle matching rstatix", {
  p <- ggboxplot(df, x = "dose", y = "len")
  out <- add_test_label(p, type = "text")
  expected <- rstatix::get_test_label(
    rstatix::anova_test(df, len ~ dose),
    detailed = TRUE, type = "text"
  )
  expect_equal(out$labels$subtitle, expected)
  expect_null(out$labels$caption) # caption is opt-in
})

test_that("add_test_label supports the Kruskal-Wallis omnibus test", {
  p <- ggboxplot(df, x = "dose", y = "len")
  out <- add_test_label(p, method = "kruskal", type = "text")
  expected <- rstatix::get_test_label(
    rstatix::kruskal_test(df, len ~ dose),
    detailed = TRUE, type = "text"
  )
  expect_equal(out$labels$subtitle, expected)
})

test_that("add_test_label adds a pairwise caption when caption = TRUE", {
  # anova -> tukey_hsd caption by default
  out_a <- add_test_label(ggboxplot(df, x = "dose", y = "len"),
    caption = TRUE, type = "text"
  )
  expect_equal(
    out_a$labels$caption,
    rstatix::get_pwc_label(rstatix::tukey_hsd(df, len ~ dose), type = "text")
  )
  # kruskal -> dunn_test caption by default
  out_k <- add_test_label(ggboxplot(df, x = "dose", y = "len"),
    method = "kruskal", caption = TRUE, type = "text"
  )
  expect_equal(
    out_k$labels$caption,
    rstatix::get_pwc_label(rstatix::dunn_test(df, len ~ dose), type = "text")
  )
})

test_that("add_test_label honors an explicit pwc.method", {
  out <- add_test_label(ggboxplot(df, x = "dose", y = "len"),
    caption = TRUE, pwc.method = "games_howell_test", type = "text"
  )
  expect_equal(
    out$labels$caption,
    rstatix::get_pwc_label(rstatix::games_howell_test(df, len ~ dose), type = "text")
  )
})

test_that("add_test_label returns a themeable ggplot (subtitle overridable)", {
  out <- add_test_label(ggboxplot(df, x = "dose", y = "len")) +
    ggplot2::labs(subtitle = "custom")
  expect_equal(out$labels$subtitle, "custom")
  expect_s3_class(out, "ggplot")
})

test_that("add_test_label (caption = FALSE) preserves an existing caption", {
  p <- ggboxplot(df, x = "dose", y = "len") +
    ggplot2::labs(caption = "Source: study X")
  out <- add_test_label(p) # default caption = FALSE
  expect_equal(out$labels$caption, "Source: study X")
  # and caption = TRUE replaces it with the pairwise description
  out2 <- add_test_label(p, caption = TRUE, type = "text")
  expect_equal(
    out2$labels$caption,
    rstatix::get_pwc_label(rstatix::tukey_hsd(df, len ~ dose), type = "text")
  )
})

test_that("add_test_label warns for faceted plots", {
  p <- ggboxplot(df, x = "dose", y = "len", facet.by = "supp")
  expect_warning(add_test_label(p), "pooling all facet panels")
})

test_that("add_test_label errors on non-ggplot or unmapped input", {
  expect_error(add_test_label(1), "must be a ggplot")
  p_noy <- ggplot2::ggplot(df, ggplot2::aes(x = dose))
  expect_error(add_test_label(p_noy), "`x` and `y`")
})
