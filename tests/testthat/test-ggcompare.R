context("test-ggcompare")

df <- ToothGrowth
df$dose <- as.factor(df$dose)

# number of pairwise brackets on a plot (distinct pwc groups)
n_brackets <- function(p) {
  b <- ggplot2::ggplot_build(p)
  idx <- which(vapply(
    b$data, function(d) all(c("x", "xend", "y", "yend", "label") %in% names(d)),
    logical(1)
  ))
  if (length(idx) == 0) {
    return(0L)
  }
  length(unique(b$data[[idx[1]]]$group))
}

pwc_labels <- function(p) {
  b <- ggplot2::ggplot_build(p)
  idx <- which(vapply(
    b$data, function(d) all(c("x", "xend", "y", "yend", "label") %in% names(d)),
    logical(1)
  ))
  unique(b$data[[idx[1]]]$label[!is.na(b$data[[idx[1]]]$label)])
}

test_that("ggcompare returns a single ggplot with all-pairwise brackets and a subtitle", {
  p <- ggcompare(df, x = "dose", y = "len")
  expect_s3_class(p, "ggplot")
  expect_false(inherits(p, "ggsummarystats"))
  expect_equal(n_brackets(p), 3) # all pairwise for 3 groups
  expect_false(is.null(p$labels$subtitle)) # omnibus label
  expect_silent(ggplot2::ggplotGrob(p)) # renders at draw time
})

test_that("ggcompare draws only the requested comparison subset (label -> position)", {
  p <- ggcompare(df, x = "dose", y = "len",
    comparisons = list(c("0.5", "1"), c("1", "2"))
  )
  expect_equal(n_brackets(p), 2)
})

test_that("ggcompare errors on a comparison level that does not exist", {
  expect_error(
    ggcompare(df, x = "dose", y = "len", comparisons = list(c("0.5", "9"))),
    "not found in"
  )
})

test_that("ggcompare effsize uses the method-appropriate symbol", {
  # t_test -> Cohen's d
  labs_t <- pwc_labels(ggcompare(df, "dose", "len", method = "t_test", effsize = TRUE))
  expect_true(all(grepl("d=", labs_t, fixed = TRUE)))
  # wilcox_test -> Cliff's delta (must NOT be mislabelled as d=)
  labs_w <- pwc_labels(ggcompare(df, "dose", "len", method = "wilcox_test", effsize = TRUE))
  expect_true(all(grepl("delta=", labs_w, fixed = TRUE)))
  expect_false(any(grepl("d=", labs_w, fixed = TRUE)))
  # dunn_test -> r
  labs_d <- pwc_labels(ggcompare(df, "dose", "len", method = "dunn_test", effsize = TRUE))
  expect_true(all(grepl("r=", labs_d, fixed = TRUE)))
})

test_that("ggcompare omnibus modes set the matching subtitle / none omits it", {
  # kruskal
  p_k <- ggcompare(df, x = "dose", y = "len", omnibus = "kruskal")
  # omnibus none -> no subtitle added by ggcompare
  p_none <- ggcompare(df, x = "dose", y = "len", omnibus = "none")
  expect_null(p_none$labels$subtitle)
  expect_false(is.null(p_k$labels$subtitle))
})

test_that("ggcompare supports violin and stripchart bases", {
  expect_silent(ggplot2::ggplotGrob(ggcompare(df, "dose", "len", base = "violin")))
  expect_silent(ggplot2::ggplotGrob(ggcompare(df, "dose", "len", base = "stripchart")))
})

test_that("ggcompare computes brackets within each facet panel", {
  p <- ggcompare(df, x = "dose", y = "len", facet.by = "supp")
  b <- ggplot2::ggplot_build(p)
  idx <- which(vapply(
    b$data, function(d) all(c("x", "xend", "y", "yend", "label") %in% names(d)),
    logical(1)
  ))
  panels <- unique(b$data[[idx[1]]]$PANEL)
  expect_equal(length(panels), 2)
  expect_silent(ggplot2::ggplotGrob(p))
})

test_that("ggcompare coerces a non-factor x", {
  d2 <- df
  d2$dose <- as.character(d2$dose)
  expect_equal(n_brackets(ggcompare(d2, "dose", "len")), 3)
})

# ---- composition with ggsummarystats -------------------------------------
test_that("ggsummarystats(ggfunc = ggcompare) returns the compound object", {
  r <- ggsummarystats(df, x = "dose", y = "len", ggfunc = ggcompare)
  expect_s3_class(r, "ggsummarystats")
  expect_equal(n_brackets(r$main.plot), 3)
})

test_that("ggsummarystats(comparisons=) sugar routes to ggcompare", {
  r <- ggsummarystats(df, x = "dose", y = "len", comparisons = list(c("0.5", "1")))
  expect_s3_class(r, "ggsummarystats")
  expect_equal(n_brackets(r$main.plot), 1)
})

test_that("an explicit ggfunc wins over the comparisons= sugar", {
  # ggboxplot has no brackets; comparisons falls into ... harmlessly
  r <- ggsummarystats(df, x = "dose", y = "len",
    ggfunc = ggboxplot, comparisons = list(c("0.5", "1"))
  )
  expect_equal(n_brackets(r$main.plot), 0)
})

test_that("ggsummarystats without comparisons is unaffected by the sugar", {
  r <- ggsummarystats(df, x = "dose", y = "len")
  expect_s3_class(r, "ggsummarystats")
  expect_equal(n_brackets(r$main.plot), 0) # no brackets, plain boxplot
})
