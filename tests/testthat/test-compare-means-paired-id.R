context("test-compare-means-paired-id")

# compare_means() gains an opt-in `id` argument for paired tests (#560). Without
# it, paired tests pair by ROW ORDER, so mis-sorted data give a wrong p-value.
# With `id`, the pairs are aligned by subject id (row-order independent) via
# rstatix. Default (id = NULL) is byte-identical.

suppressMessages(library(dplyr))

.make_paired <- function(seed = 123) {
  set.seed(seed)
  data.frame(
    sampleType = c(rep("Normal", 10), rep("Tumor", 10)),
    value = c(runif(10, 0, 0.2), runif(10, 0, 0.4)),
    ID = c(1:10, 10:1) # deliberately unsorted so row-order pairing is wrong
  )
}

.hand_paired <- function(d, method = "wilcox.test") {
  x <- d %>% filter(sampleType == "Normal") %>% arrange(ID) %>% pull(value)
  y <- d %>% filter(sampleType == "Tumor") %>% arrange(ID) %>% pull(value)
  fun <- match.fun(method)
  suppressWarnings(fun(x, y, paired = TRUE))$p.value
}

test_that("id yields the correct paired p-value regardless of row order (#560)", {
  d <- .make_paired()
  sorted <- d %>% arrange(sampleType, ID)
  hand <- .hand_paired(d, "wilcox.test")

  # id path matches the hand id-aligned test...
  expect_equal(compare_means(value ~ sampleType, d, paired = TRUE, id = "ID")$p, hand)
  # ...and is order-independent (unsorted == sorted)
  expect_equal(
    compare_means(value ~ sampleType, d, paired = TRUE, id = "ID")$p,
    compare_means(value ~ sampleType, sorted, paired = TRUE, id = "ID")$p
  )
  # the buggy row-order default differs from the correct value (this is the bug)
  expect_false(isTRUE(all.equal(
    compare_means(value ~ sampleType, d, paired = TRUE)$p, hand
  )))
})

test_that("id works for both wilcox.test and t.test", {
  d <- .make_paired()
  expect_equal(compare_means(value ~ sampleType, d, method = "wilcox.test", paired = TRUE, id = "ID")$p,
               .hand_paired(d, "wilcox.test"))
  expect_equal(compare_means(value ~ sampleType, d, method = "t.test", paired = TRUE, id = "ID")$p,
               .hand_paired(d, "t.test"))
})

test_that("group1/group2 labels match the default convention", {
  d <- .make_paired()
  got <- compare_means(value ~ sampleType, d, paired = TRUE, id = "ID")
  def <- compare_means(value ~ sampleType, d, paired = TRUE)
  expect_equal(c(got$group1, got$group2), c(def$group1, def$group2))
})

test_that("id inner-joins on mismatched membership (uses only complete pairs)", {
  # group A ids {1,2,3}, group B ids {1,2,4}: only {1,2} are shared
  mm <- data.frame(
    g = c("A", "A", "A", "B", "B", "B"),
    value = c(1.2, 2.4, 3.1, 1.9, 2.8, 9.2),
    ID = c(1, 2, 3, 1, 2, 4)
  )
  got <- compare_means(value ~ g, mm, method = "t.test", paired = TRUE, id = "ID")$p
  hand <- t.test(c(1.2, 2.4), c(1.9, 2.8), paired = TRUE)$p.value
  expect_equal(got, hand)
})

test_that("id validation rejects unsupported combinations", {
  d <- .make_paired()
  expect_error(compare_means(value ~ sampleType, d, id = "ID"), "paired = TRUE")
  expect_error(compare_means(value ~ sampleType, d, method = "anova", paired = TRUE, id = "ID"),
               "wilcox.test")
  expect_error(compare_means(value ~ sampleType, d, paired = TRUE, id = "nope"),
               "can't find")
  # ref.group = ".all." (basemean) is unsupported with id
  expect_error(compare_means(value ~ sampleType, d, paired = TRUE, id = "ID", ref.group = ".all."),
               "\\.all\\.")
  # duplicate id within a group is rejected (by the underlying paired test)
  dup <- data.frame(g = c("A", "A", "B", "B"), value = c(1, 2, 3, 4.5), ID = c(1, 1, 1, 2))
  expect_error(compare_means(value ~ g, dup, method = "t.test", paired = TRUE, id = "ID"))
})

test_that("id works for pairwise (>2 groups) comparisons, matching rstatix (#560)", {
  set.seed(3)
  d <- data.frame(g = rep(c("A", "B", "C"), each = 8),
                  value = rnorm(24),
                  ID = c(1:8, sample(1:8), sample(1:8)))  # unsorted
  got <- compare_means(value ~ g, d, method = "wilcox.test", paired = TRUE, id = "ID")
  ref <- rstatix::wilcox_test(d, value ~ g, paired = TRUE, id = "ID", p.adjust.method = "none")
  # same set of comparisons and p-values as a direct rstatix call
  key <- function(x) paste(x$group1, x$group2, signif(x$p, 6))
  expect_setequal(key(got), key(ref))
  expect_equal(nrow(got), 3L)  # A-B, A-C, B-C
})

test_that("id works together with ref.group (only ref comparisons, matching rstatix)", {
  set.seed(4)
  d <- data.frame(g = rep(c("ctrl", "t1", "t2"), each = 8),
                  value = rnorm(24), ID = c(1:8, sample(1:8), sample(1:8)))
  got <- compare_means(value ~ g, d, method = "t.test", paired = TRUE, id = "ID", ref.group = "ctrl")
  expect_equal(nrow(got), 2L)                 # ctrl vs t1, ctrl vs t2
  expect_true(all(got$group1 == "ctrl"))      # ref is always group1
  ref <- rstatix::t_test(d, value ~ g, paired = TRUE, id = "ID", ref.group = "ctrl",
                         p.adjust.method = "none")
  expect_equal(sort(got$p), sort(ref$p))
})

test_that("id path works with a non-syntactic (hyphenated) group column (#385 parity)", {
  d <- data.frame(
    `grp-x` = c("A", "A", "B", "B"),
    value = c(1.2, 2.4, 1.9, 2.8),
    ID = c(1, 2, 1, 2),
    check.names = FALSE
  )
  got <- compare_means(value ~ `grp-x`, d, method = "t.test", paired = TRUE, id = "ID")$p
  hand <- t.test(c(1.2, 2.4), c(1.9, 2.8), paired = TRUE)$p.value
  expect_equal(got, hand)
  # and matches the default path's handling of the same column
  expect_equal(got, compare_means(value ~ `grp-x`, d %>% dplyr::arrange(`grp-x`, ID),
                                   method = "t.test", paired = TRUE)$p)
})

test_that("id type errors give a clear message", {
  d <- .make_paired()
  expect_error(compare_means(value ~ sampleType, d, paired = TRUE, id = 1),
               "single column name")
  expect_error(compare_means(value ~ sampleType, d, paired = TRUE, id = c("ID", "x")),
               "single column name")
})

test_that("default (id = NULL) is byte-identical to omitting it", {
  set.seed(1)
  dd <- data.frame(g = rep(c("A", "B"), each = 8), v = rnorm(16), ID = rep(1:8, 2))
  # paired and unpaired, with and without group.by
  expect_identical(compare_means(v ~ g, dd, paired = TRUE),
                   compare_means(v ~ g, dd, paired = TRUE, id = NULL))
  expect_identical(compare_means(v ~ g, dd),
                   compare_means(v ~ g, dd, id = NULL))
  dd$blk <- rep(c("x", "y"), 8)
  expect_identical(compare_means(v ~ g, dd, group.by = "blk", paired = TRUE),
                   compare_means(v ~ g, dd, group.by = "blk", paired = TRUE, id = NULL))
})

test_that("id column is retained (not dropped by the internal column selection)", {
  # regression guard for the df_select() fix that keeps the id column
  d <- .make_paired()
  expect_error(compare_means(value ~ sampleType, d, paired = TRUE, id = "ID"), NA)
})
