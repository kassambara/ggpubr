context("test-ggbarplot-dodge2-reverse")

# #783: with position_dodge2(reverse = TRUE) every error bar was drawn on the
# neighbouring bar, carrying that neighbour's mean and error. ggplot2's
# collide2() lays each x out by DESCENDING group id when `reverse` is set
# (`order(data$x, -data$group)`), while ggbarplot()'s re-centring ordered the
# summary ascending - so each row was matched to the mirror bar. The values were
# right; the pairing was not, and nothing in the figure showed it.
#
# References are computed with stats::aggregate and sd(x)/sqrt(n), never
# desc_statby(), so the assertions cannot pass by agreeing with the code under
# test.

.rev_ref <- function(d, by, yv) {
  m <- stats::aggregate(stats::as.formula(paste(yv, "~", paste(by, collapse = " + "))), d, mean)
  s <- stats::aggregate(stats::as.formula(paste(yv, "~", paste(by, collapse = " + "))), d,
                        function(z) stats::sd(z) / sqrt(length(z)))
  names(m)[ncol(m)] <- "mean"; names(s)[ncol(s)] <- "se"
  merge(m, s, by = by)
}

.rev_layer <- function(p, b, classes) {
  i <- which(vapply(p$layers, function(l) class(l$geom)[1] %in% classes, logical(1)))
  if (!length(i)) return(NULL)
  b$data[[i[1]]]
}

# Every interval must lie inside exactly one bar and carry THAT bar's own
# statistic. Returns the number that do.
.rev_on_own_bar <- function(p, ref, limit = "both") {
  b <- suppressWarnings(ggplot2::ggplot_build(p))
  bd <- .rev_layer(p, b, "GeomBar")
  ed <- .rev_layer(p, b, c("GeomErrorbar", "GeomPointrange", "GeomLinerange"))
  if (is.null(ed)) stop("no error layer was drawn")
  ok <- 0L
  for (i in seq_len(nrow(ed))) {
    j <- which(as.character(bd$PANEL) == as.character(ed$PANEL[i]) &
                 as.numeric(ed$x[i]) >= as.numeric(bd$xmin) - 1e-9 &
                 as.numeric(ed$x[i]) <= as.numeric(bd$xmax) + 1e-9)
    if (length(j) != 1) next
    q <- which(abs(ref$mean - as.numeric(bd$y[j])) < 1e-9)
    if (length(q) != 1) next
    lo <- if (limit == "upper") ref$mean[q] else ref$mean[q] - ref$se[q]
    hi <- if (limit == "lower") ref$mean[q] else ref$mean[q] + ref$se[q]
    if (isTRUE(all.equal(c(ed$ymin[i], ed$ymax[i]), c(lo, hi), tolerance = 1e-9))) ok <- ok + 1L
  }
  list(ok = ok, n = nrow(ed))
}

test_that("dodge2(reverse = TRUE) pairs each error bar with its own bar (#783)", {
  tg <- ToothGrowth
  tg$dose <- factor(tg$dose)
  ref <- .rev_ref(tg, c("dose", "supp"), "len")

  p <- ggbarplot(tg, "dose", "len", fill = "supp", add = "mean_se",
                 position = position_dodge2(reverse = TRUE))
  r <- .rev_on_own_bar(p, ref)
  expect_equal(r$ok, r$n)
  expect_equal(r$n, 6L)

  # the specific transposition this fixes: the bar heights and the interval
  # centres are the same six numbers IN THE SAME ORDER, not swapped in pairs
  b <- suppressWarnings(ggplot2::ggplot_build(p))
  bd <- .rev_layer(p, b, "GeomBar"); ed <- .rev_layer(p, b, "GeomErrorbar")
  expect_equal(as.numeric(bd$y), (ed$ymin + ed$ymax) / 2, tolerance = 1e-9)
})

test_that("the reverse pairing holds across error.plot and preserve (#783)", {
  # reported against #363's recommended `preserve = "single"`, so that
  # combination is driven explicitly
  tbl <- data.frame(
    Treatment = c("+", "+", "+", "+", "+", "+", "-", "-", "-"),
    Group     = c("a", "a", "a", "b", "b", "b", "a", "a", "a"),
    Count     = c(12, 13, 11, 14, 15, 14, 23, 24, 25),
    stringsAsFactors = FALSE
  )
  ref <- .rev_ref(tbl, c("Treatment", "Group"), "Count")
  positions <- list(
    `reverse` = position_dodge2(reverse = TRUE),
    `reverse+single` = position_dodge2(preserve = "single", reverse = TRUE)
  )
  limits <- c(errorbar = "both", upper_errorbar = "upper", lower_errorbar = "lower",
              pointrange = "both", linerange = "both")
  for (pn in names(positions)) {
    for (ep in names(limits)) {
      p <- ggbarplot(tbl, "Treatment", "Count", fill = "Group", add = "mean_se",
                     error.plot = ep, position = positions[[pn]])
      r <- .rev_on_own_bar(p, ref, limit = limits[[ep]])
      expect_equal(r$ok, r$n, info = paste(pn, ep))
    }
  }
})

test_that("an NA level mirrors with the rest under reverse (#783)", {
  # order()'s na.last would pin NA to the same end in both directions; the rank
  # is assigned explicitly so it mirrors like every other level.
  set.seed(9)
  d <- expand.grid(g = c("x1", "x2"), f = c("f1", "f2", NA), r = 1:4,
                   stringsAsFactors = FALSE)
  d$v <- round(runif(nrow(d), 5, 50), 3)
  k <- interaction(addNA(factor(d$g), ifany = TRUE), addNA(factor(d$f), ifany = TRUE), drop = TRUE)
  cell <- do.call(rbind, lapply(split(d$v, k), function(z) {
    data.frame(mean = mean(z), se = stats::sd(z) / sqrt(length(z)))
  }))
  for (rv in c(FALSE, TRUE)) {
    p <- ggbarplot(d, "g", "v", fill = "f", add = "mean_se",
                   position = position_dodge2(reverse = rv))
    r <- .rev_on_own_bar(p, cell, limit = "both")
    expect_equal(r$ok, r$n, info = paste("reverse =", rv))
  }
})

test_that("no-regression: dodge2 WITHOUT reverse is untouched (#783)", {
  # Pinned absolute positions and values. The fix also gives the released
  # (legend-only) sort key an explicit NA rank; without `reverse` that is the
  # same order `order()` already produced, and these pins say so.
  tg <- ToothGrowth
  tg$dose <- factor(tg$dose)
  p <- ggbarplot(tg, "dose", "len", fill = "supp", add = "mean_se",
                 position = position_dodge2())
  b <- suppressWarnings(ggplot2::ggplot_build(p))
  bd <- .rev_layer(p, b, "GeomBar"); ed <- .rev_layer(p, b, "GeomErrorbar")
  expect_equal(round(as.numeric(bd$x), 4),
               c(0.825, 1.175, 1.825, 2.175, 2.825, 3.175))
  expect_equal(as.numeric(ed$x), as.numeric(bd$x), tolerance = 1e-9)
  expect_equal(round(as.numeric(bd$y), 4),
               c(13.23, 7.98, 22.70, 16.77, 26.06, 26.14))
})
