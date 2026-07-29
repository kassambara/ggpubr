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

test_that("a CONTINUOUS legend variable is not mirrored under reverse (#783)", {
  # collide2() reverses via -group, and `group` is id() over the DISCRETE
  # aesthetics only. Map a continuous column to fill and every bar in an x
  # shares one group id, so the -group sort is a stable tie and the layout is
  # NOT reversed. Negating the sort key there mirrors the summary against bars
  # that never moved and puts every interval on its neighbour - the very defect
  # this file exists to prevent, reintroduced one configuration over. Master
  # drew this correctly, so it is a regression, not a missing feature.
  # Driven with stock ToothGrowth and `dose` left numeric - the form used
  # throughout ggpubr's own examples - as well as a synthetic integer column.
  tg0 <- ToothGrowth
  ref0 <- .rev_ref(tg0, c("supp", "dose"), "len")
  for (rv in c(FALSE, TRUE)) {
    p0 <- ggbarplot(tg0, "supp", "len", fill = "dose", add = "mean_se",
                    position = position_dodge2(reverse = rv))
    r0 <- .rev_on_own_bar(p0, ref0)
    expect_equal(r0$ok, r0$n, info = paste("ToothGrowth fill=dose, reverse =", rv))
    expect_equal(r0$n, 6L, info = paste("ToothGrowth fill=dose, reverse =", rv))
  }

  tg <- ToothGrowth
  tg$dose <- factor(tg$dose)
  tg$suppn <- as.integer(factor(tg$supp))   # continuous legend variable
  ref <- .rev_ref(tg, c("dose", "suppn"), "len")

  for (rv in c(FALSE, TRUE)) {
    p <- ggbarplot(tg, "dose", "len", fill = "suppn", add = "mean_se",
                   position = position_dodge2(reverse = rv))
    r <- .rev_on_own_bar(p, ref)
    expect_equal(r$ok, r$n, info = paste("reverse =", rv))
    expect_equal(r$n, 6L, info = paste("reverse =", rv))
  }

  # the bars really do share a group id, which is why -group cannot reverse them
  b <- suppressWarnings(ggplot2::ggplot_build(
    ggbarplot(tg, "dose", "len", fill = "suppn", add = "mean_se",
              position = position_dodge2(reverse = TRUE))
  ))
  bd <- .rev_layer(ggbarplot(tg, "dose", "len", fill = "suppn", add = "mean_se",
                             position = position_dodge2(reverse = TRUE)), b, "GeomBar")
  expect_true(anyDuplicated(bd$group) > 0)
})

test_that("two discrete legend columns stay misplaced under reverse (#783)", {
  # `color` and `fill` naming DIFFERENT discrete columns keys on the FIRST only,
  # so under reverse the second stays ascending inside each reversed block and
  # no interval lands on its own bar. Pre-existing and left alone.
  #
  # The ARRANGEMENT of the misplaced intervals does change (the first column is
  # now mirrored where it was not), so asserting only "0 of 8 correct" would be
  # true on both revisions and could not detect that. The drawn values are
  # pinned instead, recomputed here from base R so the pin says what the figure
  # actually shows rather than echoing the implementation.
  set.seed(2)
  d <- expand.grid(x = c("A", "B"), cc = c("c1", "c2"), ff = c("f1", "f2"),
                   r = 1:5, stringsAsFactors = FALSE)
  d$y <- round(runif(nrow(d), 5, 40), 3)
  ref <- .rev_ref(d, c("x", "cc", "ff"), "y")
  p <- ggbarplot(d, "x", "y", color = "cc", fill = "ff", add = "mean_se",
                 position = position_dodge2(reverse = TRUE))
  r <- .rev_on_own_bar(p, ref)
  expect_equal(r$n, 8L)
  expect_equal(r$ok, 0L)

  b <- suppressWarnings(ggplot2::ggplot_build(p))
  ed <- .rev_layer(p, b, "GeomErrorbar")
  # leftmost interval, as drawn: the (A, c2, f1) cell, on a bar that is not its
  # own. Recomputed with base R, not read off the built data.
  cell <- d[d$x == "A" & d$cc == "c2" & d$ff == "f1", "y"]
  expect_equal(as.numeric(ed$ymin[1]),
               mean(cell) - stats::sd(cell) / sqrt(length(cell)),
               tolerance = 1e-9)
})

test_that("a non-discrete color beside a discrete fill is unchanged (#783)", {
  # The bars ARE reversed here - ggplot2 groups them by the discrete `fill` -
  # but the error layer's key resolves to the non-discrete `color` first, so
  # nothing is mirrored and the intervals stay misplaced exactly as before.
  # Pinned because NEWS gives this as a distinct case with its own reason.
  set.seed(2)
  d <- expand.grid(x = c("A", "B"), ff = c("f1", "f2"), r = 1:5,
                   stringsAsFactors = FALSE)
  d$y <- round(runif(nrow(d), 5, 40), 3)
  d$num <- as.integer(factor(d$ff)) * 10   # non-discrete, 1:1 with `ff`
  ref <- .rev_ref(d, c("x", "ff"), "y")
  p <- ggbarplot(d, "x", "y", color = "num", fill = "ff", add = "mean_se",
                 position = position_dodge2(reverse = TRUE))
  r <- .rev_on_own_bar(p, ref)
  expect_equal(r$n, 4L)
  expect_equal(r$ok, 0L)
  b <- suppressWarnings(ggplot2::ggplot_build(p))
  ed <- .rev_layer(p, b, "GeomErrorbar")
  # The intervals stay in ASCENDING (f1, f2) order while the bars are drawn
  # reversed (f2, f1) - which is the defect. So the leftmost interval carries
  # f1, on the bar that draws f2.
  cell <- d[d$x == "A" & d$ff == "f1", "y"]
  expect_equal(as.numeric(ed$ymin[1]),
               mean(cell) - stats::sd(cell) / sqrt(length(cell)),
               tolerance = 1e-9)
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
