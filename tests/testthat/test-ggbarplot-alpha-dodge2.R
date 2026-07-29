context("test-ggbarplot-alpha-dodge2")

# #404 under position_dodge2(). Mapping `alpha` to a discrete column used to fail
# at draw with "alpha * 255": the summary dropped the column, so its NAME reached
# grid as a static opacity. Carrying the column splits the summary into one row
# per (x, legend, alpha) cell, which dodge2's re-centring (#363) could not place,
# because it matched summary rows to bars by SORT POSITION and the released key
# (PANEL, x, legend) is not total once the alpha subgroup exists. It is now given
# the full discrete key and CHECKS the pairing it produced against the bars.
#
# The fixture makes every cell mean AND every cell standard error distinct, so an
# error bar drawn on a neighbouring bar cannot pass by carrying a value that
# happens to match: both endpoints move. Each cell is c(m - s, m, m + s), whose
# mean is exactly m and whose se is exactly s/sqrt(3).

.mk <- function() {
  cells <- expand.grid(
    g = c("x1", "x2", "x3"), f = c("f1", "f2"), a = c("a1", "a2"),
    stringsAsFactors = FALSE
  )
  cells$m <- seq(10, by = 9, length.out = nrow(cells))   # 12 distinct means
  cells$s <- seq(1.5, by = 0.7, length.out = nrow(cells)) # 12 distinct spreads
  do.call(rbind, lapply(seq_len(nrow(cells)), function(i) {
    data.frame(
      g = cells$g[i], f = cells$f[i], a = cells$a[i],
      v = c(cells$m[i] - cells$s[i], cells$m[i], cells$m[i] + cells$s[i]),
      stringsAsFactors = FALSE
    )
  }))
}

# Independent reference: mean and se per cell from base R, never desc_statby().
.ref <- function(d) {
  m <- stats::aggregate(v ~ g + f + a, d, mean)
  s <- stats::aggregate(v ~ g + f + a, d, function(z) stats::sd(z) / sqrt(length(z)))
  names(m)[4] <- "mean"; names(s)[4] <- "se"
  merge(m, s, by = c("g", "f", "a"))
}

.layer <- function(p, b, classes) {
  i <- which(vapply(p$layers, function(l) class(l$geom)[1] %in% classes, logical(1)))
  b$data[[i[1]]]
}

# For every error bar: the single bar whose rect contains it, that bar's drawn
# value, and the interval the error bar draws. Returns NA for a bar index when
# the error bar sits between bars, so "off its bar" cannot silently pass.
.pairing <- function(p) {
  # ggplot2 warns "Using alpha for a discrete variable is not advised" on every
  # build here. These assertions are about geometry, never about that condition,
  # so silencing it does not weaken them.
  b <- suppressWarnings(ggplot2::ggplot_build(p))
  bd <- .layer(p, b, "GeomBar")
  ed <- .layer(p, b, c("GeomErrorbar", "GeomPointrange", "GeomLinerange", "GeomCrossbar"))
  # A broken pairing makes the caller drop the error layer entirely. Say so,
  # instead of crashing in seq_len(nrow(NULL)) and reporting an unrelated error.
  if (is.null(ed) || !nrow(ed)) stop("no error layer was drawn")
  idx <- vapply(seq_len(nrow(ed)), function(i) {
    hit <- which(as.character(bd$PANEL) == as.character(ed$PANEL[i]) &
                   as.numeric(ed$x[i]) >= as.numeric(bd$xmin) - 1e-9 &
                   as.numeric(ed$x[i]) <= as.numeric(bd$xmax) + 1e-9)
    if (length(hit) == 1L) hit else NA_integer_
  }, integer(1))
  list(
    n.bars = nrow(bd), bar = idx, bar.y = as.numeric(bd$y)[idx],
    ymin = as.numeric(ed$ymin), ymax = as.numeric(ed$ymax)
  )
}

# Assert each error bar sits on its own bar and carries THAT bar's own mean+se.
.expect_on_own_bar <- function(p, d, info = NULL) {
  pr <- .pairing(p)
  ref <- .ref(d)
  expect_false(any(is.na(pr$bar)), info = info)          # none between bars
  expect_equal(sort(pr$bar), seq_len(pr$n.bars), info = info)  # a bijection
  for (i in seq_along(pr$bar)) {
    cell <- ref[abs(ref$mean - pr$bar.y[i]) < 1e-9, , drop = FALSE]
    expect_equal(nrow(cell), 1L, info = info)
    expect_equal(pr$ymin[i], cell$mean - cell$se, tolerance = 1e-9, info = info)
    expect_equal(pr$ymax[i], cell$mean + cell$se, tolerance = 1e-9, info = info)
  }
}

test_that("alpha + position_dodge2() renders instead of failing on alpha * 255 (#404)", {
  d <- .mk()
  p <- suppressWarnings(ggbarplot(d, "g", "v", fill = "f", alpha = "a",
    add = "mean_se", position = position_dodge2()))
  # The claim is that it no longer ERRORS at draw. ggplot2 separately warns that
  # a discrete alpha is not advised - that warning is released behaviour and is
  # not what is under test, so asserting silence would pin the wrong condition.
  expect_no_error(suppressWarnings(ggplot2::ggplotGrob(p)))
})

test_that("every dodge2 error bar sits on its own bar carrying its own mean and se (#404)", {
  d <- .mk()
  p <- suppressWarnings(ggbarplot(d, "g", "v", fill = "f", alpha = "a",
    add = "mean_se", position = position_dodge2()))
  .expect_on_own_bar(p, d)
})

test_that("the dodge2 pairing survives every reordering of the summary (#404)", {
  d <- .mk()
  calls <- list(
    `sort.val=desc` = function() ggbarplot(d, "g", "v", fill = "f", alpha = "a",
      add = "mean_se", position = position_dodge2(), sort.val = "desc"),
    `sort.by.groups=FALSE` = function() ggbarplot(d, "g", "v", fill = "f", alpha = "a",
      add = "mean_se", position = position_dodge2(), sort.val = "desc",
      sort.by.groups = FALSE),
    `shuffled rows` = function() ggbarplot(d[sample.int(nrow(d)), ], "g", "v",
      fill = "f", alpha = "a", add = "mean_se", position = position_dodge2()),
    # add.params$color naming the alpha column re-resolves the legend variable,
    # which used to decide the sort key on its own
    `add.params$color` = function() ggbarplot(d, "g", "v", fill = "f", alpha = "a",
      add = "mean_se", position = position_dodge2(), add.params = list(color = "a"))
  )
  set.seed(1)
  for (nm in names(calls)) {
    .expect_on_own_bar(suppressWarnings(calls[[nm]]()), d, info = nm)
  }
})

test_that("top = truncates bars and error bars together under dodge2 (#404)", {
  d <- .mk()
  p <- suppressWarnings(ggbarplot(d, "g", "v", fill = "f", alpha = "a",
    add = "mean_se", position = position_dodge2(), top = 8))
  pr <- .pairing(p)
  expect_equal(pr$n.bars, 8L)
  expect_equal(length(pr$ymin), 8L)
  .expect_on_own_bar(p, d)
})

test_that("error.plot = 'crossbar' draws a crossbar, not an errorbar, and aligns (#404)", {
  d <- .mk()
  p <- suppressWarnings(ggbarplot(d, "g", "v", fill = "f", alpha = "a",
    add = "mean_se", position = position_dodge2(), error.plot = "crossbar"))
  geoms <- vapply(p$layers, function(l) class(l$geom)[1], character(1))
  # .get_geom_error_function() has no "crossbar" case and would fall back to
  # geom_errorbar; the wrong geom must not be what gets drawn.
  expect_true("GeomCrossbar" %in% geoms)
  expect_false("GeomErrorbar" %in% geoms)
  .expect_on_own_bar(p, d)
  # Re-centring fixes the crossbar's centre but not its width: left at the
  # default it is drawn 0.9 wide over a 0.1575-wide bar, spanning the whole x
  # group. It has to take the width of the bar it belongs to.
  b <- suppressWarnings(ggplot2::ggplot_build(p))
  bd <- .layer(p, b, "GeomBar"); ed <- .layer(p, b, "GeomCrossbar")
  expect_equal(as.numeric(ed$xmax - ed$xmin), as.numeric(bd$xmax - bd$xmin),
               tolerance = 1e-9)
})

test_that("one-sided and point/line error plots also align under dodge2 with alpha (#404)", {
  d <- .mk()
  ref <- .ref(d)
  for (ep in c("pointrange", "linerange", "upper_errorbar", "lower_errorbar")) {
    p <- suppressWarnings(ggbarplot(d, "g", "v", fill = "f", alpha = "a",
      add = "mean_se", position = position_dodge2(), error.plot = ep))
    pr <- .pairing(p)
    expect_false(any(is.na(pr$bar)), info = ep)
    expect_equal(sort(pr$bar), seq_len(pr$n.bars), info = ep)
    for (i in seq_along(pr$bar)) {
      cell <- ref[abs(ref$mean - pr$bar.y[i]) < 1e-9, , drop = FALSE]
      lo <- if (ep == "upper_errorbar") cell$mean else cell$mean - cell$se
      hi <- if (ep == "lower_errorbar") cell$mean else cell$mean + cell$se
      expect_equal(pr$ymin[i], lo, tolerance = 1e-9, info = ep)
      expect_equal(pr$ymax[i], hi, tolerance = 1e-9, info = ep)
    }
  }
})

.mk_facet <- function() {
  # x1 is ABSENT from panel q, so with a free x scale panel q renumbers its
  # remaining levels and a fixed-scale probe would read centres the drawn panel
  # never uses. With the last level missing instead, the two coincide and the
  # bug hides - the dropped level has to be the first one.
  cells <- expand.grid(
    g = c("x1", "x2", "x3"), f = c("f1", "f2"), a = c("a1", "a2"),
    blk = c("p", "q"), stringsAsFactors = FALSE
  )
  cells <- cells[!(cells$blk == "q" & cells$g == "x1"), ]
  cells$m <- seq(10, by = 7, length.out = nrow(cells))
  cells$s <- seq(1.5, by = 0.4, length.out = nrow(cells))
  do.call(rbind, lapply(seq_len(nrow(cells)), function(i) {
    data.frame(
      g = cells$g[i], f = cells$f[i], a = cells$a[i], blk = cells$blk[i],
      v = c(cells$m[i] - cells$s[i], cells$m[i], cells$m[i] + cells$s[i]),
      stringsAsFactors = FALSE
    )
  }))
}

.expect_faceted_on_own_bar <- function(p, d, info = NULL) {
  pr <- .pairing(p)
  m <- stats::aggregate(v ~ g + f + a + blk, d, mean)
  s <- stats::aggregate(v ~ g + f + a + blk, d, function(z) stats::sd(z) / sqrt(length(z)))
  names(m)[5] <- "mean"; names(s)[5] <- "se"
  ref <- merge(m, s, by = c("g", "f", "a", "blk"))
  expect_false(any(is.na(pr$bar)), info = info)
  expect_equal(sort(pr$bar), seq_len(pr$n.bars), info = info)
  for (i in seq_along(pr$bar)) {
    cell <- ref[abs(ref$mean - pr$bar.y[i]) < 1e-9, , drop = FALSE]
    expect_equal(nrow(cell), 1L, info = info)
    expect_equal(pr$ymin[i], cell$mean - cell$se, tolerance = 1e-9, info = info)
    expect_equal(pr$ymax[i], cell$mean + cell$se, tolerance = 1e-9, info = info)
  }
}

test_that("dodge2 with alpha aligns under a FREE x scale, where a panel renumbers (#404)", {
  d <- .mk_facet()
  for (sc in c("fixed", "free_x", "free")) {
    p <- suppressWarnings(ggbarplot(d, "g", "v", fill = "f", alpha = "a",
      add = "mean_se", position = position_dodge2(), facet.by = "blk", scales = sc))
    .expect_faceted_on_own_bar(p, d, info = sc)
  }
})

test_that("`scales` is read through partial matching, as facet() receives it (#404)", {
  # scales reaches facet() through `...`, so `scale =` and `scal =` arrive as
  # `scales`. An exact-name lookup would silently probe fixed scales and put the
  # error bars on the wrong bars in exactly the free-scale case above.
  d <- .mk_facet()
  for (nm in c("scales", "scale", "scal")) {
    args <- list(d, "g", "v", fill = "f", alpha = "a", add = "mean_se",
                 position = position_dodge2(), facet.by = "blk")
    args[[nm]] <- "free_x"
    .expect_faceted_on_own_bar(suppressWarnings(do.call(ggbarplot, args)), d, info = nm)
  }
  # `s =` is ambiguous with short.panel.labs/strip.position, so it reaches
  # facet() no better than it reaches us: the default stands.
  expect_equal(ggpubr:::.facet_scales_from_dots(list(s = "free_x")), "fixed")
  expect_equal(ggpubr:::.facet_scales_from_dots(list(scal = "free_x")), "free_x")
  expect_equal(ggpubr:::.facet_scales_from_dots(list()), "fixed")
  # An EXACT name wins, and an abbreviation of an already-matched formal is left
  # over for `...` and ignored - so facet() here uses "free_x". Reading the last
  # partial hit instead gave "fixed" and probed a layout the plot never draws.
  expect_equal(
    ggpubr:::.facet_scales_from_dots(list(scales = "free_x", scale = "fixed")),
    "free_x"
  )
  expect_equal(
    ggpubr:::.facet_scales_from_dots(list(scale = "fixed", scales = "free_x")),
    "free_x"
  )
})

test_that("dodge2 with alpha aligns with two facet variables, which use facet_grid (#404)", {
  d <- .mk_facet()
  d$blk2 <- rep(c("u", "w"), length.out = nrow(d))
  pr <- NULL
  for (sc in c("fixed", "free_x")) {
    p <- suppressWarnings(ggbarplot(d, "g", "v", fill = "f", alpha = "a",
      add = "mean_se", position = position_dodge2(), facet.by = c("blk", "blk2"),
      scales = sc))
    pr <- .pairing(p)
    expect_false(any(is.na(pr$bar)), info = sc)
    expect_equal(sort(pr$bar), seq_len(pr$n.bars), info = sc)
  }
})

test_that("no-regression: a faceted dodge2 WITHOUT alpha keeps its released layout (#404)", {
  # The probe is only made facet-accurate on the alpha path. Without alpha this
  # call keeps exactly the positions it has always had, including under a free
  # x scale where the released probe is known to be imperfect - changing that is
  # a separate, pre-existing issue and not this fix's to make.
  d <- .mk_facet()
  # Measured on master (857f0b0) and pinned, so a future change is noticed.
  # Under a free x scale panel q renumbers its bars to 0.825..2.175 while the
  # released probe still reads the fixed-scale centres 1.825..3.175 - all four
  # of that panel's error bars are off their bars. That is a PRE-EXISTING defect
  # of the no-alpha path, unchanged here and deliberately left for its own fix.
  expected <- list(
    fixed = list(
      bar = c(0.825, 1.175, 1.825, 2.175, 2.825, 3.175, 1.825, 2.175, 2.825, 3.175),
      err = c(0.825, 1.175, 1.825, 2.175, 2.825, 3.175, 1.825, 2.175, 2.825, 3.175)
    ),
    free_x = list(
      bar = c(0.825, 1.175, 1.825, 2.175, 2.825, 3.175, 0.825, 1.175, 1.825, 2.175),
      err = c(0.825, 1.175, 1.825, 2.175, 2.825, 3.175, 1.825, 2.175, 2.825, 3.175)
    )
  )
  for (sc in names(expected)) {
    p <- ggbarplot(d, "g", "v", fill = "f", add = "mean_se",
                   position = position_dodge2(), facet.by = "blk", scales = sc)
    b <- suppressWarnings(ggplot2::ggplot_build(p))
    bd <- .layer(p, b, "GeomBar"); ed <- .layer(p, b, "GeomErrorbar")
    expect_equal(round(as.numeric(bd$x), 4), expected[[sc]]$bar, info = sc)
    expect_equal(round(as.numeric(ed$x), 4), expected[[sc]]$err, info = sc)
  }
})

test_that("dodge2(reverse = TRUE) pairs each interval with its own bar (#404)", {
  # collide2() sorts each x by -group when reverse = TRUE, so ordering the
  # summary ascending matched every row to the MIRROR bar. The centre check
  # caught it and refused, and the caller then drew an unpaired layer - four
  # intervals clustered at the tick centre, two bars carrying two each and two
  # none. The sort follows `reverse` now, so it aligns instead of bailing.
  d <- .mk()
  p <- suppressWarnings(ggbarplot(d, "g", "v", fill = "f", alpha = "a",
    add = "mean_se", position = position_dodge2(reverse = TRUE)))
  .expect_on_own_bar(p, d)
  b <- suppressWarnings(ggplot2::ggplot_build(p))
  expect_equal(as.numeric(.layer(p, b, "GeomErrorbar")$x),
               as.numeric(.layer(p, b, "GeomBar")$x), tolerance = 1e-9)
})

test_that("cells that share a mean still pair with their own bar (#404)", {
  # An earlier revision refused a centre tied within one (panel, x), reasoning
  # that a swap could move a different half-width onto each bar. That was
  # backwards - the order comes from the discrete key ggplot2 groups the bars
  # by, so it is right whether or not the centres tie; refusing only sent
  # ordinary rounded/count data down the unpaired path. Two cells here share a
  # mean of 10 while their standard errors differ.
  d <- data.frame(
    g = rep(c("A", "B"), each = 12),
    f = rep(rep(c("f1", "f2"), each = 6), 2),
    a = rep(rep(c("a1", "a2"), each = 3), 4),
    v = c(9, 10, 11,  38, 40, 42,  19, 20, 21,  58, 60, 62,
          29, 30, 31, 68, 70, 72,  49, 50, 51,  88, 90, 92),
    stringsAsFactors = FALSE
  )
  d$v[d$g == "A" & d$f == "f1" & d$a == "a2"] <- c(5, 10, 15)
  p <- suppressWarnings(ggbarplot(d, "g", "v", fill = "f", alpha = "a",
    add = "mean_se", position = position_dodge2()))
  pr <- .pairing(p)
  expect_false(any(is.na(pr$bar)))
  expect_equal(sort(pr$bar), seq_len(pr$n.bars))

  ref <- merge(
    stats::aggregate(v ~ g + f + a, d, mean),
    stats::aggregate(v ~ g + f + a, d, function(z) stats::sd(z) / sqrt(length(z))),
    by = c("g", "f", "a")
  )
  names(ref)[4:5] <- c("mean", "se")

  # Identify each bar by the aesthetics IT was drawn with, decoded through
  # ggplot2's own scales - NOT by its height. Looking the cell up by height
  # returns BOTH tied cells, and accepting either makes the assertion pass just
  # as happily when the two tied intervals are swapped, which is the one failure
  # this test exists to catch.
  b <- suppressWarnings(ggplot2::ggplot_build(p))
  bd <- .layer(p, b, "GeomBar")
  f.lv <- levels(factor(d$f)); a.lv <- levels(factor(d$a))
  f.map <- stats::setNames(b$plot$scales$get_scales("fill")$map(f.lv), f.lv)
  a.map <- stats::setNames(b$plot$scales$get_scales("alpha")$map(a.lv), a.lv)
  x.lab <- as.character(b$layout$panel_params[[1]]$x$get_labels())

  for (i in seq_along(pr$bar)) {
    j <- pr$bar[i]
    cell.g <- x.lab[round(as.numeric(bd$x[j]))]
    cell.f <- names(f.map)[match(as.character(bd$fill[j]), as.character(f.map))]
    cell.a <- names(a.map)[which.min(abs(a.map - as.numeric(bd$alpha[j])))]
    expect_false(is.na(cell.g) || is.na(cell.f) || length(cell.a) != 1L)
    cell <- ref[ref$g == cell.g & ref$f == cell.f & ref$a == cell.a, , drop = FALSE]
    expect_equal(nrow(cell), 1L)
    expect_equal(pr$ymin[i], cell$mean - cell$se, tolerance = 1e-9)
    expect_equal(pr$ymax[i], cell$mean + cell$se, tolerance = 1e-9)
  }
})

test_that("an asymmetric summary keeps the released refusal under dodge2 (#404)", {
  # median_q1q3 / median_hilow are quantile PAIRS, not centre +/- error, so the
  # summary has no half-width column and the re-centring helper cannot place
  # them. Their intervals are correct but unpaired, and with the subgroup
  # carried "unpaired" means drawn inside another cell's bar - 4 of 8 measured.
  d <- .mk()
  for (f in c("median_q1q3", "median_hilow")) {
    p <- suppressWarnings(ggbarplot(d, "g", "v", fill = "f", alpha = "a",
      add = f, position = position_dodge2()))
    b <- suppressWarnings(ggplot2::ggplot_build(p))
    expect_equal(nrow(.layer(p, b, "GeomBar")), 6L, info = f)
    expect_error(ggplot2::ggplotGrob(p), info = f)
  }
  # a symmetric one on the same data is unaffected: it draws, and every interval
  # is its own cell's median +/- IQR (the ggpubr convention is the FULL IQR on
  # each side, not Q1-Q3), checked against base R
  p <- suppressWarnings(ggbarplot(d, "g", "v", fill = "f", alpha = "a",
    add = "median_iqr", position = position_dodge2()))
  pr <- .pairing(p)
  expect_false(any(is.na(pr$bar)))
  expect_equal(sort(pr$bar), seq_len(pr$n.bars))
  ref <- merge(
    stats::aggregate(v ~ g + f + a, d, stats::median),
    stats::aggregate(v ~ g + f + a, d, stats::IQR),
    by = c("g", "f", "a")
  )
  names(ref)[4:5] <- c("mid", "iqr")
  for (i in seq_along(pr$bar)) {
    cell <- ref[abs(ref$mid - pr$bar.y[i]) < 1e-9, , drop = FALSE]
    expect_equal(nrow(cell), 1L)
    expect_equal(pr$ymin[i], cell$mid - cell$iqr, tolerance = 1e-9)
    expect_equal(pr$ymax[i], cell$mid + cell$iqr, tolerance = 1e-9)
  }
})

test_that("a re-centred crossbar keeps the mapped fill, not add.params$fill (#404)", {
  # Every other ggpubr crossbar takes its fill from the mapped fill aesthetic.
  # ggbarplot_core() defaults add.params$fill to "white" before
  # .check_add.params() can set it, so reading that turned a released, working
  # call's crossbars from the group colours to white - and a white cap truncates
  # the coloured bar at centre - error, so the bar reads lower than it is.
  tg <- ToothGrowth
  tg$dose <- factor(tg$dose)
  p <- suppressWarnings(ggbarplot(tg, "dose", "len", fill = "supp", alpha = "supp",
    add = "mean_se", position = position_dodge2(), error.plot = "crossbar"))
  b <- suppressWarnings(ggplot2::ggplot_build(p))
  bd <- .layer(p, b, "GeomBar"); ed <- .layer(p, b, "GeomCrossbar")
  expect_equal(sort(unique(as.character(ed$fill))), sort(unique(as.character(bd$fill))))
  expect_false(any(as.character(ed$fill) == "white"))
  # and it is centred on its bar, at the bar's width
  expect_equal(as.numeric(ed$x), as.numeric(bd$x), tolerance = 1e-9)
  expect_equal(as.numeric(ed$xmax - ed$xmin), as.numeric(bd$xmax - bd$xmin),
               tolerance = 1e-9)
})

test_that("the re-centred crossbar does not emit a spurious aesthetics warning (#404)", {
  # `width` is not in GeomCrossbar$aesthetics(), so layer() warns "Ignoring
  # unknown aesthetics: width" - but GeomErrorbar$setup_data(), which
  # GeomCrossbar delegates to, reads data$width first, so the mapping IS
  # honoured and the warning is wrong. Master never emitted it.
  d <- .mk()
  seen <- character(0)
  withCallingHandlers(
    invisible(ggbarplot(d, "g", "v", fill = "f", alpha = "a", add = "mean_se",
      position = position_dodge2(), error.plot = "crossbar")),
    warning = function(w) {
      seen <<- c(seen, conditionMessage(w))
      invokeRestart("muffleWarning")
    }
  )
  expect_false(any(grepl("unknown aesthetics", seen)))
  # ...and warnings are NOT blanket-suppressed: ggplot2's own advice about a
  # discrete alpha must still reach the user, or this test could not fail.
  expect_true(any(grepl("alpha for a discrete variable", seen)))
})

test_that("reverse is read the way collide2() reads it, not just isTRUE (#404)", {
  # ggplot2 branches on `if (reverse)`, which is TRUE for 1 and "TRUE" as well.
  # isTRUE() alone saw those as FALSE, so the summary was ordered ascending
  # against bars laid out descending: the pairing check then refused and the
  # error layer was dropped entirely. Only `reverse = TRUE` was covered before,
  # so the coercion could have been reverted with the suite still green.
  d <- .mk()
  for (rv in list(TRUE, 1, "TRUE")) {
    p <- suppressWarnings(ggbarplot(d, "g", "v", fill = "f", alpha = "a",
      add = "mean_se", position = position_dodge2(reverse = rv)))
    info <- paste("reverse =", format(rv))
    b <- suppressWarnings(ggplot2::ggplot_build(p))
    # the error layer must still be there - dropping it is the failure mode
    expect_equal(nrow(.layer(p, b, "GeomErrorbar")), 12L, info = info)
    .expect_on_own_bar(p, d, info = info)
  }
})

test_that("a re-centred crossbar tolerates add.params$color naming a column (#404)", {
  # `color.is.var` tests membership in the SUMMARY, so a data column that is not
  # a grouping variable falls through and would be passed to the geom as a
  # literal colour ("Unknown colour name: z"). The released crossbar dodged
  # itself and never reached that code, so such a call DREW, ignoring the
  # argument; routing the crossbar through the re-centring turned it into a
  # crash. It draws again.
  tg <- ToothGrowth
  tg$dose <- factor(tg$dose)
  tg$z <- rep(c("z1", "z2"), length.out = nrow(tg))
  expect_no_error(suppressWarnings(ggplot2::ggplotGrob(
    ggbarplot(tg, "dose", "len", fill = "supp", alpha = "supp", add = "mean_se",
              position = position_dodge2(), error.plot = "crossbar",
              add.params = list(color = "z"))
  )))
  # a real colour is still honoured
  p <- suppressWarnings(ggbarplot(tg, "dose", "len", fill = "supp", alpha = "supp",
    add = "mean_se", position = position_dodge2(), error.plot = "crossbar",
    add.params = list(color = "red")))
  b <- suppressWarnings(ggplot2::ggplot_build(p))
  expect_true(all(as.character(.layer(p, b, "GeomCrossbar")$colour) == "red"))
})

test_that("the key orders colour before fill, as ggplot2 lays the bars out (#404)", {
  # ggplot2's add_group() ids the bars over the layer's discrete columns in the
  # order they appear in the layer data - `colour` before `fill`. Keying on the
  # other order transposes the pairing as soon as `color` and `fill` name
  # DIFFERENT columns, which is the failure mode released ggpubr got right. No
  # other test in this file sets colour and fill to different columns alongside
  # alpha, so transposing the key left the whole suite green.
  #
  # Tied cell means make it bite hardest: the centre self-check cannot see a
  # swap between them, so only a correct key order gets the half-widths right.
  cells <- expand.grid(
    g = c("A", "B"), cc = c("c1", "c2"), f = c("f1", "f2"), a = c("a1", "a2"),
    stringsAsFactors = FALSE
  )
  cells$mid <- 50                                  # every cell shares a mean
  cells$s <- seq(1, by = 1.5, length.out = nrow(cells))  # every spread differs
  d <- do.call(rbind, lapply(seq_len(nrow(cells)), function(i) {
    data.frame(
      g = cells$g[i], cc = cells$cc[i], f = cells$f[i], a = cells$a[i],
      v = c(cells$mid[i] - cells$s[i], cells$mid[i], cells$mid[i] + cells$s[i]),
      stringsAsFactors = FALSE
    )
  }))
  p <- suppressWarnings(ggbarplot(d, "g", "v", color = "cc", fill = "f",
    alpha = "a", add = "mean_se", position = position_dodge2()))
  pr <- .pairing(p)
  expect_false(any(is.na(pr$bar)))
  expect_equal(sort(pr$bar), seq_len(pr$n.bars))

  ref <- merge(
    stats::aggregate(v ~ g + cc + f + a, d, mean),
    stats::aggregate(v ~ g + cc + f + a, d, function(z) stats::sd(z) / sqrt(length(z))),
    by = c("g", "cc", "f", "a")
  )
  names(ref)[5:6] <- c("mean", "se")

  # decode each bar from ITS OWN aesthetics, through ggplot2's scales
  b <- suppressWarnings(ggplot2::ggplot_build(p))
  bd <- .layer(p, b, "GeomBar")
  lv <- function(v) levels(factor(d[[v]]))
  c.map <- stats::setNames(b$plot$scales$get_scales("colour")$map(lv("cc")), lv("cc"))
  f.map <- stats::setNames(b$plot$scales$get_scales("fill")$map(lv("f")), lv("f"))
  a.map <- stats::setNames(b$plot$scales$get_scales("alpha")$map(lv("a")), lv("a"))
  x.lab <- as.character(b$layout$panel_params[[1]]$x$get_labels())

  for (i in seq_along(pr$bar)) {
    j <- pr$bar[i]
    cell <- ref[
      ref$g == x.lab[round(as.numeric(bd$x[j]))] &
        ref$cc == names(c.map)[match(as.character(bd$colour[j]), as.character(c.map))] &
        ref$f == names(f.map)[match(as.character(bd$fill[j]), as.character(f.map))] &
        ref$a == names(a.map)[which.min(abs(a.map - as.numeric(bd$alpha[j])))], ,
      drop = FALSE
    ]
    expect_equal(nrow(cell), 1L)
    expect_equal(pr$ymin[i], cell$mean - cell$se, tolerance = 1e-9)
    expect_equal(pr$ymax[i], cell$mean + cell$se, tolerance = 1e-9)
  }
})

test_that("an NA key level keeps its trailing rank, in both directions (#404)", {
  # interaction(addNA(...)) keeps NA as a real TRAILING level and ggplot2's
  # id_var(drop = TRUE) sorts na.last, so the ordering gives NA that same
  # trailing rank explicitly instead of letting order() place it. Removing that
  # one line leaves the whole suite green while the pairing goes wrong - here
  # the error layer is dropped entirely, and on other arrangements two
  # intervals simply swap. Under `reverse` the rank has to mirror with
  # everything else, which is why both directions are driven.
  cells <- expand.grid(xg = c("X1", "X2"), fi = c("f1", "f2"),
                       al = c("a1", NA), stringsAsFactors = FALSE)
  cells$m <- seq(20, by = 10, length.out = nrow(cells))
  cells$s <- seq(1, by = 1, length.out = nrow(cells))
  d <- do.call(rbind, lapply(seq_len(nrow(cells)), function(i) {
    data.frame(
      xg = cells$xg[i], fi = cells$fi[i], al = cells$al[i],
      v = c(cells$m[i] - cells$s[i], cells$m[i], cells$m[i] + cells$s[i]),
      stringsAsFactors = FALSE
    )
  }))
  k <- interaction(
    addNA(factor(d$xg), ifany = TRUE), addNA(factor(d$fi), ifany = TRUE),
    addNA(factor(d$al), ifany = TRUE), drop = TRUE
  )
  ref <- do.call(rbind, lapply(split(d$v, k), function(z) {
    data.frame(m = mean(z), s = stats::sd(z))
  }))

  for (rv in c(FALSE, TRUE)) {
    p <- suppressWarnings(ggbarplot(d, "xg", "v", fill = "fi", alpha = "al",
      add = "mean_sd", position = position_dodge2(reverse = rv)))
    b <- suppressWarnings(ggplot2::ggplot_build(p))
    bd <- .layer(p, b, "GeomBar")
    ed <- .layer(p, b, "GeomErrorbar")
    info <- paste("reverse =", rv)
    # the layer must exist at all - dropping it is one of the failure modes
    expect_false(is.null(ed), info = info)
    expect_equal(nrow(ed), nrow(bd), info = info)
    for (i in seq_len(nrow(ed))) {
      j <- which(as.numeric(ed$x[i]) >= as.numeric(bd$xmin) - 1e-9 &
                   as.numeric(ed$x[i]) <= as.numeric(bd$xmax) + 1e-9)
      expect_equal(length(j), 1L, info = info)
      q <- which(abs(ref$m - as.numeric(bd$y[j])) < 1e-9)
      expect_equal(length(q), 1L, info = info)
      expect_equal(ed$ymin[i], ref$m[q] - ref$s[q], tolerance = 1e-9, info = info)
      expect_equal(ed$ymax[i], ref$m[q] + ref$s[q], tolerance = 1e-9, info = info)
    }
  }
})

test_that("a key that cannot describe the bars keeps the released refusal (#404)", {
  # When a keyed column is not discrete, or is named after a desc_statby()
  # statistic, no ordering maps one error bar to one bar. Drawing anyway would
  # put an interval beside a bar it was not computed from, so these keep the
  # released behaviour of not drawing at all. Text is not pinned - it comes from
  # ggplot2 and is translated.
  d <- .mk()
  dnum <- d; dnum$fnum <- as.numeric(factor(dnum$f))
  p1 <- suppressWarnings(ggbarplot(dnum, "g", "v", fill = "fnum", alpha = "a",
    add = "mean_se", position = position_dodge2()))
  expect_error(ggplot2::ggplotGrob(p1))

  dnum2 <- d; dnum2$anum <- as.integer(factor(dnum2$a))
  p2 <- suppressWarnings(ggbarplot(dnum2, "g", "v", fill = "f", alpha = "anum",
    add = "mean_se", position = position_dodge2()))
  expect_error(ggplot2::ggplotGrob(p2))
})

test_that("label = TRUE keeps the released behaviour rather than floating labels (#404)", {
  # The value labels are drawn by their own layer, which dodges on the legend key
  # alone. With the alpha subgroup carried they land BETWEEN the bars - measured
  # 4 of 8 over the bar whose value they show. Aligning the error bars while half
  # the numbers float would make the figure look trustworthy and read wrong, so a
  # labelled call is left exactly as released (it does not draw) until the label
  # layer is keyed too. Same reasoning the plain-dodge fix recorded for `label`.
  d <- .mk()
  p <- suppressWarnings(ggbarplot(d, "g", "v", fill = "f", alpha = "a",
    add = "mean_se", position = position_dodge2(), label = TRUE))
  b <- suppressWarnings(ggplot2::ggplot_build(p))
  # the alpha column is NOT carried: one bar per (x, fill), as before
  expect_equal(nrow(.layer(p, b, "GeomBar")), 6L)
  expect_error(ggplot2::ggplotGrob(p))
  # a character label vector is a label too, and is gated the same way
  p2 <- suppressWarnings(ggbarplot(d, "g", "v", fill = "f", alpha = "a",
    add = "mean_se", position = position_dodge2(),
    label = as.character(seq_len(6))))
  expect_equal(nrow(.layer(p2, suppressWarnings(ggplot2::ggplot_build(p2)), "GeomBar")), 6L)
  # and label = FALSE is unaffected: the subgroup is carried and aligned
  .expect_on_own_bar(suppressWarnings(ggbarplot(d, "g", "v", fill = "f",
    alpha = "a", add = "mean_se", position = position_dodge2(), label = FALSE)), d)
})

test_that("a raw-data layer keeps the released behaviour rather than stray points (#404)", {
  # jitter/point/dotplot/boxplot/violin draw the OBSERVATIONS, placed by ggadd()
  # under the same position - and position_dodge2() packs by each element's own
  # width, which a point does not have. They already sit off their own bar with
  # no alpha at all (8 of 12, pre-existing and unchanged); splitting the bars
  # finer takes it to 12 of 24, half the observations over a bar they are not
  # from. So this combination is left exactly as released.
  d <- .mk()
  for (extra in c("jitter", "point", "dotplot", "boxplot", "violin")) {
    p <- suppressWarnings(ggbarplot(d, "g", "v", fill = "f", alpha = "a",
      add = c("mean_se", extra), position = position_dodge2()))
    b <- suppressWarnings(ggplot2::ggplot_build(p))
    # the alpha column is NOT carried: one bar per (x, fill)
    expect_equal(nrow(.layer(p, b, "GeomBar")), 6L, info = extra)
  }
  # the summary alone is unaffected, with or without an explicit "none"
  .expect_on_own_bar(suppressWarnings(ggbarplot(d, "g", "v", fill = "f",
    alpha = "a", add = "mean_se", position = position_dodge2())), d)
})

test_that("no-regression: dodge2 WITHOUT alpha is untouched by the alpha path (#404)", {
  # Pinned absolute positions, not a sorted set: a permutation of the same values
  # would satisfy a set comparison. Values measured on the released path.
  d <- .mk()
  p <- ggbarplot(d, "g", "v", fill = "f", add = "mean_se",
                 position = position_dodge2())
  b <- ggplot2::ggplot_build(p)
  bd <- .layer(p, b, "GeomBar")
  ed <- .layer(p, b, "GeomErrorbar")
  expect_equal(as.numeric(ed$x), as.numeric(bd$x), tolerance = 1e-9)
  expect_equal(round(as.numeric(bd$x), 4),
               c(0.8250, 1.1750, 1.8250, 2.1750, 2.8250, 3.1750))
  # and the summary is NOT split by anything else
  expect_equal(nrow(bd), 6L)
})

test_that("no-regression: plain position_dodge() with alpha keeps its own key (#404)", {
  d <- .mk()
  p <- suppressWarnings(ggbarplot(d, "g", "v", fill = "f", alpha = "a",
    add = "mean_se", position = position_dodge(0.9)))
  .expect_on_own_bar(p, d)
})
