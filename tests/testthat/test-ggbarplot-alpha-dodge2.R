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
