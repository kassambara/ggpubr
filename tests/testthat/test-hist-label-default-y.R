# `label=` is documented on gghistogram() and ggdensity() and errored on the
# functions' own default `y`. The label path tested for the older `..count..`
# spelling while the formals default to the bare `count`, so the default fell
# through and the code looked for a data column named "count".
#
# The bare names cannot be resolved from here at all. `gghistogram(y = "count")`
# means the bar height and `ggtext(y = "count", ggp = <that histogram>)` means the
# caller's own column, and both arrive with the same data, the same y and the same
# plot. The calling function therefore says which it is; two earlier attempts to
# infer it -- from the data's column names, then from the built plot's mapping --
# each got one of the pair right and the other silently wrong.

labelled_layer <- function(p) {
  built <- ggplot2::ggplot_build(p)
  i <- which(vapply(p$layers, function(l) inherits(l$geom, "GeomText"), logical(1)))
  expect_length(i, 1)
  built$data[[i]]
}

# Read the drawn heights off the built distribution layer, so the expectation
# does not come from the same helper the code under test uses. Asserting only
# "not NA" would pass for any constant, which is what an earlier version of these
# tests did.
drawn_heights <- function(p) unique(ggplot2::ggplot_build(p)$data[[1]]$y)

test_that("gghistogram(label=) works on its own default y", {
  d <- data.frame(v = c(1, 2, 3, 4, 5, 6, 7, 8), lab = letters[1:8])
  p <- gghistogram(d, x = "v", label = "lab", bins = 4)
  lab <- labelled_layer(p)

  expect_true(nrow(lab) > 0)
  expect_true(all(lab$label %in% d$lab))
  expect_false(any(is.na(lab$y)))
  # every label sits at a height the histogram actually drew ...
  expect_true(all(lab$y %in% drawn_heights(p)))
  # ... and not at the raw x values, which is the most likely wrong answer
  expect_false(setequal(sort(lab$y), sort(d$v)))
})

test_that("ggdensity(label=) works on its own default y", {
  d <- data.frame(v = c(1, 2, 3, 4, 5, 6, 7, 8), lab = letters[1:8])
  p <- ggdensity(d, x = "v", label = "lab")
  lab <- labelled_layer(p)

  expect_true(nrow(lab) > 0)
  expect_true(all(lab$label %in% d$lab))
  expect_false(any(is.na(lab$y)))
  expect_true(all(lab$y %in% drawn_heights(p)))
  expect_false(setequal(sort(lab$y), sort(d$v)))
})

test_that("a vector label survives the computed-height path", {
  # `label` is documented as "a column name or a vector of length = nrow(data)".
  # The vector is stashed as an extra column on ggtext()'s local frame, but the
  # plot arrives already built from the caller's data -- so reading the plot's
  # own frame dropped it and every annotation rendered as the literal column
  # name. Assert the drawn labels are the supplied values.
  d <- data.frame(v = c(1, 2, 3, 4, 5, 6, 7, 8))
  labs <- c("aa", "bb", "cc", "dd", "ee", "ff", "gg", "hh")

  for (p in list(
    gghistogram(d, x = "v", label = labs, bins = 4),
    gghistogram(d, x = "v", y = "..count..", label = labs, bins = 4),
    ggdensity(d, x = "v", label = labs)
  )) {
    drawn <- labelled_layer(p)$label
    expect_true(all(drawn %in% labs))
    expect_false(any(drawn == "label.xx"))
  }
})

test_that("a vector label survives faceting too", {
  d <- data.frame(v = c(1, 2, 3, 4, 5, 6, 7, 8), g = rep(c("A", "B"), each = 4))
  labs <- c("aa", "bb", "cc", "dd", "ee", "ff", "gg", "hh")

  drawn <- labelled_layer(gghistogram(d, x = "v", label = labs, facet.by = "g", bins = 4))$label
  expect_true(all(drawn %in% labs))
  expect_false(any(drawn == "label.xx"))
})

test_that("each label takes the height of the bar that contains it", {
  # The heights were previously found by cutting on bar CENTRES, so any value
  # between two centres was annotated at the neighbouring bar's height. It is
  # invisible when the data sit on the centres, which is why the x values here
  # deliberately do not: on this data five of twelve labels were wrong.
  v <- c(0.4, 1.5, 1.9, 2.4, 3.5, 3.9, 4.4, 5.5, 5.9, 6.4, 7.5, 7.9)
  d <- data.frame(v = v, lab = paste0("p", seq_along(v)))

  p <- gghistogram(d, x = "v", label = "lab", bins = 4)
  built <- ggplot2::ggplot_build(p)
  hist <- built$data[[1]]
  lab <- labelled_layer(p)

  expect_equal(nrow(lab), nrow(d))
  for (k in seq_len(nrow(lab))) {
    containing <- which(lab$x[k] >= hist$xmin & lab$x[k] <= hist$xmax)[1]
    expect_false(is.na(containing))
    expect_equal(lab$y[k], hist$y[containing])
  }
})

test_that("density labels sit on the drawn curve", {
  # A density layer has no bar interval to fall inside, so the height is read
  # off the curve at the observation's x.
  v <- c(0.4, 1.5, 1.9, 2.4, 3.5, 3.9, 4.4, 5.5, 5.9, 6.4, 7.5, 7.9)
  d <- data.frame(v = v, lab = paste0("p", seq_along(v)))

  p <- ggdensity(d, x = "v", label = "lab")
  built <- ggplot2::ggplot_build(p)
  curve <- built$data[[1]]
  lab <- labelled_layer(p)

  expect_equal(nrow(lab), nrow(d))
  on_curve <- stats::approx(curve$x, curve$y, xout = lab$x, rule = 2)$y
  expect_equal(lab$y, on_curve, tolerance = 1e-8)
})

test_that("faceted labels sit at heights drawn in their own panel", {
  # .hist_label_data() now takes the caller's frame rather than the built plot's.
  # If those two could diverge in row count or order, a label could land at
  # another panel's height and the plot would still build -- so assert per panel,
  # not merely that a height was found somewhere.
  per_panel_ok <- function(p) {
    built <- ggplot2::ggplot_build(p)
    i <- which(vapply(p$layers, function(l) inherits(l$geom, "GeomText"), logical(1)))
    hist <- built$data[[1]]
    lab <- built$data[[i]]
    all(vapply(
      unique(lab$PANEL),
      function(pn) all(lab$y[lab$PANEL == pn] %in% unique(hist$y[hist$PANEL == pn])),
      logical(1)
    ))
  }

  dg <- data.frame(
    v = c(1, 2, 3, 4, 5, 6, 7, 8), lab = letters[1:8],
    g = rep(c("A", "B"), each = 4)
  )
  d4 <- transform(dg, h = rep(c("x", "y"), times = 4))

  expect_true(per_panel_ok(gghistogram(dg, x = "v", label = "lab", facet.by = "g", bins = 4)))
  expect_true(per_panel_ok(gghistogram(dg, x = "v", label = letters[8:1], facet.by = "g", bins = 4)))
  expect_true(per_panel_ok(gghistogram(d4, x = "v", label = "lab", facet.by = c("g", "h"), bins = 4)))
  expect_true(per_panel_ok(ggdensity(dg, x = "v", label = "lab", facet.by = "g")))
})

test_that("label.select still works on the computed-height path", {
  d <- data.frame(v = c(1, 2, 3, 4, 5, 6, 7, 8), lab = letters[1:8])

  p <- gghistogram(d, x = "v", label = "lab", bins = 4, label.select = c("a", "c"))
  expect_setequal(labelled_layer(p)$label, c("a", "c"))

  # and the other selection form still reaches the same path
  expect_s3_class(
    ggplot2::ggplot_build(
      gghistogram(d, x = "v", label = "lab", bins = 4, label.select = list(top.up = 2))
    ),
    "ggplot_built"
  )
})

test_that("after_stat() spellings label as well as they draw", {
  # These build fine without a label, so refusing them once a label is added is
  # an inconsistency rather than a documented limit.
  d <- data.frame(v = c(1, 2, 3, 4, 5, 6, 7, 8), lab = letters[1:8])

  p <- gghistogram(d, x = "v", y = "after_stat(count)", label = "lab", bins = 4)
  expect_true(all(labelled_layer(p)$y %in% drawn_heights(p)))

  p <- ggdensity(d, x = "v", y = "after_stat(density)", label = "lab")
  expect_true(all(labelled_layer(p)$y %in% drawn_heights(p)))
})

test_that("the explicit dot-dot spelling still works", {
  d <- data.frame(v = c(1, 2, 3, 4, 5, 6, 7, 8), lab = letters[1:8])
  expect_s3_class(
    ggplot2::ggplot_build(gghistogram(d, x = "v", y = "..count..", label = "lab", bins = 4)),
    "ggplot_built"
  )
})

test_that("a data column named count does not capture the histogram labels", {
  # An earlier version of this file asserted the opposite, and was wrong. The
  # bars NEVER honour such a column: gghistogram() rewrites y to
  # after_stat(count) before building, whether y was defaulted or passed. Reading
  # the label height from the column therefore floated the labels away from the
  # bars they annotate.
  d <- data.frame(
    v = c(1, 2, 3, 4, 5, 6, 7, 8),
    count = c(5, 3, 8, 1, 9, 2, 7, 4),
    lab = letters[1:8]
  )

  for (p in list(
    gghistogram(d, x = "v", label = "lab", bins = 4),
    gghistogram(d, x = "v", y = "count", label = "lab", bins = 4)
  )) {
    lab <- labelled_layer(p)
    expect_true(all(lab$y %in% drawn_heights(p)))
    expect_false(setequal(sort(lab$y), sort(d$count)))
  }
})

test_that("ggtext() called directly still plots a real column as y", {
  # The same word means different things by caller, which is why the decision is
  # taken from the built mapping rather than the argument string: here y really
  # is a column and the labels belong at its values.
  d <- data.frame(
    v = c(1, 2, 3, 4, 5, 6, 7, 8),
    count = c(5, 3, 8, 1, 9, 2, 7, 4),
    lab = letters[1:8]
  )
  lab <- labelled_layer(ggtext(d, x = "v", y = "count", label = "lab"))
  expect_setequal(round(sort(lab$y), 6), round(sort(d$count), 6))
})

test_that("a data column named density does not capture the density labels", {
  d <- data.frame(
    v = c(1, 2, 3, 4, 5, 6, 7, 8),
    density = c(0.5, 0.3, 0.8, 0.1, 0.9, 0.2, 0.7, 0.4),
    lab = letters[1:8]
  )
  lab <- labelled_layer(ggdensity(d, x = "v", y = "density", label = "lab"))
  expect_false(setequal(round(sort(lab$y), 6), round(sort(d$density), 6)))
})

test_that("plots without labels are unaffected either way", {
  d  <- data.frame(v = c(1, 2, 3, 4, 5, 6, 7, 8))
  dc <- data.frame(v = c(1, 2, 3, 4, 5, 6, 7, 8), count = c(5, 3, 8, 1, 9, 2, 7, 4))

  expect_s3_class(ggplot2::ggplot_build(gghistogram(d, x = "v", bins = 4)), "ggplot_built")
  expect_s3_class(ggplot2::ggplot_build(gghistogram(dc, x = "v", y = "count", bins = 4)), "ggplot_built")
  expect_s3_class(ggplot2::ggplot_build(ggdensity(d, x = "v")), "ggplot_built")
})

test_that("after_stat() is recognised whatever the spacing", {
  # The drawing side parses y as an expression, so it accepts spacing variants.
  # Matching the string literally here recognised only the canonical form, and a
  # plot that built fine errored the moment a label was added.
  d <- data.frame(v = c(1, 2, 3, 4, 5, 6, 7, 8), lab = letters[1:8])

  for (spelling in c("after_stat(count)", "after_stat( count )", "after_stat (count)")) {
    p <- gghistogram(d, x = "v", y = spelling, label = "lab", bins = 4)
    built <- ggplot2::ggplot_build(p)
    expect_true(all(labelled_layer(p)$y %in% unique(built$data[[1]]$y)), info = spelling)
  }
})

test_that("faceted labels follow their own panel's distribution, not its position", {
  # The annotation groups and the panels were paired by position. Annotations
  # nest in the order values first appear in the data; panels follow the factor's
  # level order. Where those disagree, every label takes another panel's heights
  # -- silently. Here panel "Z" is a tight cluster and panel "A" is spread, so a
  # swap is unmistakable.
  d <- data.frame(
    v = c(1, 1, 1, 1, 1.2, 5, 6, 7, 8, 9),
    lab = paste0("p", 1:10),
    g = factor(rep(c("Z", "A"), each = 5), levels = c("A", "Z"))
  )

  p <- gghistogram(d, x = "v", label = "lab", facet.by = "g", bins = 4)
  built <- ggplot2::ggplot_build(p)
  hist <- built$data[[1]]
  lab <- labelled_layer(p)

  expect_equal(nrow(lab), nrow(d))
  for (pn in unique(lab$PANEL)) {
    hp <- hist[hist$PANEL == pn, ]
    for (k in which(lab$PANEL == pn)) {
      containing <- which(lab$x[k] >= hp$xmin & lab$x[k] <= hp$xmax)[1]
      expect_false(is.na(containing))
      expect_equal(lab$y[k], hp$y[containing])
    }
  }
})

test_that("the pairing holds whichever way the factor levels run", {
  base <- data.frame(
    v = c(1, 1, 1, 1, 1.2, 5, 6, 7, 8, 9),
    lab = paste0("p", 1:10)
  )
  for (lv in list(c("A", "Z"), c("Z", "A"))) {
    d <- base
    d$g <- factor(rep(c("Z", "A"), each = 5), levels = lv)
    p <- gghistogram(d, x = "v", label = "lab", facet.by = "g", bins = 4)
    built <- ggplot2::ggplot_build(p)
    hist <- built$data[[1]]
    lab <- labelled_layer(p)
    ok <- vapply(seq_len(nrow(lab)), function(k) {
      hp <- hist[hist$PANEL == lab$PANEL[k], ]
      j <- which(lab$x[k] >= hp$xmin & lab$x[k] <= hp$xmax)[1]
      !is.na(j) && isTRUE(all.equal(hp$y[j], lab$y[k]))
    }, logical(1))
    expect_true(all(ok), info = paste(lv, collapse = ","))
  }
})

test_that("a sparse two-way facet grid still labels every panel", {
  # The layout carries a row for every combination of facet levels, including
  # those with no data, while the built distribution has entries only for the
  # panels that were drawn. Masking the distributions with a vector built over
  # the layout mis-indexes the shorter list and the call errors.
  d <- data.frame(
    v = c(1, 2, 3, 4, 6, 7, 8, 9),
    lab = paste0("p", 1:8),
    g = rep(c("A", "B"), each = 4),
    h = rep(c("x", "y"), each = 4)
  )
  expect_equal(nrow(unique(d[, c("g", "h")])), 2) # 2 of 4 combinations present

  p <- gghistogram(d, x = "v", label = "lab", facet.by = c("g", "h"), bins = 3)
  built <- ggplot2::ggplot_build(p)
  hist <- built$data[[1]]
  lab <- labelled_layer(p)

  expect_equal(nrow(built$layout$layout), 4)
  expect_equal(length(unique(hist$PANEL)), 2)
  expect_equal(nrow(lab), nrow(d))
  for (k in seq_len(nrow(lab))) {
    hp <- hist[hist$PANEL == lab$PANEL[k], ]
    containing <- which(lab$x[k] >= hp$xmin & lab$x[k] <= hp$xmax)[1]
    expect_false(is.na(containing))
    expect_equal(lab$y[k], hp$y[containing])
  }
})

test_that("any after_stat() expression reaches the computed-height path", {
  # Listing exact spellings kept missing forms the drawing side accepts. The
  # call itself is the signal, not the expression inside it.
  d <- data.frame(v = c(1, 2, 3, 4, 5, 6, 7, 8), lab = letters[1:8])

  for (spelling in c(
    "after_stat(count)", "after_stat( count )",
    "after_stat(density)", "after_stat(density * width)"
  )) {
    p <- gghistogram(d, x = "v", y = spelling, label = "lab", bins = 4)
    built <- ggplot2::ggplot_build(p)
    expect_true(all(labelled_layer(p)$y %in% unique(built$data[[1]]$y)), info = spelling)
  }
})

test_that("an explicit annotation y survives being added to a histogram", {
  # ggtext() adds labels to a plot supplied through `ggp`. That plot's own y is
  # a computed height, but the caller's y here names a column to annotate at.
  # Deciding from the plot's mapping alone moved these labels onto the bars.
  d <- data.frame(
    v = c(1, 2, 3, 4, 5, 6, 7, 8),
    annotation_y = c(50, 30, 80, 10, 90, 20, 70, 40),
    lab = letters[1:8]
  )
  p <- ggtext(d,
    x = "v", y = "annotation_y", label = "lab",
    ggp = gghistogram(d, x = "v", bins = 4)
  )
  expect_setequal(round(sort(labelled_layer(p)$y), 6), round(sort(d$annotation_y), 6))
})

test_that("after_stat is recognised when namespaced or nested", {
  # The drawing side computes these; matching the text of the call missed them,
  # so the plot built until a label was added.
  d <- data.frame(v = c(1, 2, 3, 4, 5, 6, 7, 8), lab = letters[1:8])

  for (spelling in c("ggplot2::after_stat(count)", "sqrt(after_stat(count))")) {
    p <- gghistogram(d, x = "v", y = spelling, label = "lab", bins = 4)
    built <- ggplot2::ggplot_build(p)
    # assert the labels landed on drawn heights, not merely that it built
    expect_true(all(labelled_layer(p)$y %in% unique(built$data[[1]]$y)), info = spelling)
  }
})

test_that("facet values are matched column-wise, not through a serialised key", {
  # A separator or missing-value marker is itself a legal facet value, so any
  # serialised key can collide with real data.
  v <- c(1, 2, 3, 4, 6, 7, 8, 9)
  lb <- paste0("p", 1:8)
  awkward <- list(
    c(rep("NA", 4), rep(NA, 4)),
    rep(c("a\002b", "c"), each = 4),
    rep(c("\001absent\001", "x"), each = 4)
  )

  for (g in awkward) {
    d <- data.frame(v = v, lab = lb, g = g)
    p <- gghistogram(d, x = "v", label = "lab", facet.by = "g", bins = 3)
    built <- ggplot2::ggplot_build(p)
    hist <- built$data[[1]]
    lab <- labelled_layer(p)
    expect_equal(nrow(lab), nrow(d))
    for (k in seq_len(nrow(lab))) {
      hp <- hist[hist$PANEL == lab$PANEL[k], ]
      containing <- which(lab$x[k] >= hp$xmin & lab$x[k] <= hp$xmax)[1]
      expect_false(is.na(containing))
      expect_equal(lab$y[k], hp$y[containing])
    }
  }
})

test_that("a bare count or density column is the caller's own when ggtext is direct", {
  # The pair that cannot be told apart by inference: same data, same y, same
  # plot. Here the caller is annotating an existing histogram with a column that
  # happens to be called count, and means that column.
  d <- data.frame(
    v = c(1, 2, 3, 4, 5, 6, 7, 8),
    count = c(50, 30, 80, 10, 90, 20, 70, 40),
    density = c(5, 3, 8, 1, 9, 2, 7, 4),
    lab = letters[1:8]
  )

  for (col in c("count", "density")) {
    p <- ggtext(d, x = "v", y = col, label = "lab", ggp = gghistogram(d, x = "v", bins = 4))
    expect_setequal(round(sort(labelled_layer(p)$y), 6), round(sort(d[[col]]), 6))
  }
})

test_that("the same names are computed heights when gghistogram asks for them", {
  # The other half of the pair, so the two are locked together and neither can
  # be fixed by breaking the other.
  d <- data.frame(
    v = c(1, 2, 3, 4, 5, 6, 7, 8),
    count = c(50, 30, 80, 10, 90, 20, 70, 40),
    lab = letters[1:8]
  )

  for (p in list(
    gghistogram(d, x = "v", label = "lab", bins = 4),
    gghistogram(d, x = "v", y = "count", label = "lab", bins = 4)
  )) {
    expect_true(all(labelled_layer(p)$y %in% drawn_heights(p)))
  }
})
