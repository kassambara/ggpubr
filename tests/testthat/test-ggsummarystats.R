
test_that("ggsummarytable forwards angle to the text layer (#595)", {
  # Regression: angle rotates the summary-table text
  p90 <- ggsummarytable(ToothGrowth, x = "dose", y = "len",
                        summaries = "mean", angle = 90)
  expect_true(all(ggplot2::layer_data(p90, 1)$angle == 90))

  # No-regression: default (no angle) is unchanged (angle 0)
  p0 <- ggsummarytable(ToothGrowth, x = "dose", y = "len", summaries = "mean")
  expect_true(all(ggplot2::layer_data(p0, 1)$angle == 0))
})

test_that("ggsummarystats(free.panels) titles each panel with its own group", {
  # Each panel must be named for the group whose rows it draws. rstatix's
  # df_unite_factors() sorts the rows before building the label and the label is
  # assigned back onto the unsorted nested frame, so the panel titled "East" drew
  # North's data - a mislabelled result, with the summary table under it carrying
  # the transposed medians too. Bites for a character facet column that is not in
  # alphabetical order, and for labeller = "label_both" even with a factor.
  set.seed(1)
  d <- data.frame(
    region = rep(c("North", "East", "South"), each = 12),
    grp = rep(c("a", "b"), 18),
    val = c(stats::rnorm(12, 100), stats::rnorm(12, 10), stats::rnorm(12, 50)),
    stringsAsFactors = FALSE
  )
  # independent reference: the median of each (region, grp) cell, which is what
  # the box middles draw. Compared exactly - a relative tolerance of 1 would
  # accept a 100% error and certify nothing.
  truth <- tapply(d$val, list(d$region, d$grp), stats::median)

  for (lab in c("label_value", "label_both")) {
    for (as.factor.x in c(FALSE, TRUE)) {
      dd <- d
      if (as.factor.x) {
        dd$region <- factor(dd$region, levels = c("North", "East", "South"))
      }
      p <- suppressWarnings(ggsummarystats(dd, x = "grp", y = "val",
        facet.by = "region", free.panels = TRUE, labeller = lab))
      # panel order follows the data, not the alphabet, and the labeller's
      # own formatting is preserved
      expect_equal(
        names(p),
        if (lab == "label_both") {
          c("region:North", "region:East", "region:South")
        } else {
          c("North", "East", "South")
        }
      )
      for (nm in names(p)) {
        grp <- sub("^region:", "", nm)
        built <- suppressWarnings(ggplot2::ggplot_build(p[[nm]]$main.plot))
        # the strip actually drawn over the panel carries this group's name
        expect_equal(as.character(built$layout$layout$panel), nm)
        # and every box in it draws THIS group's median, not a neighbour's
        drawn <- built$data[[1]]
        expect_equal(
          as.numeric(drawn$middle[order(as.numeric(drawn$x))]),
          as.numeric(truth[grp, ]),
          tolerance = 1e-8
        )
      }
    }
  }
})

test_that("ggsummarystats(free.panels) survives a facet column named 'panel'", {
  # The panel label is written into the column named by label_col, which is
  # "panel". A facet variable of that name therefore has its key overwritten by
  # the label, and rebuilding the label from the overwritten key formatted it a
  # second time: "panel:panel:Alpha", or "Alpha, p, p" with two facet variables.
  # The keys are now read before the split, while they are still the keys.
  set.seed(7)
  d <- data.frame(
    panel = rep(c("Alpha", "Beta"), each = 8),
    grp = rep(c("a", "b"), 8),
    val = c(stats::rnorm(8, 10), stats::rnorm(8, 20)),
    stringsAsFactors = FALSE
  )
  p <- suppressWarnings(ggsummarystats(d, x = "grp", y = "val",
    facet.by = "panel", free.panels = TRUE, labeller = "label_both"))
  expect_equal(names(p), c("panel:Alpha", "panel:Beta"))
  expect_equal(
    as.character(
      suppressWarnings(ggplot2::ggplot_build(p[["panel:Alpha"]]$main.plot)
      )$layout$layout$panel
    ),
    "panel:Alpha"
  )

  # two facet variables, one of them named "panel", with label_value
  d2 <- data.frame(
    panel = rep(c("Alpha", "Beta"), each = 8),
    q = rep(c("p", "q"), each = 4, length.out = 16),
    grp = rep(c("a", "b"), 8),
    val = c(stats::rnorm(8, 10), stats::rnorm(8, 20)),
    stringsAsFactors = FALSE
  )
  p2 <- suppressWarnings(ggsummarystats(d2, x = "grp", y = "val",
    facet.by = c("panel", "q"), free.panels = TRUE, labeller = "label_value"))
  expect_equal(names(p2), c("Alpha, p", "Alpha, q", "Beta, p", "Beta, q"))

  # and each panel still draws its own cell's values: every box is the median of
  # a (panel, q, grp) sub-cell, so compare the whole set for that panel
  for (nm in names(p2)) {
    parts <- strsplit(nm, ", ", fixed = TRUE)[[1]]
    cell <- d2[d2$panel == parts[1] & d2$q == parts[2], ]
    expected <- sort(as.numeric(tapply(cell$val, cell$grp, stats::median)))
    built <- suppressWarnings(ggplot2::ggplot_build(p2[[nm]]$main.plot))
    expect_equal(as.character(built$layout$layout$panel), nm)
    expect_equal(sort(as.numeric(built$data[[1]]$middle)), expected,
      tolerance = 1e-8)
  }
})

test_that("a two-variable free.panels facet labels all four panels correctly", {
  # The help page's faceted example plus free.panels = TRUE: ALL FOUR panels were
  # transposed, the panel headed "supp:OJ, qc:fail" drawing VC/pass's boxes and
  # its n/median/iqr. (The shipped example itself does not set free.panels, so it
  # was never affected.) Expected medians recomputed from ToothGrowth in base R.
  df <- ToothGrowth
  df$dose <- as.factor(df$dose)
  set.seed(123)
  qc <- rep(c("pass", "fail"), 30)
  df$qc <- as.factor(sample(qc, 60))

  p <- suppressWarnings(ggsummarystats(df, x = "dose", y = "len",
    ggfunc = ggboxplot, add = "jitter", color = "dose", palette = "npg",
    facet.by = c("supp", "qc"), labeller = "label_both", free.panels = TRUE))
  expect_equal(length(p), 4L)

  for (nm in names(p)) {
    parts <- strsplit(nm, ", ", fixed = TRUE)[[1]]
    cell <- df[df$supp == sub("supp:", "", parts[1]) &
                 df$qc == sub("qc:", "", parts[2]), ]
    expected <- sort(as.numeric(
      tapply(cell$len, droplevels(cell$dose), stats::median)
    ))
    built <- suppressWarnings(ggplot2::ggplot_build(p[[nm]]$main.plot))
    expect_equal(as.character(built$layout$layout$panel), nm)
    expect_equal(sort(as.numeric(built$data[[1]]$middle)), expected,
      tolerance = 1e-8)
  }
})

test_that("ggsummarystats(free.panels) survives facet columns named like internals", {
  # facet.by = c("panel", "label") is wrong on the old code path regardless of
  # which rstatix is installed: rstatix's labeller has a local variable named
  # `label`, which a data column of that name shadows, so the "panel" variable
  # vanished from every title and the returned names came out DUPLICATED ("q",
  # "p", "q", "p") - p[["q"]] could only ever reach the first of the two. The
  # "panel" name is covered here because the label is written into a column of
  # that name, which the key read must not be confused by.
  set.seed(4)
  d <- data.frame(
    panel = rep(c("Z", "Y"), each = 12),
    label = rep(c("q", "p"), 12),
    grp = rep(c("a", "b"), 12),
    stringsAsFactors = FALSE
  )
  d$val <- ifelse(d$panel == "Z", 0, 30) + stats::rnorm(24)

  p <- suppressWarnings(ggsummarystats(d, x = "grp", y = "val",
    facet.by = c("panel", "label"), free.panels = TRUE, labeller = "label_both"))
  expect_equal(
    names(p),
    c("panel:Z, label:q", "panel:Z, label:p", "panel:Y, label:q", "panel:Y, label:p")
  )
  expect_false(any(duplicated(names(p))))

  pv <- suppressWarnings(ggsummarystats(d, x = "grp", y = "val",
    facet.by = c("panel", "label"), free.panels = TRUE, labeller = "label_value"))
  expect_equal(names(pv), c("Z, q", "Z, p", "Y, q", "Y, p"))

  # every panel draws its own cell, checked against base R
  for (nm in names(pv)) {
    parts <- strsplit(nm, ", ", fixed = TRUE)[[1]]
    cell <- d[d$panel == parts[1] & d$label == parts[2], ]
    expected <- sort(as.numeric(tapply(cell$val, cell$grp, stats::median)))
    built <- suppressWarnings(ggplot2::ggplot_build(pv[[nm]]$main.plot))
    expect_equal(as.character(built$layout$layout$panel), nm)
    expect_equal(sort(as.numeric(built$data[[1]]$middle)), expected,
      tolerance = 1e-8)
  }

  # a facet column named `label` keeps the prefix label_both asks for; rstatix's
  # own labeller has it shadowed by the data column and silently drops it
  d2 <- data.frame(
    label = rep(c("North", "East"), each = 12), grp = rep(c("a", "b"), 12),
    val = c(stats::rnorm(12, 100), stats::rnorm(12, 10)), stringsAsFactors = FALSE
  )
  p2 <- suppressWarnings(ggsummarystats(d2, x = "grp", y = "val",
    facet.by = "label", free.panels = TRUE, labeller = "label_both"))
  expect_equal(names(p2), c("label:North", "label:East"))

  # a duplicated or named facet.by must keep working (df_split_by normalises both)
  d3 <- data.frame(
    region = rep(c("North", "East"), each = 12), grp = rep(c("a", "b"), 12),
    val = c(stats::rnorm(12, 100), stats::rnorm(12, 10)), stringsAsFactors = FALSE
  )
  expect_equal(
    names(suppressWarnings(ggsummarystats(d3, x = "grp", y = "val",
      facet.by = c("region", "region"), free.panels = TRUE))),
    c("North", "East")
  )
  expect_equal(
    names(suppressWarnings(ggsummarystats(d3, x = "grp", y = "val",
      facet.by = c(a = "region"), free.panels = TRUE))),
    c("North", "East")
  )
})

test_that("ggsummarystats(free.panels) panel titles follow the data, not the alphabet", {
  # Deliberate choice, pinned here: panels are returned in the order the groups
  # appear in the data (which is the order they have always been drawn in), and
  # each title names the panel it sits on. Before, the titles came from a sorted
  # copy, so they read in sorted order while the panels did not - which is how a
  # title came to name a different panel from the one it was drawn over.
  set.seed(1)
  d <- data.frame(
    region = rep(c("North", "East", "South"), each = 12),
    grp = rep(c("a", "b"), 18),
    val = c(stats::rnorm(12, 100), stats::rnorm(12, 10), stats::rnorm(12, 50)),
    stringsAsFactors = FALSE
  )
  p <- suppressWarnings(ggsummarystats(d, x = "grp", y = "val",
    facet.by = "region", free.panels = TRUE))

  # data order, not sort order
  expect_equal(names(p), c("North", "East", "South"))
  expect_false(identical(names(p), sort(names(p))))

  # and the panel each title sits on draws that group's rows
  for (nm in names(p)) {
    cell <- d[d$region == nm, ]
    expected <- sort(as.numeric(tapply(cell$val, cell$grp, stats::median)))
    built <- suppressWarnings(ggplot2::ggplot_build(p[[nm]]$main.plot))
    expect_equal(as.character(built$layout$layout$panel), nm)
    expect_equal(sort(as.numeric(built$data[[1]]$middle)), expected,
      tolerance = 1e-8)
  }
})

test_that("ggsummarystats(free.panels) survives facet columns named like paste()'s formals", {
  # The label used to be assembled with do.call(paste, values). Map() names its
  # result after the facet variables, so those names reached paste() as arguments:
  # a column called `sep` or `recycle0` errored, and `collapse` silently produced a
  # blank title for every panel. The join is a fold now, which cannot pass a name.
  set.seed(1)
  for (nm in c("sep", "collapse", "recycle0")) {
    d <- data.frame(
      grp = rep(c("a", "b"), 12),
      val = c(stats::rnorm(12, 10), stats::rnorm(12, 20)),
      stringsAsFactors = FALSE
    )
    d[[nm]] <- rep(c("North", "East"), each = 12)
    p <- suppressWarnings(ggsummarystats(d, x = "grp", y = "val", facet.by = nm,
      free.panels = TRUE, labeller = "label_both"))
    expect_equal(names(p), paste0(nm, c(":North", ":East")))
    expect_false(any(names(p) == ""))
  }

  # a facet.by that resolves to no grouping column still yields one labelled panel
  d2 <- ToothGrowth
  d2$dose <- factor(d2$dose)
  for (fb in list(character(0), "")) {
    p2 <- suppressWarnings(ggsummarystats(d2, x = "dose", y = "len",
      facet.by = fb, free.panels = TRUE))
    expect_equal(length(p2), 1L)
    expect_equal(names(p2), "")
  }
})

test_that("ggsummarystats(free.panels) titles a missing group 'NA' and draws its rows", {
  # A missing group is titled with the string "NA" - that is how paste() renders
  # it and how such a panel has always been labelled. Joining the facet variables
  # without paste()-ing the first one would leave the label a real NA, which
  # factor() drops rather than levels, losing the panel's title entirely.
  set.seed(3)
  d <- data.frame(
    region = rep(c("North", "East", "South"), each = 8),
    grp = rep(c("a", "b"), 12), stringsAsFactors = FALSE
  )
  d$val <- rep(c(100, 200, 300), each = 8) + stats::rnorm(24)
  d$region[c(1, 9)] <- NA # NA group is NOT last, so the old path transposed it

  p <- suppressWarnings(ggsummarystats(d, x = "grp", y = "val",
    facet.by = "region", free.panels = TRUE))
  expect_true("NA" %in% names(p))
  expect_false(any(is.na(names(p))))

  truth <- tapply(d$val, addNA(d$region), stats::median)
  for (nm in names(p)) {
    rows <- if (nm == "NA") is.na(d$region) else !is.na(d$region) & d$region == nm
    cell <- d[rows, ]
    expected <- sort(as.numeric(tapply(cell$val, cell$grp, stats::median)))
    built <- suppressWarnings(ggplot2::ggplot_build(p[[nm]]$main.plot))
    expect_equal(as.character(built$layout$layout$panel), nm)
    expect_equal(sort(as.numeric(built$data[[1]]$middle)), expected,
      tolerance = 1e-8)
  }
})
