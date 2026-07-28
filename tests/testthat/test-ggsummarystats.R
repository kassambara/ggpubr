
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

# Each summary-table column must sit under the box it describes. Returns the
# number of drawn columns whose numbers are not that box's own, recomputed here
# in base R rather than through ggpubr or rstatix.
.miscounted_columns <- function(p, d, x, y) {
  bm <- suppressWarnings(suppressMessages(ggplot2::ggplot_build(p$main.plot)))
  bt <- suppressWarnings(suppressMessages(ggplot2::ggplot_build(p$summary.plot)))
  cats <- as.character(bm$layout$panel_scales_x[[1]]$get_limits())
  lab <- bt$data[[1]]
  xs <- as.numeric(lab$x)
  wrong <- 0
  for (i in seq_along(cats)) {
    drawn <- gsub("\n", "|", lab$label[abs(xs - i) < 1e-9])
    v <- d[[y]][if (is.na(cats[i])) is.na(d[[x]]) else
      (!is.na(d[[x]]) & as.character(d[[x]]) == cats[i])]
    v <- v[!is.na(v)]
    if (!length(v)) next
    expected <- paste(length(v), round(stats::median(v)), round(stats::IQR(v)), sep = "|")
    if (length(drawn) != 1 || !identical(drawn, expected)) wrong <- wrong + 1
  }
  wrong
}

test_that("ggsummarystats() table columns sit under the boxes they describe", {
  # The table is built from its own frame and trained its own x scale: dplyr
  # returns group keys sorted while the builders factor x in the order the groups
  # appear, so with a non-alphabetical character x every column sat under the
  # wrong box. Measured before the fix: the column under "Pre" (median 11.5)
  # printed 22, which is "Mid"'s.
  d <- data.frame(
    time = rep(c("Pre", "Post", "Mid"), each = 4),
    val = c(10:13, 30:33, 20:23), stringsAsFactors = FALSE
  )
  for (f in list(ggboxplot, ggviolin, ggdotplot, ggstripchart, ggbarplot,
                 ggline, ggerrorplot)) {
    p <- suppressWarnings(suppressMessages(
      ggsummarystats(d, x = "time", y = "val", ggfunc = f)
    ))
    expect_equal(.miscounted_columns(p, d, "time", "val"), 0)
    # the table now trains the same categories, in the same order, as the plot
    expect_equal(
      as.character(suppressWarnings(suppressMessages(
        ggplot2::ggplot_build(p$summary.plot)))$layout$panel_scales_x[[1]]$get_limits()),
      c("Pre", "Post", "Mid")
    )
  }
})

test_that("ggsummarystats() follows the plot for numeric, integer and Date x", {
  # These reach the gate because five of the seven builders factor such a column.
  # Moving the table's data onto the plot's categories is what keeps it on the
  # axis it is drawn against; pinning only the axis, and leaving the data
  # numeric, would shift every column one slot.
  set.seed(1)
  frames <- list(
    numeric = data.frame(k = rep(c(0.5, 1, 2), each = 6), v = rep(c(10, 20, 30), each = 6) + stats::rnorm(18)),
    integer = data.frame(k = rep(c(0L, 3L, 24L), each = 6), v = rep(c(10, 20, 30), each = 6) + stats::rnorm(18)),
    date = data.frame(k = rep(as.Date("2020-01-01") + c(0, 10, 40), each = 6),
                      v = rep(c(10, 20, 30), each = 6) + stats::rnorm(18))
  )
  for (nm in names(frames)) {
    d <- frames[[nm]]
    p <- suppressWarnings(suppressMessages(ggsummarystats(d, x = "k", y = "v")))
    expect_equal(.miscounted_columns(p, d, "k", "v"), 0)
  }
})

test_that("ggsummarystats() leaves a continuous or free-scaled plot alone", {
  # The table cannot express a continuous axis, and one set of categories cannot
  # describe free per-panel scales, so the gate must not fire for either.
  skip_if_not_installed("ggplot2")
  disc <- function(p) {
    suppressWarnings(suppressMessages(
      ggplot2::ggplot_build(p)))$layout$panel_scales_x[[1]]$is_discrete()
  }
  p1 <- suppressWarnings(suppressMessages(ggsummarystats(
    ToothGrowth, x = "dose", y = "len", ggfunc = ggline, numeric.x.axis = TRUE
  )))
  expect_false(disc(p1$main.plot))
  expect_false(disc(p1$summary.plot)) # untouched: still its own continuous scale

  # free_x with DISJOINT categories per panel: one set of categories cannot
  # describe both, so the gate must stay off. If it fired, panel 1's categories
  # would be applied to the whole table and panel 2's groups would be filtered
  # away - 2 rows instead of 4.
  set.seed(2)
  d2 <- data.frame(
    g = c(rep(c("a", "b"), each = 6), rep(c("c", "d"), each = 6)),
    p = rep(c("P1", "P2"), each = 12),
    v = c(stats::rnorm(12, 10), stats::rnorm(12, 50)), stringsAsFactors = FALSE
  )
  p2 <- suppressWarnings(suppressMessages(ggsummarystats(
    d2, x = "g", y = "v", facet.by = "p", scales = "free_x"
  )))
  b2 <- suppressWarnings(suppressMessages(ggplot2::ggplot_build(p2$summary.plot)))
  expect_equal(nrow(b2$data[[1]]), 4L)
  expect_s3_class(ggplot2::ggplotGrob(b2), "gtable")
})

test_that("ggsummarystats() summarises exactly the groups the plot draws", {
  # The case that made an earlier attempt print a fabricated statistic: a removed
  # group plus genuine NAs. Released draws FOUR columns for THREE categories, so
  # the removed dose 1's median lands under the box labelled 2.
  skip_if_not_installed("emmeans")
  d <- ToothGrowth
  d$dose <- as.character(d$dose)
  d$dose[c(3, 17)] <- NA
  p <- suppressWarnings(suppressMessages(ggsummarystats(
    d, x = "dose", y = "len", comparisons = list(c("0.5", "2")), remove = "1"
  )))
  bt <- suppressWarnings(suppressMessages(ggplot2::ggplot_build(p$summary.plot)))
  expect_equal(nrow(bt$data[[1]]), 3L) # not 4
  expect_equal(.miscounted_columns(p, d, "dose", "len"), 0)

  # a group the plot draws but cannot summarise keeps its slot, and prints nothing
  d2 <- data.frame(g = rep(c("A", "B", "C"), each = 4),
                   v = c(1:4, rep(NA_real_, 4), 9:12), stringsAsFactors = FALSE)
  p2 <- suppressWarnings(suppressMessages(ggsummarystats(d2, x = "g", y = "v")))
  bt2 <- suppressWarnings(suppressMessages(ggplot2::ggplot_build(p2$summary.plot)))
  expect_equal(
    as.character(bt2$layout$panel_scales_x[[1]]$get_limits()), c("A", "B", "C")
  )
  expect_equal(.miscounted_columns(p2, d2, "g", "v"), 0)
})

test_that("ggsummarystats() adds no construction-time message and no scale of its own", {
  # The probe build must stay silent (ggdotplot otherwise reports its bin width at
  # call time, where released reports nothing), and no scale is added on the
  # common path - a user's own scale_x_discrete() would replace it and silently
  # restore the misalignment, and $summary.plot is documented as editable.
  for (f in list(ggboxplot, ggviolin, ggdotplot, ggstripchart, ggbarplot,
                 ggline, ggerrorplot)) {
    expect_message(
      suppressWarnings(invisible(
        ggsummarystats(ToothGrowth, x = "dose", y = "len", ggfunc = f)
      )),
      NA
    )
  }
  d <- data.frame(time = rep(c("Pre", "Post", "Mid"), each = 4),
                  val = c(10:13, 30:33, 20:23), stringsAsFactors = FALSE)
  p <- suppressWarnings(suppressMessages(ggsummarystats(d, x = "time", y = "val")))
  styled <- style_summarystats(p, table = ggplot2::scale_x_discrete(labels = toupper))
  b <- suppressWarnings(suppressMessages(ggplot2::ggplot_build(styled$summary.plot)))
  # the user's own scale must not be able to undo the alignment
  expect_equal(as.character(b$layout$panel_scales_x[[1]]$get_limits()),
               c("Pre", "Post", "Mid"))
})

test_that("ggsummarystats() keeps an ordered x ordered, so the table's colours match", {
  # The class of the x column picks ggplot2's default discrete scale: an ordered
  # factor gets scale_*_ordinal (viridis), an unordered one scale_*_hue. Coercing
  # x without carrying `ordered` through recoloured the table's numbers away from
  # the boxes they sit under, so the composite's own colour key disagreed with
  # itself - on a call that was already correct.
  ord <- data.frame(
    g = factor(rep(c("lo", "mid", "hi"), each = 5),
               levels = c("lo", "mid", "hi"), ordered = TRUE),
    v = c(1:5, 11:15, 21:25)
  )
  p <- suppressWarnings(suppressMessages(
    ggsummarystats(ord, x = "g", y = "v", color = "g")
  ))
  expect_true(is.ordered(p$summary.plot$data$g))
  bm <- suppressWarnings(suppressMessages(ggplot2::ggplot_build(p$main.plot)))
  bt <- suppressWarnings(suppressMessages(ggplot2::ggplot_build(p$summary.plot)))
  bx <- as.numeric(bm$data[[1]]$x)
  tx <- as.numeric(bt$data[[1]]$x)
  for (i in 1:3) {
    expect_equal(
      unique(bt$data[[1]]$colour[abs(tx - i) < 1e-9]),
      unique(bm$data[[1]]$colour[abs(bx - i) < 1e-9])
    )
  }

  # and with order= the labels follow the re-ordered plot, colours still matching
  p2 <- suppressWarnings(suppressMessages(ggsummarystats(
    ord, x = "g", y = "v", color = "g", order = c("hi", "lo", "mid")
  )))
  expect_equal(.miscounted_columns(p2, ord, "g", "v"), 0)
})

test_that("ggsummarystats() keeps an NA category in its slot (exclude = NULL)", {
  # get_limits() reports NA as a real category. factor() drops NA from its levels
  # by default, so without exclude = NULL every column from the NA group onward
  # rotates onto the wrong box - the same defect this fix exists to remove.
  # Measured with the guard removed: NA's 52 lands on b's box, a's 12 on NA's.
  k <- factor(c(rep(NA, 4), rep("a", 4), rep("b", 4)),
              levels = c(NA, "a", "b"), exclude = NULL)
  d <- data.frame(v = c(50:53, 10:13, 30:33))
  d$k <- k
  p <- suppressWarnings(suppressMessages(ggsummarystats(d, x = "k", y = "v")))
  bt <- suppressWarnings(suppressMessages(ggplot2::ggplot_build(p$summary.plot)))
  lab <- bt$data[[1]]
  ord <- order(as.numeric(lab$x))
  expect_equal(as.numeric(lab$x)[ord], c(1, 2, 3))
  # slot 1 is the NA group (median 51.5 -> 52), then a (11.5 -> 12), then b (32)
  expect_equal(gsub("\n", "|", lab$label[ord]), c("4|52|2", "4|12|2", "4|32|2"))
  expect_equal(.miscounted_columns(p, d, "k", "v"), 0)
})

test_that("ggsummarystats() falls back to released behaviour when it cannot follow the plot", {
  # Two guards, both load-bearing, neither previously covered.
  #
  # 1. If no summarised row matches the plot's categories the two key spaces are
  #    not what we think they are. Keeping the released table is better than a
  #    blank one: without the any(keep) guard this draws ZERO columns.
  d <- data.frame(
    time = rep(c("Pre", "Post", "Mid"), each = 4),
    val = c(10:13, 30:33, 20:23), stringsAsFactors = FALSE
  )
  p <- suppressWarnings(suppressMessages(
    ggsummarystats(d, x = "time", y = "val", order = c("zz1", "zz2"))
  ))
  bt <- suppressWarnings(suppressMessages(ggplot2::ggplot_build(p$summary.plot)))
  expect_equal(nrow(bt$data[[1]]), 3L)

  # 2. If the main plot cannot be built, ggsummarystats() must still return its
  #    object and let the error surface at print, as it always has - not raise it
  #    at the call. Without the tryCatch the probe build throws here.
  bad <- function(data, x, y, ...) {
    ggboxplot(data, x, y, ...) + ggplot2::scale_x_continuous()
  }
  d2 <- data.frame(g = rep(c("a", "b"), each = 4), v = c(1:4, 9:12),
                   stringsAsFactors = FALSE)
  expect_s3_class(
    suppressWarnings(suppressMessages(
      ggsummarystats(d2, x = "g", y = "v", ggfunc = bad)
    )),
    "ggsummarystats"
  )
})

test_that("ggsummarystats(free.panels) refuses an invalid labeller and says why", {
  # `labeller` used to be validated only as a side effect of looking the labelling
  # function up, so an invalid value failed with whatever that lookup raised -
  # "no applicable method for 'mutate'", or "EXPR must be a length 1 vector".
  # The same values are still refused; the message now names the argument.
  d <- ToothGrowth
  d$dose <- factor(d$dose)
  for (bad in list("foo", NA, NULL, c("label_value", "label_both"), sum)) {
    expect_error(
      suppressWarnings(suppressMessages(ggsummarystats(
        d, x = "dose", y = "len", facet.by = "supp",
        free.panels = TRUE, labeller = bad
      ))),
      "labeller"
    )
  }
  # and the two documented values still work
  for (good in c("label_value", "label_both")) {
    expect_s3_class(
      suppressWarnings(suppressMessages(ggsummarystats(
        d, x = "dose", y = "len", facet.by = "supp",
        free.panels = TRUE, labeller = good
      ))),
      "ggsummarystats_list"
    )
  }
})
