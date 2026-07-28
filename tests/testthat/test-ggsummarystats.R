
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

test_that("the faceted ?ggsummarystats example labels all four panels correctly", {
  # The two-variable example on the help page had ALL FOUR panels transposed:
  # the panel headed "supp:OJ, qc:fail" drew VC/pass's boxes and its n/median/iqr.
  # Expected medians below are recomputed from ToothGrowth with base R.
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
