
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
  truth <- tapply(d$val, d$region, mean) # independent reference

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
        # and the boxes sit at this group's level, not another's
        expect_equal(mean(built$data[[1]]$middle), unname(truth[[grp]]),
          tolerance = 1)
      }
    }
  }
})
