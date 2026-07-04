context("test-ggline-jitter-dodge")

# #436: in a grouped ggline() with add = "jitter", passing position_dodge() used
# to dodge only the line / error bars, leaving the jitter points centered (and
# thus misaligned from their group). The jitter now dodges with the same width,
# so points sit under their group's line. Default (position = "identity") is
# unchanged (jitter stays centered).

df <- ToothGrowth
df$dose <- as.factor(df$dose)

test_that("position_dodge dodges the jitter to align with the line (#436)", {
  w <- 0.4
  p <- ggline(df, x = "dose", y = "len", color = "supp",
              add = c("mean_se", "jitter"), position = position_dodge(w))
  b <- ggplot2::ggplot_build(p)
  # jitter layer (60 rows) mean x per (dose, group); error-bar layer x per (dose, group)
  jit <- eb <- NULL
  for (d in b$data) {
    if (nrow(d) == 60 && "group" %in% names(d)) jit <- d
    if ("ymin" %in% names(d) && nrow(d) <= 8) eb <- d
  }
  expect_false(is.null(jit)); expect_false(is.null(eb))
  jit.center <- as.numeric(aggregate(x ~ interaction(round(x), group), data = jit, FUN = mean)$x)
  eb.center <- as.numeric(sort(eb$x))
  # each error bar has a jitter cluster centered on it (within jitter noise)
  # -> the sorted per-cluster jitter means match the sorted error-bar centers
  expect_equal(sort(jit.center), eb.center, tolerance = 0.05)
  # and the two groups are actually separated (dodged), not centered on integers
  seps <- tapply(jit$x, round(jit$x), function(v) diff(range(tapply(v, v > median(v), mean))))
  expect_true(mean(seps) > 0.1)
})

test_that("default (position = identity) leaves the jitter centered, not dodged", {
  p_def <- ggline(df, x = "dose", y = "len", color = "supp", add = c("mean_se", "jitter"))
  d <- ggplot2::ggplot_build(p_def)$data[[1]]
  # not dodged -> the two groups' x-ranges OVERLAP at each dose (share a center)
  overlap_ok <- TRUE
  for (xc in unique(round(d$x))) {
    sub <- d[round(d$x) == xc, ]
    r1 <- range(sub$x[sub$group == min(sub$group)])
    r2 <- range(sub$x[sub$group == max(sub$group)])
    if (!(r1[1] <= r2[2] && r2[1] <= r1[2])) overlap_ok <- FALSE
  }
  expect_true(overlap_ok)
})

test_that("default jitter is byte-identical to the pre-fix path (seed = 123)", {
  # the fix only fires for a PositionDodge; default position = 'identity' must be
  # untouched. Compare the built jitter x to a manual position_jitter(seed=123).
  p_def <- ggline(df, x = "dose", y = "len", color = "supp", add = "jitter")
  got <- sort(ggplot2::ggplot_build(p_def)$data[[1]]$x)
  ref <- ggplot2::ggplot_build(
    ggplot2::ggplot(df, ggplot2::aes(dose, len)) +
      ggplot2::geom_jitter(position = ggplot2::position_jitter(0.2, seed = 123))
  )$data[[1]]$x
  expect_equal(got, sort(ref), tolerance = 1e-9)
})

test_that("non-grouped ggline jitter is unchanged", {
  expect_error(
    ggplot2::ggplot_build(ggline(df, x = "dose", y = "len", add = "jitter")), NA
  )
})

test_that("position_dodge() with no width does not error (jitter stays centered)", {
  expect_error(
    suppressWarnings(ggplot2::ggplot_gtable(ggplot2::ggplot_build(
      ggline(df, x = "dose", y = "len", color = "supp", add = "jitter",
             position = position_dodge())
    ))),
    NA
  )
})

test_that("position_dodge2 leaves the jitter centered (unchanged, avoids overshoot)", {
  # dodge2 packs zero-width summary geoms differently; the fix is scoped to plain
  # position_dodge, so dodge2 keeps the centered (position_jitter) behavior.
  p_d2 <- ggline(df, x = "dose", y = "len", color = "supp", add = "jitter",
                 position = ggplot2::position_dodge2(0.5))
  p_j <- ggline(df, x = "dose", y = "len", color = "supp", add = "jitter")
  expect_equal(
    sort(ggplot2::ggplot_build(p_d2)$data[[1]]$x),
    sort(ggplot2::ggplot_build(p_j)$data[[1]]$x)
  )
})

test_that("non-line geoms with jitter are unaffected", {
  # ggboxplot uses the else-branch (already jitterdodge) -- must still build
  expect_error(
    ggplot2::ggplot_build(ggboxplot(df, x = "dose", y = "len", color = "supp", add = "jitter")),
    NA
  )
})
