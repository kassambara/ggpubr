context("test-ggcompare-two-way")

# jobsatisfaction (datarium) is the canonical two-way lesson dataset. Skip if it
# is not installed rather than fail the suite.
get_js <- function() {
  testthat::skip_if_not_installed("datarium")
  testthat::skip_if_not_installed("emmeans")
  e <- new.env()
  utils::data("jobsatisfaction", package = "datarium", envir = e)
  e$jobsatisfaction
}

# Number of brackets drawn (GeomBracket draws 3 rows -- two tips + label -- per
# bracket).
n_brackets <- function(p) {
  i <- which(vapply(p$layers, function(l) inherits(l$geom, "GeomBracket"), logical(1)))
  if (length(i) == 0L) return(0L)
  nrow(ggplot2::layer_data(p, i[1])) / 3
}
subtitle_text <- function(p) {
  s <- ggplot2::ggplot_build(p)$plot$labels$subtitle
  paste(deparse(s), collapse = "")
}

# ---- two-way mode composes the interaction figure ---------------------------
test_that("ggcompare() enters two-way mode and reproduces the lesson numbers", {
  js <- get_js()
  p <- ggcompare(js, x = "gender", y = "score", color = "education_level",
    omnibus = "anova")

  # 3 education comparisons within each of the 2 genders = 6 brackets.
  expect_equal(n_brackets(p), 6)

  # The bracket p.adj must equal the pooled-model emmeans + Bonferroni values.
  truth <- js %>%
    dplyr::group_by(gender) %>%
    rstatix::emmeans_test(score ~ education_level, p.adjust.method = "bonferroni")
  i <- which(vapply(p$layers, function(l) inherits(l$geom, "GeomBracket"), logical(1)))
  brk <- ggplot2::layer_data(p, i[1])
  # one label row per bracket carries the significance stars
  expect_setequal(sort(unique(brk$label)), sort(unique(truth$p.adj.signif)))

  # Subtitle names the interaction with the exact ANOVA numbers.
  st <- subtitle_text(p)
  expect_match(st, "Interaction \\(gender")
  expect_match(st, "education_level")
  expect_match(st, "7.34")   # F(2,52) = 7.34
  expect_match(st, "0.002")  # p = 0.002
  expect_match(st, "0.22")   # ges = 0.22

  # Caption describes the pairwise test.
  cap <- ggplot2::ggplot_build(p)$plot$labels$caption
  expect_match(paste(deparse(cap), collapse = ""), "Emmeans test")
  expect_match(paste(deparse(cap), collapse = ""), "Bonferroni")

  expect_silent(ggplot2::ggplotGrob(ggplot2::ggplot_build(p)))
})

test_that("two-way defaults (emmeans / bonferroni / hide.ns) are used, and overridable", {
  js <- get_js()
  # Default method is emmeans; overriding to t_test changes the p.adj values.
  p_emm <- ggcompare(js, x = "gender", y = "score", color = "education_level")
  p_t <- suppressWarnings(
    ggcompare(js, x = "gender", y = "score", color = "education_level",
      method = "t_test")
  )
  brk <- function(p) {
    i <- which(vapply(p$layers, function(l) inherits(l$geom, "GeomBracket"), logical(1)))
    ggplot2::layer_data(p, i[1])$label
  }
  # Both fully significant here, but the underlying tests differ -> exercise both.
  expect_equal(n_brackets(p_emm), 6)
  expect_equal(n_brackets(p_t), 6)

  # hide.ns defaults to TRUE in two-way: the legend.var direction hides the two
  # ns gender contrasts, leaving one significant bracket; hide.ns=FALSE shows 3.
  p_hidden <- ggcompare(js, x = "gender", y = "score", color = "education_level",
    pwc.group.by = "legend.var")
  p_shown <- ggcompare(js, x = "gender", y = "score", color = "education_level",
    pwc.group.by = "legend.var", hide.ns = FALSE)
  expect_equal(n_brackets(p_hidden), 1)
  expect_equal(n_brackets(p_shown), 3)
})

test_that("brackets stay on the panel in both comparison directions", {
  js <- get_js()
  # Brackets must sit within the plotted x range (2 dodged gender clusters,
  # roughly [0.5, 2.5]); a bracket positioned on the wrong (legend) grid would
  # land near x = 3, off the panel.
  brk_x <- function(p) {
    i <- which(vapply(p$layers, function(l) inherits(l$geom, "GeomBracket"), logical(1)))
    d <- ggplot2::layer_data(p, i[1])
    range(c(d$xmin, d$xmax), na.rm = TRUE)
  }
  p_x <- ggcompare(js, x = "gender", y = "score", color = "education_level")
  p_leg <- ggcompare(js, x = "gender", y = "score", color = "education_level",
    pwc.group.by = "legend.var", hide.ns = FALSE)
  expect_true(brk_x(p_x)[1] >= 0.5 && brk_x(p_x)[2] <= 2.5)
  expect_true(brk_x(p_leg)[1] >= 0.5 && brk_x(p_leg)[2] <= 2.5)
  # The legend.var university bracket must span the two gender clusters
  # (male university ~1.267 to female university ~2.267), not sit past them.
  expect_gt(brk_x(p_leg)[2], 2)
})

test_that("faceted two-way draws per-panel brackets and interaction labels", {
  js <- get_js()
  js$region <- factor(rep(c("north", "south"), length.out = nrow(js)))
  p <- ggcompare(js, x = "gender", y = "score", color = "education_level",
    facet.by = "region")
  expect_silent(ggplot2::ggplotGrob(ggplot2::ggplot_build(p)))
  # Brackets are computed per panel: 3 education comparisons x 2 genders x 2
  # regions = 12 (before hide.ns).
  expect_equal(n_brackets(ggcompare(js, x = "gender", y = "score",
    color = "education_level", facet.by = "region", hide.ns = FALSE)), 12)
  # One per-panel interaction label per region.
  ti <- which(vapply(p$layers, function(l) inherits(l$geom, "GeomText"), logical(1)))
  labs <- ggplot2::layer_data(p, ti[1])
  expect_equal(nrow(labs), 2)
  expect_true(all(grepl("^Interaction: F", labs$label)))
  # omnibus = "none" drops the interaction labels.
  p0 <- ggcompare(js, x = "gender", y = "score", color = "education_level",
    facet.by = "region", omnibus = "none")
  expect_false(any(vapply(p0$layers, function(l) inherits(l$geom, "GeomText"), logical(1))))
})

test_that("ref.group draws only comparisons to the reference level", {
  js <- get_js()
  p <- ggcompare(js, x = "gender", y = "score", color = "education_level",
    ref.group = "school", hide.ns = FALSE)
  # 2 non-reference levels x 2 genders = 4 brackets.
  expect_equal(n_brackets(p), 4)
})

test_that("other bases and fill grouping work in two-way mode", {
  js <- get_js()
  for (b in c("violin", "stripchart")) {
    p <- ggcompare(js, x = "gender", y = "score", color = "education_level",
      base = b, hide.ns = FALSE)
    expect_equal(n_brackets(p), 6, info = b)
    expect_silent(ggplot2::ggplotGrob(ggplot2::ggplot_build(p)))
  }
  p_fill <- ggcompare(js, x = "gender", y = "score", fill = "education_level",
    hide.ns = FALSE)
  expect_equal(n_brackets(p_fill), 6)
})

# ---- guards -----------------------------------------------------------------
test_that("two-way mode refuses unsupported inputs with clear messages", {
  js <- get_js()
  expect_error(
    ggcompare(js, x = "gender", y = "score", color = "education_level",
      comparisons = list(c("school", "college"))),
    "not supported in two-way"
  )
  expect_warning(
    ggcompare(js, x = "gender", y = "score", color = "education_level",
      effsize = TRUE),
    "not supported in two-way"
  )
  myfun <- function(data, formula, ...) rstatix::t_test(data, formula, ...)
  expect_error(
    ggcompare(js, x = "gender", y = "score", color = "education_level",
      method = myfun),
    "function `method` is not supported"
  )
})

# ---- the one-way path is untouched ------------------------------------------
test_that("color == x does NOT trigger two-way mode", {
  df <- ToothGrowth
  df$dose <- as.factor(df$dose)
  p <- ggcompare(df, x = "dose", y = "len", color = "dose")
  # A one-way ANOVA subtitle has no interaction term.
  expect_false(grepl("Interaction", subtitle_text(p)))
})

# ---- add_test_label() two-way effect naming ---------------------------------
test_that("add_test_label(group.by=) names the interaction and selects effects", {
  js <- get_js()
  p <- ggboxplot(js, x = "gender", y = "score", color = "education_level")

  # Default: the interaction, described "Interaction (gender x education_level)".
  lab_int <- add_test_label(p, group.by = "education_level", type = "text")$labels$subtitle
  expect_match(lab_int, "Interaction \\(gender")
  expect_match(lab_int, "7.34")

  # A specific main effect by term name.
  lab_main <- add_test_label(p, group.by = "education_level", effect = "gender",
    type = "text")$labels$subtitle
  expect_false(grepl("Interaction", lab_main))

  # effect = "all" -> multi-line (3 effect lines) in text mode.
  lab_all <- add_test_label(p, group.by = "education_level", effect = "all",
    type = "text")$labels$subtitle
  expect_equal(length(strsplit(lab_all, "\n")[[1]]), 3)

  # A non-existent term errors clearly.
  expect_error(
    add_test_label(p, group.by = "education_level", effect = "nope"),
    "does not match an ANOVA term"
  )

  # kruskal + group.by warns and falls back to one-way (no interaction).
  expect_warning(
    add_test_label(p, method = "kruskal", group.by = "education_level"),
    "requires method = 'anova'"
  )
})

# ---- simple.effects: per-group simple main effects on the plot --------------
# Number of GeomText layers (the simple-effect F labels), excluding the mean and
# bracket layers.
n_text_layers <- function(p) {
  sum(vapply(p$layers, function(l) inherits(l$geom, "GeomText"), logical(1)))
}
text_labels <- function(p) {
  i <- which(vapply(p$layers, function(l) inherits(l$geom, "GeomText"), logical(1)))
  if (length(i) == 0L) return(character(0))
  as.character(p$layers[[i[1]]]$data$.label)
}

test_that("simple.effects = TRUE draws pooled-error simple main effects (132 / 62.8)", {
  js <- get_js()
  p <- ggcompare(js, x = "gender", y = "score", color = "education_level",
    simple.effects = TRUE)

  # Exactly one GeomText layer carrying one label per x group (2 genders).
  expect_equal(n_text_layers(p), 1)
  labs <- text_labels(p)
  expect_equal(length(labs), 2)

  # The labels reproduce the pooled-error simple main effects from the lesson:
  # F(2,52) for BOTH genders (the full-model residual df, not a per-subset df).
  expect_true(all(grepl("F\\(2,52\\)", labs)))
  expect_true(any(grepl("132", labs)))    # male
  expect_true(any(grepl("62.8", labs)))   # female

  # Cross-check against the direct pooled-error computation.
  model <- stats::lm(score ~ gender * education_level, data = js)
  truth <- js %>%
    dplyr::group_by(gender) %>%
    rstatix::anova_test(score ~ education_level, error = model) %>%
    rstatix::get_anova_table()
  expect_equal(sort(truth$DFd), c(52, 52))
  expect_equal(round(truth$F[truth$gender == "male"]), 132)
  expect_equal(round(truth$F[truth$gender == "female"], 1), 62.8)
})

test_that("simple.effects default (FALSE) adds no text layer (no regression)", {
  js <- get_js()
  p <- ggcompare(js, x = "gender", y = "score", color = "education_level")
  expect_equal(n_text_layers(p), 0)
})

test_that("simple.effects warns and is skipped outside its supported config", {
  js <- get_js()

  # One-way mode: no second factor -> warn, no text.
  df <- ToothGrowth
  df$dose <- as.factor(df$dose)
  expect_warning(
    p1 <- ggcompare(df, x = "dose", y = "len", simple.effects = TRUE),
    "only in two-way"
  )
  expect_equal(n_text_layers(p1), 0)

  # legend.var direction -> warn, no text.
  expect_warning(
    p2 <- ggcompare(js, x = "gender", y = "score", color = "education_level",
      pwc.group.by = "legend.var", simple.effects = TRUE),
    "pwc.group.by"
  )
  expect_equal(n_text_layers(p2), 0)

  # With facet.by -> warn.
  js$grp <- rep(c("A", "B"), length.out = nrow(js))
  expect_warning(
    ggcompare(js, x = "gender", y = "score", color = "education_level",
      facet.by = "grp", simple.effects = TRUE),
    "not supported together with `facet.by`"
  )
})

test_that("simple.effects is robust to an x column named like add_xy_position's synthetic columns", {
  js <- get_js()
  # Name the x-axis factor literally "x" (collides with add_xy_position's `x`).
  names(js)[names(js) == "gender"] <- "x"
  p <- ggcompare(js, x = "x", y = "score", color = "education_level",
    simple.effects = TRUE)
  labs <- text_labels(p)
  # The labels must still be drawn (not silently dropped) with the right numbers.
  expect_equal(length(labs), 2)
  expect_true(any(grepl("132", labs)))
  expect_true(any(grepl("62.8", labs)))
})

# ---- two-way interaction subtitle wraps to two lines ------------------------
test_that("the default two-way interaction subtitle wraps onto two lines", {
  js <- get_js()
  p <- ggboxplot(js, x = "gender", y = "score", color = "education_level")

  # Expression mode: the wrapped label is an atop() call (line 1 = the effect
  # name, line 2 = the statistics) so it does not overflow at narrow widths.
  expr <- add_test_label(p, group.by = "education_level")$labels$subtitle
  expect_true(is.call(expr) && identical(expr[[1]], as.name("atop")))

  # Text mode: two newline-separated lines, description above the statistics,
  # with every value retained (F, p, ges, n).
  txt <- add_test_label(p, group.by = "education_level", type = "text")$labels$subtitle
  lines <- strsplit(txt, "\n")[[1]]
  expect_equal(length(lines), 2)
  expect_match(lines[1], "^Interaction \\(gender")
  expect_match(lines[2], "F\\(2,52\\)")
  expect_match(lines[2], "n = 58")

  # A main-effect label is short, so it stays on one line (no wrap).
  main <- add_test_label(p, group.by = "education_level", effect = "gender",
    type = "text")$labels$subtitle
  expect_false(grepl("\n", main))

  # A caller-supplied description keeps the single-line form (escape hatch).
  usr <- add_test_label(p, group.by = "education_level", description = "Int",
    type = "text")$labels$subtitle
  expect_false(grepl("\n", usr))
})
