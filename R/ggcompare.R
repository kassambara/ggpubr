#' @include utilities.R geom_pwc.R add_test_label.R
NULL
#' Publication-Ready Group Comparison Plot
#' @description Draws, in a single call, a publication-ready comparison figure:
#'   a box plot (or violin/stripchart) with jittered points and group means,
#'   pairwise comparison brackets with adjusted p-values, and an omnibus test
#'   label. It composes existing ggpubr layers - the base builder,
#'   \code{\link{geom_pwc}()} and \code{\link{add_test_label}()} - into one
#'   customizable \code{ggplot}. \dQuote{Clean by default, complete on request}:
#'   turn individual pieces on or off through the arguments.
#' @details \code{ggcompare()} follows the \code{ggfunc} contract of
#'   \code{\link{ggsummarystats}()}, so it can be used as
#'   \code{ggsummarystats(data, x, y, ggfunc = ggcompare)} to place the summary
#'   statistics table under the comparison figure.
#'
#'   By default all pairwise comparisons are drawn. Supply \code{comparisons} (a
#'   list of length-2 vectors of group levels) to draw only a subset, or
#'   \code{ref.group} to compare each group to a reference. Pairwise p-values are
#'   adjusted (\code{p.adjust.method}); the brackets are packed to the fewest
#'   levels (\code{pack = "auto"}, see \code{\link{geom_pwc}()}).
#' @inheritParams ggboxplot
#' @param data a data frame.
#' @param x,y x and y variables for drawing.
#' @param color,fill outline and fill colors.
#' @param palette the color palette to be used for coloring or filling by
#'   groups. Passed to the base builder.
#' @param ggtheme a ggplot theme. Default is \code{\link{theme_pubr}()}.
#' @param facet.by character vector, of length 1 or 2, specifying grouping
#'   variables for faceting the plot. When set, pairwise comparisons and the
#'   omnibus test are computed within each panel.
#' @param labeller passed to \code{\link{facet}()}.
#' @param base the base plot geometry. One of \code{"boxplot"} (default),
#'   \code{"violin"} or \code{"stripchart"}.
#' @param add character vector adding other plot elements through the base
#'   builder (e.g. \code{"jitter"}, \code{"mean"}, \code{"mean_sd"}). Default
#'   \code{c("jitter", "mean")}.
#' @param add.params a list of parameters passed to the \code{add} layers.
#' @param comparisons a list of length-2 vectors giving the group levels to
#'   compare. If \code{NULL} (default), all pairwise comparisons are drawn.
#'   Subsetting is supported for the tests that accept a \code{comparisons}
#'   argument (\code{"t_test"}, \code{"wilcox_test"}, \code{"sign_test"},
#'   \code{"emmeans_test"}); the all-pairwise-only tests (\code{"dunn_test"},
#'   \code{"games_howell_test"}, \code{"tukey_hsd"}) draw all pairs, so
#'   \code{comparisons} is not allowed with them (use \code{ref.group} instead).
#'
#'   \strong{Two-way / grouped mode.} When a second factor is mapped through
#'   \code{color} or \code{fill} (a data column other than \code{x}),
#'   \code{ggcompare()} composes a two-way figure in one call: dodged boxes for
#'   the two factors, simple pairwise comparison brackets of one factor within
#'   each level of the other (see \code{pwc.group.by}), and a subtitle from the
#'   two-way ANOVA that names the interaction. In this mode the pairwise
#'   \code{method} defaults to \code{"emmeans_test"} (pooled-model simple
#'   comparisons), \code{p.adjust.method} to \code{"bonferroni"} and
#'   \code{hide.ns} to \code{TRUE}; each remains overridable. With \code{facet.by}
#'   the pairwise comparisons and the two-way interaction are computed \emph{per
#'   panel}, and a compact interaction label is drawn inside each panel (in place
#'   of the single pooled subtitle). Set \code{simple.effects = TRUE} to also draw
#'   each \code{x} group's simple main effect of the compared factor above its
#'   brackets, computed with the pooled error from the full two-way model. The
#'   one-way path is unchanged.
#' @param ref.group a group level used as the reference; each other group is
#'   compared to it. See \code{\link{geom_pwc}()}.
#' @param method the pairwise comparison test. Default \code{"t_test"}. See
#'   \code{\link{geom_pwc}()} for allowed methods.
#' @param method.args additional arguments passed to the pairwise test.
#' @param p.adjust.method method for adjusting the pairwise p-values.
#' @param label the bracket label, a \code{\link[glue]{glue}} template. Default
#'   \code{"\{p.adj.signif\}"} (significance stars).
#' @param effsize logical. If \code{TRUE}, the effect size is appended to each
#'   bracket label. The symbol matches the effect size the \code{method}
#'   reports: \code{"d"} (Cohen's d) for \code{"t_test"} and
#'   \code{"games_howell_test"}, \code{"delta"} (Cliff's delta) for
#'   \code{"wilcox_test"}, and \code{"r"} for \code{"dunn_test"}. See
#'   \code{\link{geom_pwc}()}.
#' @param hide.ns logical or character; hide non-significant brackets. See
#'   \code{\link{geom_pwc}()}.
#' @param pack how the brackets are stacked; \code{"auto"} (default) packs them
#'   compactly, \code{"none"} keeps one per level. See \code{\link{geom_pwc}()}.
#' @param pwc.args additional arguments passed to \code{\link{geom_pwc}()}
#'   (e.g. \code{step.increase}, \code{tip.length}, \code{bracket.nudge.y}).
#' @param omnibus the omnibus test shown as the plot subtitle. One of
#'   \code{"anova"} (default), \code{"kruskal"} or \code{"none"}.
#' @param omnibus.args additional arguments passed to
#'   \code{\link{add_test_label}()} (non-faceted plots) or the corresponding stat
#'   layer (faceted plots).
#' @param pwc.group.by used only in the two-way (grouped) mode described below.
#'   Direction of the grouped pairwise comparisons: \code{"x.var"} (default)
#'   compares the second factor within each \code{x} group; \code{"legend.var"}
#'   compares the \code{x} groups within each level of the second factor.
#' @param simple.effects logical, used only in the two-way (grouped) mode. When
#'   \code{TRUE}, the simple main effect of the compared factor within each
#'   \code{x} group is drawn above that group's brackets, computed with the
#'   pooled error term from the full two-way model (so its denominator degrees of
#'   freedom are the full-model residual df). Default \code{FALSE}. Supported for
#'   the default \code{pwc.group.by = "x.var"} direction and without
#'   \code{facet.by}; other configurations issue a warning and are skipped.
#' @param ... additional arguments passed to the base builder (e.g.
#'   \code{ggboxplot}), such as \code{title}, \code{xlab}, \code{ylab}.
#' @return a single customizable \code{ggplot}.
#' @seealso \code{\link{ggboxplot}}, \code{\link{geom_pwc}},
#'   \code{\link{add_test_label}}, \code{\link{ggsummarystats}}.
#' @examples
#' df <- ToothGrowth
#' df$dose <- as.factor(df$dose)
#'
#' # Default: box + jitter + mean + all-pairwise adjusted brackets + ANOVA subtitle
#' ggcompare(df, x = "dose", y = "len")
#'
#' # Non-parametric, only a subset of comparisons, with effect sizes
#' ggcompare(df, x = "dose", y = "len",
#'   method = "wilcox_test", omnibus = "kruskal",
#'   comparisons = list(c("0.5", "1"), c("1", "2")),
#'   effsize = TRUE, hide.ns = TRUE
#' )
#'
#' # Composed with a summary-statistics table
#' ggsummarystats(df, x = "dose", y = "len", ggfunc = ggcompare)
#'
#' # Two-way / grouped design: a second factor mapped through `color` triggers
#' # dodged boxes, simple pairwise brackets within each x group, and a subtitle
#' # naming the interaction. (Uses emmeans_test by default, so guard on emmeans.)
#' if (requireNamespace("emmeans", quietly = TRUE)) {
#'   data("ToothGrowth")
#'   tg <- ToothGrowth
#'   tg$dose <- as.factor(tg$dose)
#'   ggcompare(tg, x = "supp", y = "len", color = "dose")
#'
#'   # Add each x group's simple main effect (pooled-error F) above its brackets
#'   ggcompare(tg, x = "supp", y = "len", color = "dose", simple.effects = TRUE)
#' }
#'
#' @export
ggcompare <- function(data, x, y,
                      color = "black", fill = "white", palette = "jco",
                      ggtheme = theme_pubr(), facet.by = NULL,
                      labeller = "label_value",
                      base = c("boxplot", "violin", "stripchart"),
                      add = c("jitter", "mean"), add.params = list(),
                      comparisons = NULL, ref.group = NULL,
                      method = "t_test", method.args = list(),
                      p.adjust.method = "holm", label = "{p.adj.signif}",
                      effsize = FALSE, hide.ns = FALSE,
                      pack = c("auto", "none"), pwc.args = list(),
                      omnibus = c("anova", "kruskal", "none"),
                      omnibus.args = list(),
                      pwc.group.by = c("x.var", "legend.var"),
                      simple.effects = FALSE, ...) {
  base <- match.arg(base)
  pack <- match.arg(pack)
  omnibus <- match.arg(omnibus)
  pwc.group.by <- match.arg(pwc.group.by)

  # Record which comparison controls the caller left at their defaults, so a
  # two-way call can adopt two-way-appropriate defaults (emmeans / Bonferroni /
  # hide ns) without overriding an explicit choice. Captured before any coercion.
  method.missing <- missing(method)
  padj.missing <- missing(p.adjust.method)
  hide.ns.missing <- missing(hide.ns)
  color.missing <- missing(color)
  fill.missing <- missing(fill)

  # Coerce x to a factor so the discrete positions (and the comparison
  # translation below) are well defined; keep an existing factor's level order.
  if (!is.factor(data[[x]])) {
    data[[x]] <- factor(data[[x]])
  }

  # Two-way mode is entered only when a SECOND factor is mapped through
  # color/fill (a data column distinct from x). Everything below is gated on
  # this, so a one-way call (color/fill a fixed colour) is byte-identical.
  # Only an EXPLICITLY supplied color/fill that names a data column (other than
  # x) is treated as the second factor; the "black"/"white" defaults never
  # trigger two-way mode, even if the data happens to carry such a column.
  group.var <- NULL
  is_col <- function(v) is.character(v) && length(v) == 1L && v %in% colnames(data)
  if (!color.missing && is_col(color) && color != x) {
    group.var <- color
  } else if (!fill.missing && is_col(fill) && fill != x) {
    group.var <- fill
  }
  two.way <- !is.null(group.var)
  if (two.way) {
    if (!is.factor(data[[group.var]])) data[[group.var]] <- factor(data[[group.var]])
    # Simple pairwise comparisons in a two-way design are best drawn from the
    # pooled-model emmeans test with Bonferroni adjustment; declutter by hiding
    # ns. These are defaults only - an explicit argument always wins.
    if (method.missing) method <- "emmeans_test"
    if (padj.missing) p.adjust.method <- "bonferroni"
    if (hide.ns.missing) hide.ns <- TRUE
  }
  if (isTRUE(simple.effects) && !two.way) {
    warning("`simple.effects` applies only in two-way (grouped) mode ",
      "(map a second factor via `color`/`fill`); ignoring it.", call. = FALSE)
  }

  if (missing(ggtheme) && !is.null(facet.by)) {
    ggtheme <- theme_pubr(border = TRUE)
  }

  builder <- switch(base,
    boxplot = ggboxplot, violin = ggviolin, stripchart = ggstripchart
  )

  # (1) Base plot with jitter/mean, sharing the ggfunc contract arguments.
  p <- builder(
    data, x = x, y = y, color = color, fill = fill, palette = palette,
    ggtheme = ggtheme, facet.by = facet.by, labeller = labeller,
    add = add, add.params = add.params, ...
  )

  # Resolve the pairwise method to a canonical rstatix name (used for the
  # effect-size symbol) and its test function (used for the comparisons guard).
  # `method` may be a name, a name with dots/aliases, or a function object.
  method.name <- NA_character_
  test.fun <- NULL
  if (is.character(method)) {
    method.name <- switch(gsub("[.]", "_", method),
      wilcoxon = "wilcox_test", emmeans = "emmeans_test",
      games_howell = "games_howell_test", gsub("[.]", "_", method)
    )
    if (exists(method.name, envir = asNamespace("rstatix"), inherits = FALSE)) {
      test.fun <- get(method.name, envir = asNamespace("rstatix"))
    }
  } else if (is.function(method)) {
    test.fun <- method
    known <- c(
      "t_test", "wilcox_test", "dunn_test", "games_howell_test",
      "sign_test", "tukey_hsd", "emmeans_test"
    )
    for (nm in known) {
      if (exists(nm, envir = asNamespace("rstatix"), inherits = FALSE) &&
        identical(method, get(nm, envir = asNamespace("rstatix")))) {
        method.name <- nm
        break
      }
    }
  }
  # Normalize a recognized function-object method to its canonical name so it
  # takes the same (string) code path in geom_pwc() - which handles p-value
  # adjustment internally for the all-pairwise tests - as the equivalent string
  # method. An unrecognized custom function is passed through untouched.
  if (is.function(method) && !is.na(method.name)) {
    method <- method.name
  }

  # (2) Append the effect-size token to the (glue) label when requested. The
  # symbol reflects the effect size the chosen method reports (Cohen's d for
  # t_test/games_howell_test, Cliff's delta for wilcox_test, r for dunn_test),
  # so a rank-based effect size is not mislabelled as "d". A bare column token
  # is braced first so it stays a valid glue template.
  if (isTRUE(effsize) && !two.way) {
    es.symbol <- switch(if (is.na(method.name)) "" else method.name,
      t_test = "d", games_howell_test = "d",
      wilcox_test = "delta", dunn_test = "r",
      "effsize"
    )
    if (!grepl("\\{", label)) label <- paste0("{", label, "}")
    label <- paste0(label, ", ", es.symbol, "={effsize}")
  }

  # ---- Two-way / grouped mode ------------------------------------------------
  # When a second factor is mapped through color/fill, draw simple pairwise
  # comparisons of one factor within each level of the other, from a PRECOMPUTED
  # rstatix test (positioned with add_xy_position(), drawn with
  # stat_pvalue_manual()), plus a two-way omnibus subtitle that names the
  # interaction. The pooled-model emmeans / per-group pairwise values are
  # computed directly here because geom_pwc()'s internal grouped test does not
  # reproduce them for a multi-level legend.
  if (two.way) {
    if (isTRUE(effsize)) {
      warning("`effsize` is not supported in two-way (grouped) mode; ignoring it.",
        call. = FALSE)
    }
    if (!is.null(comparisons)) {
      stop("`comparisons` subsetting is not supported in two-way (grouped) mode; ",
        "all pairwise comparisons are drawn within each group. Use `ref.group` ",
        "to compare against a reference level.", call. = FALSE)
    }
    if (is.function(method)) {
      stop("A function `method` is not supported in two-way (grouped) mode; ",
        "pass a method name such as 'emmeans_test', 't_test' or 'wilcox_test'.",
        call. = FALSE)
    }
    if (is.na(method.name) ||
      !exists(method.name, envir = asNamespace("rstatix"), inherits = FALSE)) {
      stop("Unknown two-way pairwise `method`: '", method, "'.", call. = FALSE)
    }
    # group.by = "x.var" (default): group by x, compare the second factor within
    # each x. "legend.var": the reverse.
    if (pwc.group.by == "x.var") {
      grp.var <- x
      comp.var <- group.var
    } else {
      grp.var <- group.var
      comp.var <- x
    }
    test.fun2 <- get(method.name, envir = asNamespace("rstatix"))
    test.formula <- stats::as.formula(paste(y, "~", comp.var))
    # Group by the faceting variable(s) as well, so the pairwise test is run
    # per panel; facet.by is NULL for a single (non-faceted) panel.
    grouped.data <- dplyr::group_by(
      data, dplyr::across(dplyr::all_of(c(facet.by, grp.var)))
    )
    call.args <- c(list(grouped.data, test.formula), method.args)
    if ("p.adjust.method" %in% names(formals(test.fun2))) {
      call.args$p.adjust.method <- p.adjust.method
    }
    if (!is.null(ref.group)) {
      if (!("ref.group" %in% names(formals(test.fun2)))) {
        stop("`ref.group` is not supported for method = '", method,
          "' in two-way mode.", call. = FALSE)
      }
      call.args$ref.group <- as.character(ref.group)
    }
    pwc <- do.call(test.fun2, call.args)
    # Position the brackets against the plotted layout: x is always the plot's
    # x axis and the second factor is the dodge group -- regardless of the
    # comparison direction. (Positioning on `grp.var` would place legend.var
    # brackets on the wrong x scale, off the panel.)
    pwc <- rstatix::add_xy_position(pwc, x = x, group = group.var, dodge = 0.8)
    spm.call <- c(list(data = pwc, label = label, hide.ns = hide.ns), pwc.args)
    p <- p + do.call(stat_pvalue_manual, spm.call)

    # Caption describing the pairwise test (kept unless the caller already set
    # one). Matches the subtitle's output type.
    cap.type <- if (!is.null(omnibus.args$type)) omnibus.args$type else "expression"
    if (is.null(p$labels$caption)) {
      p <- p + ggplot2::labs(caption = rstatix::get_pwc_label(pwc, type = cap.type))
    }

    # Optional per-group simple main effects drawn above each cluster's brackets.
    # Supported for the default comparison direction (grp.var = the x axis) and
    # without facets; other configurations warn and are skipped rather than
    # placing a label with no clear anchor.
    if (isTRUE(simple.effects)) {
      if (!is.null(facet.by)) {
        warning("`simple.effects` is not supported together with `facet.by`; ",
          "ignoring it.", call. = FALSE)
      } else if (pwc.group.by != "x.var") {
        warning("`simple.effects = TRUE` is only supported with ",
          "`pwc.group.by = \"x.var\"`; ignoring it.", call. = FALSE)
      } else {
        p <- .add_simple_effect_labels(p, data, x, y, group.var, grp.var, comp.var, pwc)
      }
    }

    # Omnibus test. Without facets, one subtitle names the pooled two-way
    # interaction. With facets, the interaction is per panel, so a compact label
    # is drawn inside each panel instead (a single subtitle would misleadingly
    # pool the panels).
    if (is.null(facet.by)) {
      if (omnibus == "anova") {
        p <- do.call(
          add_test_label,
          c(list(p, method = "anova", group.by = group.var), omnibus.args)
        )
      } else if (omnibus == "kruskal") {
        p <- do.call(add_test_label, c(list(p, method = "kruskal"), omnibus.args))
      }
    } else if (omnibus == "anova") {
      p <- .add_faceted_interaction_labels(p, data, x, y, group.var, facet.by)
    }
    return(p)
  }

  # (3) Translate a subset of comparisons from group levels to the 1-based
  # x-axis positions that geom_pwc()'s StatPwc expects.
  if (!is.null(comparisons)) {
    # Only tests that accept a `comparisons` argument can draw a subset; the
    # all-pairwise-only tests (dunn_test, games_howell_test, tukey_hsd) would
    # silently drop the subset and render nothing, so refuse with a clear
    # message rather than producing an empty figure.
    if (!is.null(test.fun) && !("comparisons" %in% names(formals(test.fun)))) {
      stop(
        "`comparisons` subsetting is not supported for method = '",
        if (is.character(method)) method else "<function>",
        "' (it computes all pairwise comparisons). Use `ref.group`, a method ",
        "such as 't_test' or 'wilcox_test', or omit `comparisons` to draw all ",
        "pairwise comparisons.",
        call. = FALSE
      )
    }
    lv <- levels(data[[x]])
    pos <- lapply(comparisons, function(pair) as.character(match(as.character(pair), lv)))
    if (any(vapply(pos, anyNA, logical(1)))) {
      stop(
        "`comparisons` contains a group not found in `", x, "`. ",
        "Available levels: ", paste(lv, collapse = ", "), ".",
        call. = FALSE
      )
    }
    method.args$comparisons <- pos
  }

  # (4) Pairwise comparison brackets (adjusted p, packed).
  pwc.call <- c(
    list(
      method = method, method.args = method.args, ref.group = ref.group,
      label = label, p.adjust.method = p.adjust.method,
      hide.ns = hide.ns, pack = pack
    ),
    pwc.args
  )
  p <- p + do.call(geom_pwc, pwc.call)

  # (5) Omnibus test label. add_test_label() pools facet panels, so faceted
  # plots use the per-panel stat layer instead.
  if (omnibus != "none") {
    if (is.null(facet.by)) {
      p <- do.call(add_test_label, c(list(p, method = omnibus), omnibus.args))
    } else {
      stat.fun <- if (omnibus == "anova") stat_anova_test else stat_kruskal_test
      p <- p + do.call(stat.fun, omnibus.args)
    }
  }

  p
}


# Draw a compact two-way interaction label inside each facet panel: for a
# faceted grouped comparison, the interaction (x * group.var) is computed per
# panel and placed at the top-center of the panel. Returns the updated plot.
.add_faceted_interaction_labels <- function(p, data, x, y, group.var, facet.by) {
  fmt.p <- function(pv) {
    if (is.na(pv)) return("p = NA")
    if (pv < 0.001) "p < 0.001" else paste0("p = ", formatC(pv, format = "fg", digits = 2))
  }
  one <- function(df) {
    at <- rstatix::get_anova_table(rstatix::anova_test(
      df, stats::as.formula(paste(y, "~", x, "*", group.var))
    ))
    ir <- which(grepl(":", at$Effect))[1]
    # A degenerate panel (aliased/empty design) has no estimable interaction:
    # omit its label rather than print "F(NA,NA) = NA".
    if (is.na(ir) || is.na(at$F[ir])) return(NA_character_)
    sprintf("Interaction: F(%g,%g) = %.2f, %s",
      at$DFn[ir], at$DFd[ir], at$F[ir], fmt.p(at$p[ir]))
  }
  groups <- do.call(paste, c(data[, facet.by, drop = FALSE], sep = "\r"))
  parts <- split(seq_len(nrow(data)), groups)
  labs <- lapply(parts, function(idx) {
    d <- data[idx, , drop = FALSE]
    lab <- d[1, facet.by, drop = FALSE]
    lab$.label <- one(d)
    lab
  })
  labs <- do.call(rbind, labs)
  labs <- labs[!is.na(labs$.label), , drop = FALSE]
  if (nrow(labs) == 0L) {
    return(p + ggplot2::scale_y_continuous(expand = ggplot2::expansion(mult = c(0.05, 0.15))))
  }
  # Center on the discrete x axis; y = Inf pins to the panel top (extra top
  # expansion keeps the label clear of the brackets).
  n.x <- nlevels(factor(data[[x]]))
  labs$.x <- (1 + n.x) / 2
  p +
    ggplot2::geom_text(
      data = labs, inherit.aes = FALSE,
      ggplot2::aes(x = .data[[".x"]], y = Inf, label = .data[[".label"]]),
      vjust = 1.4, size = 3
    ) +
    ggplot2::scale_y_continuous(expand = ggplot2::expansion(mult = c(0.05, 0.15)))
}


# Draw per-group simple-main-effect F labels above each cluster's brackets. The
# simple main effect of `comp.var` within each level of `grp.var` is computed
# with the pooled error term from the full two-way model
# lm(y ~ x * group.var), so its denominator df is the full-model residual df
# (not a smaller per-subset df). `grp.var` is the x-axis variable (the default
# pwc.group.by = "x.var" direction), so each label sits at that x position, just
# above the tallest bracket of the cluster (read from the already-positioned
# `pwc`). Returns the updated plot; a degenerate group with no estimable simple
# effect is skipped rather than labelled "F(NA,NA)".
.add_simple_effect_labels <- function(p, data, x, y, group.var, grp.var, comp.var, pwc) {
  model <- stats::lm(
    stats::as.formula(paste(y, "~", x, "*", group.var)), data = data
  )
  grouped <- dplyr::group_by(data, dplyr::across(dplyr::all_of(grp.var)))
  sme <- tryCatch(
    rstatix::get_anova_table(rstatix::anova_test(
      grouped, stats::as.formula(paste(y, "~", comp.var)), error = model
    )),
    error = function(e) NULL
  )
  if (is.null(sme) || nrow(sme) == 0L) {
    return(p)
  }
  sme <- as.data.frame(sme)
  # Format one compact "F(df1,df2) = F, p" label per group, matching the
  # documented simple-effects recipe.
  fmt.p <- function(pv) {
    ifelse(pv < 1e-4, "p < 0.0001", paste0("p = ", signif(pv, 2)))
  }
  lv <- levels(factor(data[[grp.var]]))
  # Key the tallest-bracket height by the integer x position (the midpoint of
  # each bracket's xmin/xmax) rather than by pwc[[grp.var]]: add_xy_position()
  # adds synthetic columns (x, xmin, xmax, groups, ...) that can shadow the
  # grouping column when the x variable is itself named, e.g., "x".
  x.idx <- round((pwc$xmin + pwc$xmax) / 2)
  tops <- tapply(pwc$y.position, x.idx, max)
  gap <- 0.06 * diff(range(data[[y]], na.rm = TRUE))
  sme$.x <- match(as.character(sme[[grp.var]]), lv)
  sme$.y <- as.numeric(tops[as.character(sme$.x)]) + gap
  sme$.label <- sprintf(
    "F(%g,%g) = %.3g, %s", sme$DFn, sme$DFd, sme$F, fmt.p(sme$p)
  )
  sme <- sme[is.finite(sme$.y) & is.finite(sme$F) & !is.na(sme$.label), , drop = FALSE]
  if (nrow(sme) == 0L) {
    return(p)
  }
  p +
    ggplot2::geom_text(
      data = sme, inherit.aes = FALSE,
      ggplot2::aes(x = .data[[".x"]], y = .data[[".y"]], label = .data[[".label"]]),
      size = 3, vjust = 0, fontface = "italic"
    ) +
    ggplot2::scale_y_continuous(expand = ggplot2::expansion(mult = c(0.05, 0.28)))
}
