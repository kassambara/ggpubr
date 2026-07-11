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
                      omnibus.args = list(), ...) {
  base <- match.arg(base)
  pack <- match.arg(pack)
  omnibus <- match.arg(omnibus)

  # Coerce x to a factor so the discrete positions (and the comparison
  # translation below) are well defined; keep an existing factor's level order.
  if (!is.factor(data[[x]])) {
    data[[x]] <- factor(data[[x]])
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

  # (2) Append the effect-size token to the (glue) label when requested. The
  # symbol reflects the effect size the chosen method reports (Cohen's d for
  # t_test/games_howell_test, Cliff's delta for wilcox_test, r for dunn_test),
  # so a rank-based effect size is not mislabelled as "d". A bare column token
  # is braced first so it stays a valid glue template.
  if (isTRUE(effsize)) {
    es.symbol <- switch(method,
      t_test = "d", games_howell_test = "d",
      wilcox_test = "delta", dunn_test = "r",
      "effsize"
    )
    if (!grepl("\\{", label)) label <- paste0("{", label, "}")
    label <- paste0(label, ", ", es.symbol, "={effsize}")
  }

  # (3) Translate a subset of comparisons from group levels to the 1-based
  # x-axis positions that geom_pwc()'s StatPwc expects.
  if (!is.null(comparisons)) {
    # Only tests that accept a `comparisons` argument can draw a subset; the
    # all-pairwise-only tests (dunn_test, games_howell_test, tukey_hsd) would
    # silently drop the subset and render nothing, so refuse with a clear
    # message rather than producing an empty figure.
    method.name <- if (is.character(method)) gsub("[.]", "_", method) else NA_character_
    if (!is.na(method.name)) {
      method.name <- switch(method.name,
        wilcoxon = "wilcox_test", emmeans = "emmeans_test",
        games_howell = "games_howell_test", method.name
      )
    }
    is.rstatix.fn <- !is.na(method.name) &&
      exists(method.name, envir = asNamespace("rstatix"), inherits = FALSE)
    if (is.rstatix.fn &&
      !("comparisons" %in% names(formals(get(method.name, envir = asNamespace("rstatix")))))) {
      stop(
        "`comparisons` subsetting is not supported for method = '", method,
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
