#' @include utilities.R
NULL
#' Add an Omnibus Test Label to a GGPlot
#' @description Adds, in one call, the omnibus statistical test result as a plot
#'   subtitle (e.g. the one-way ANOVA \code{F}, generalized eta squared and
#'   p-value, or the Kruskal-Wallis statistic), and optionally a pairwise
#'   comparison description as a plot caption. This reproduces the common idiom
#'   \code{p + labs(subtitle = get_test_label(...), caption = get_pwc_label(...))}
#'   in a single step, reusing the re-exported \code{\link[rstatix]{get_test_label}}
#'   and \code{\link[rstatix]{get_pwc_label}} helpers.
#' @details The omnibus test is computed on the plot data using the plot's
#'   \code{x} (grouping) and \code{y} (outcome) mappings, as a one-way design
#'   (\code{y ~ x}). For a faceted plot the test pools all panels together (a
#'   warning is issued), so for panel-specific tests use
#'   \code{\link{stat_anova_test}()}/\code{\link{stat_kruskal_test}()} instead.
#' @param p a ggplot, typically a \code{\link{ggboxplot}()},
#'   \code{\link{ggviolin}()} or \code{\link{ggstripchart}()} with a discrete
#'   \code{x} axis and a continuous \code{y} axis.
#' @param method the omnibus test. Either \code{"anova"} (default, one-way ANOVA)
#'   or \code{"kruskal"} (Kruskal-Wallis).
#' @param caption logical. If \code{TRUE}, a pairwise comparison caption is added
#'   (describing the pairwise test and p-value adjustment). Default is
#'   \code{FALSE} (subtitle only).
#' @param pwc.method the pairwise test used for the caption when
#'   \code{caption = TRUE}. If \code{NULL} (default), \code{"tukey_hsd"} is used
#'   for \code{method = "anova"} and \code{"dunn_test"} for
#'   \code{method = "kruskal"}. Allowed values include \code{"tukey_hsd"},
#'   \code{"games_howell_test"}, \code{"dunn_test"}, \code{"wilcox_test"} and
#'   \code{"t_test"}.
#' @param detailed logical passed to \code{\link[rstatix]{get_test_label}}. If
#'   \code{TRUE} (default), the detailed test label (statistic, degrees of
#'   freedom, effect size, n) is shown.
#' @param type the label output type, one of \code{"expression"} (default) or
#'   \code{"text"}.
#' @param p.adjust.method method for adjusting the pairwise p-values used by the
#'   caption (see \code{\link[stats]{p.adjust}}); ignored by \code{"tukey_hsd"}
#'   and \code{"games_howell_test"}, which adjust internally.
#' @param group.by optional name of a second grouping factor (typically the one
#'   mapped to \code{color}/\code{fill}). When supplied, a two-way ANOVA
#'   \code{y ~ x * group.by} is computed and the subtitle names the selected
#'   effect (see \code{effect}). Only used when \code{method = "anova"}. Default
#'   \code{NULL} (one-way \code{y ~ x}, the historical behavior).
#' @param effect which effect of the two-way ANOVA to label (only used when
#'   \code{group.by} is set). One of \code{"interaction"} (default, the
#'   \code{x:group.by} row, described as \dQuote{Interaction (x \eqn{\times}
#'   group.by)}), \code{"all"} (a multi-line label with both main effects and the
#'   interaction), or a term name (e.g. \code{"gender"} or
#'   \code{"gender:education_level"}) or a numeric row index of the ANOVA table.
#'   The default interaction label is drawn on two lines (the effect name above
#'   the statistics) so it does not overflow at single-column figure widths; a
#'   caller-supplied \code{description} keeps it on one line.
#' @param ... additional arguments passed to
#'   \code{\link[rstatix]{get_test_label}} (e.g. \code{style = "apa"}, or
#'   \code{description} to override the auto-derived effect name).
#' @return the input ggplot with the subtitle (and optionally caption) added.
#' @seealso \code{\link{stat_anova_test}}, \code{\link{stat_kruskal_test}},
#'   \code{\link[rstatix]{get_test_label}}, \code{\link[rstatix]{get_pwc_label}}.
#' @examples
#' df <- ToothGrowth
#' df$dose <- as.factor(df$dose)
#'
#' # Omnibus ANOVA subtitle (pass the plot to add_test_label())
#' p <- ggboxplot(df, x = "dose", y = "len")
#' add_test_label(p)
#'
#' # Kruskal-Wallis, with a pairwise (Dunn) caption
#' p2 <- ggboxplot(df, x = "dose", y = "len") +
#'   geom_pwc(method = "dunn_test", label = "p.adj.signif", hide.ns = TRUE)
#' add_test_label(p2, method = "kruskal", caption = TRUE)
#'
#' # Two-way design: name the interaction in the subtitle
#' p3 <- ggboxplot(df, x = "dose", y = "len", color = "supp")
#' add_test_label(p3, group.by = "supp", type = "text")
#'
#' @export
add_test_label <- function(p, method = c("anova", "kruskal"),
                           caption = FALSE, pwc.method = NULL,
                           detailed = TRUE, type = c("expression", "text"),
                           p.adjust.method = "holm",
                           group.by = NULL, effect = NULL, ...) {
  if (!inherits(p, "ggplot")) {
    stop("`p` must be a ggplot.", call. = FALSE)
  }
  method <- match.arg(method)
  type <- match.arg(type)

  # Resolve the x (grouping) and y (outcome) columns from the plot mapping.
  if (is.null(p$mapping$x) || is.null(p$mapping$y)) {
    stop("The plot must have both `x` and `y` aesthetics mapped.", call. = FALSE)
  }
  x <- rlang::as_label(p$mapping$x)
  y <- rlang::as_label(p$mapping$y)
  if (!all(c(x, y) %in% colnames(p$data))) {
    stop(
      "Could not find the mapped x ('", x, "') and y ('", y, "') columns in the ",
      "plot data. add_test_label() expects a plot with plain column mappings.",
      call. = FALSE
    )
  }

  # A second grouping factor turns the omnibus into a two-way ANOVA whose named
  # effect (by default the interaction) becomes the subtitle. This is gated
  # entirely behind a non-NULL `group.by`, so the one-way call below is
  # byte-identical to the historical behavior.
  two.way <- !is.null(group.by)
  if (two.way && method != "anova") {
    warning(
      "Naming a two-way effect requires method = 'anova'; ignoring `group.by`.",
      call. = FALSE
    )
    two.way <- FALSE
  }
  if (two.way && !(group.by %in% colnames(p$data))) {
    stop("`group.by` column ('", group.by, "') not found in the plot data.",
      call. = FALSE
    )
  }

  if (!inherits(p$facet, "FacetNull")) {
    warning(
      "add_test_label() computes a single test pooling all facet panels. ",
      "For panel-specific tests use stat_anova_test()/stat_kruskal_test().",
      call. = FALSE
    )
  }

  .data <- p$data
  .formula <- stats::as.formula(paste(y, "~", x))

  # Omnibus test -> subtitle
  if (two.way) {
    subtitle.label <- .two_way_test_label(
      .data, x = x, y = y, group.by = group.by, effect = effect,
      detailed = detailed, type = type, ...
    )
  } else {
    omnibus <- switch(method,
      anova = rstatix::anova_test(.data, .formula),
      kruskal = rstatix::kruskal_test(.data, .formula)
    )
    subtitle.label <- rstatix::get_test_label(
      omnibus, detailed = detailed, type = type, ...
    )
  }

  # Only set the labels we actually produce, using separate labs() calls. In
  # particular, when caption = FALSE we must NOT touch the caption at all,
  # because labs(caption = NULL) would remove a caption the user had already
  # added to the plot. (A single do.call(labs, ...) is avoided because it would
  # evaluate the plotmath expression label rather than pass it through.)
  p <- p + ggplot2::labs(subtitle = subtitle.label)
  if (caption) {
    if (is.null(pwc.method)) {
      pwc.method <- if (method == "anova") "tukey_hsd" else "dunn_test"
    }
    pwc.fun <- utils::getFromNamespace(pwc.method, "rstatix")
    pwc.args <- list(.data, .formula)
    if (pwc.method %in% c("dunn_test", "wilcox_test", "t_test")) {
      pwc.args$p.adjust.method <- p.adjust.method
    }
    pwc <- do.call(pwc.fun, pwc.args)
    p <- p + ggplot2::labs(caption = rstatix::get_pwc_label(pwc, type = type))
  }
  p
}

# Build the subtitle label for a two-way ANOVA y ~ x * group.by, naming the
# selected effect. `effect` is "interaction" (default), "all", a term name, or a
# numeric row index. Returns a text/expression label from get_test_label().
.two_way_test_label <- function(.data, x, y, group.by, effect = NULL,
                                detailed = TRUE, type = "expression", ...) {
  .formula <- stats::as.formula(paste(y, "~", x, "*", group.by))
  res.aov <- rstatix::anova_test(.data, .formula)
  atab <- rstatix::get_anova_table(res.aov)
  terms <- as.character(atab$Effect)
  # A term's readable description: the interaction term "A:B" reads
  # "Interaction (A x B)"; a main effect keeps its term name.
  describe_term <- function(term) {
    if (grepl(":", term)) {
      paste0("Interaction (", gsub(":", " \u00d7 ", term), ")")
    } else {
      term
    }
  }
  dots <- list(...)
  user.description <- "description" %in% names(dots)

  # Resolve `effect` to the ANOVA-table row(s) to label.
  if (is.null(effect)) effect <- "interaction"
  if (identical(effect, "all")) {
    rows <- seq_along(terms)
  } else if (identical(effect, "interaction")) {
    rows <- which(grepl(":", terms))
    if (length(rows) == 0L) rows <- length(terms) # fall back to the last row
  } else if (is.numeric(effect)) {
    rows <- as.integer(effect)
  } else {
    # Match a term name, tolerating "A:B" / "A*B" and reversed factor order.
    key <- function(z) paste(sort(strsplit(gsub("[*]", ":", z), ":")[[1]]), collapse = ":")
    rows <- which(vapply(terms, function(t) key(t) == key(effect), logical(1)))
    if (length(rows) == 0L) {
      stop("`effect` = '", effect, "' does not match an ANOVA term. Available: ",
        paste(terms, collapse = ", "), ".", call. = FALSE)
    }
  }

  make_label <- function(row) {
    args <- c(
      list(res.aov, row = row, detailed = detailed, type = type),
      if (!user.description) list(description = describe_term(terms[row])),
      dots
    )
    do.call(rstatix::get_test_label, args)
  }

  # Two-line variant: the effect description on its own line above the
  # statistics. A one-line interaction label ("Interaction (A x B), F(...), ...,
  # n = N") overflows the right edge at single-column figure widths; wrapping
  # keeps it fully visible with no loss of information.
  wrap_label <- function(row) {
    desc <- describe_term(terms[row])
    stats <- do.call(rstatix::get_test_label, c(
      list(res.aov, row = row, detailed = detailed, type = type, description = ""),
      dots
    ))
    if (type == "text") {
      return(paste(desc, stats, sep = "\n"))
    }
    substitute(atop(d, s), list(d = desc, s = stats))
  }

  if (length(rows) == 1L) {
    # Wrap only the (long) default interaction label. A short main-effect label,
    # or a caller-supplied `description`, stays on a single line.
    if (!user.description && grepl(":", terms[rows])) {
      return(wrap_label(rows))
    }
    return(make_label(rows))
  }
  # Multi-line: stack one line per effect. Plain text joins with newlines;
  # plotmath stacks the per-line expressions with atop().
  labels <- lapply(rows, make_label)
  if (type == "text") {
    return(paste(unlist(labels), collapse = "\n"))
  }
  Reduce(function(a, b) substitute(atop(a, b), list(a = a, b = b)), labels)
}
