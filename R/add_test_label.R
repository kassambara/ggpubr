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
#' @param ... additional arguments passed to
#'   \code{\link[rstatix]{get_test_label}} (e.g. \code{style = "apa"}).
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
#' @export
add_test_label <- function(p, method = c("anova", "kruskal"),
                           caption = FALSE, pwc.method = NULL,
                           detailed = TRUE, type = c("expression", "text"),
                           p.adjust.method = "holm", ...) {
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
  omnibus <- switch(method,
    anova = rstatix::anova_test(.data, .formula),
    kruskal = rstatix::kruskal_test(.data, .formula)
  )
  subtitle.label <- rstatix::get_test_label(
    omnibus, detailed = detailed, type = type, ...
  )

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
