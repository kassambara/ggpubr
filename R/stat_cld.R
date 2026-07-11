#' @include utilities.R
NULL
#' Add Compact Letter Display to a GGPlot
#' @description Adds a compact letter display (CLD) of all-pairwise comparisons
#'   to a ggplot, such as box plots, violin plots, dot plots and stripcharts.
#'   One letter is placed above each group; groups that do \emph{not} share a
#'   letter are significantly different. This is a compact alternative to drawing
#'   many pairwise significance brackets when comparing several groups.
#' @details The letters are computed with \code{\link[rstatix]{add_cld}()} after
#'   an all-pairwise post-hoc test (Tukey HSD by default). The insert-and-absorb
#'   algorithm of Piepho (2004) is used; the results match
#'   \code{multcompView::multcompLetters()}.
#' @inheritParams ggpubr-common-params
#' @inheritParams ggplot2::layer
#' @param method the all-pairwise comparison test used to derive the letters.
#'   One of \code{"tukey_hsd"} (default), \code{"games_howell_test"},
#'   \code{"dunn_test"}, \code{"wilcox_test"} or \code{"t_test"}. These are the
#'   \code{rstatix} functions that produce an all-pairwise comparison table
#'   accepted by \code{\link[rstatix]{add_cld}()}.
#' @param p.adjust.method method for adjusting p values; passed to the pairwise
#'   test. Used only by \code{"dunn_test"}, \code{"wilcox_test"} and
#'   \code{"t_test"} (\code{"tukey_hsd"} and \code{"games_howell_test"} adjust
#'   internally). Allowed values include "holm", "hochberg", "hommel",
#'   "bonferroni", "BH", "BY", "fdr", "none".
#' @param threshold the significance threshold used to decide whether two groups
#'   share a letter (default 0.05). Comparisons with an adjusted p-value below
#'   \code{threshold} are treated as significant.
#' @param reversed logical. If \code{TRUE}, reverses the order in which the
#'   letters are assigned. Default is \code{FALSE}.
#' @param label.y \code{numeric}. Optional y coordinate (in data units) for the
#'   letters. If a single value, all letters are placed at that height; if
#'   \code{NULL} (default), each letter is placed just above its group's maximum
#'   value. When letters sit near the top of the panel, add head-room with
#'   \code{scale_y_continuous(expand = expansion(mult = c(0.05, 0.1)))} so they
#'   are not clipped.
#' @param vjust vertical justification of the letters, passed to
#'   \code{\link[ggplot2]{geom_text}()}. Default \code{-0.6} nudges the letters
#'   above the placement point.
#' @param ... other arguments passed to \code{\link[ggplot2]{geom_text}()}, such
#'   as \code{size}, \code{color} or \code{hjust}.
#' @param na.rm If FALSE (the default), removes missing values with a warning. If
#'   TRUE silently removes missing values.
#'
#' @section Computed variables: \itemize{ \item{label}: the compact letter
#'   assigned to each group. }
#'
#' @seealso \code{\link[rstatix]{add_cld}}, \code{\link{stat_anova_test}},
#'   \code{\link{stat_pwc}}.
#' @references Piepho, H.-P. (2004) An Algorithm for a Letter-Based
#'   Representation of All-Pairwise Comparisons. Journal of Computational and
#'   Graphical Statistics, 13(2), 456-466.
#'
#' @examples
#' # Data preparation
#' df <- ToothGrowth
#' df$dose <- as.factor(df$dose)
#'
#' # Basic boxplot with compact letter display
#' # (Tukey HSD all-pairwise comparisons).
#' # Add some head-room at the top so the letters are not clipped.
#' ggboxplot(df, x = "dose", y = "len") +
#'   stat_cld() +
#'   scale_y_continuous(expand = expansion(mult = c(0.05, 0.1)))
#'
#' # Non-parametric letters (Dunn test) and a common label height
#' ggboxplot(df, x = "dose", y = "len") +
#'   stat_cld(method = "dunn_test", label.y = 37)
#'
#' # Faceted plot: letters are computed within each panel
#' ggboxplot(df, x = "dose", y = "len", facet.by = "supp") +
#'   stat_cld() +
#'   scale_y_continuous(expand = expansion(mult = c(0.05, 0.1)))
#'
#' @export
stat_cld <- function(mapping = NULL, data = NULL,
                     method = c("tukey_hsd", "games_howell_test", "dunn_test",
                                "wilcox_test", "t_test"),
                     p.adjust.method = "holm", threshold = 0.05,
                     reversed = FALSE, label.y = NULL, vjust = -0.6,
                     geom = "text", position = "identity", na.rm = FALSE,
                     show.legend = FALSE, inherit.aes = TRUE, ...) {
  method <- match.arg(method)
  layer(
    stat = StatCld, data = data, mapping = mapping, geom = geom,
    position = position, show.legend = show.legend, inherit.aes = inherit.aes,
    params = list(
      method = method, p.adjust.method = p.adjust.method,
      threshold = threshold, reversed = reversed, label.y = label.y,
      vjust = vjust, na.rm = na.rm, ...
    )
  )
}


StatCld <- ggproto("StatCld", Stat,
  required_aes = c("x", "y"),
  # Drop inherited discrete grouping so a single letter is drawn per x group
  # (one all-pairwise test over the whole panel), not one per colour/fill level.
  setup_data = function(data, params) {
    data$group <- data$x
    data
  },
  compute_panel = function(data, scales, method, p.adjust.method,
                           threshold, reversed, label.y) {
    if (length(unique(data$x)) < 2) {
      stop("stat_cld() requires at least 2 groups on the x axis.", call. = FALSE)
    }
    df <- data.frame(x = factor(data$x), y = data$y)
    stat.test <- switch(method,
      tukey_hsd = rstatix::tukey_hsd(df, y ~ x),
      games_howell_test = rstatix::games_howell_test(df, y ~ x),
      dunn_test = rstatix::dunn_test(df, y ~ x, p.adjust.method = p.adjust.method),
      wilcox_test = rstatix::wilcox_test(df, y ~ x, p.adjust.method = p.adjust.method),
      t_test = rstatix::t_test(df, y ~ x, p.adjust.method = p.adjust.method)
    )
    cld <- rstatix::add_cld(stat.test, threshold = threshold, reversed = reversed)
    xpos <- as.numeric(as.character(cld$group))
    if (is.null(label.y)) {
      grp_max <- tapply(data$y, data$x, max, na.rm = TRUE)
      ypos <- as.numeric(grp_max[as.character(xpos)])
    } else {
      ypos <- rep_len(label.y, length(xpos))
    }
    data.frame(
      x = xpos, y = ypos, label = as.character(cld$cld),
      stringsAsFactors = FALSE
    )
  }
)
