#' @include utilities.R utilities_label.R utils_stat_test_label.R p_format_utils.R
NULL
#' Add Mean Comparison P-values to a ggplot
#' @description Add mean comparison p-values to a ggplot, such as box blots, dot
#'  plots and stripcharts.
#' @inheritParams ggpubr-common-params
#' @inheritParams ggplot2::layer
#' @inheritParams compare_means
#' @param method a character string indicating which method to be used for
#'  comparing means.
#' @param method.args a list of additional arguments used for the test method.
#'  For example one might use \code{method.args = list(alternative = "greater")}
#'  for wilcoxon test.
#' @param comparisons A list of length-2 vectors. The entries in the vector are
#'  either the names of 2 values on the x-axis or the 2 integers that correspond
#'  to the index of the groups of interest, to be compared.
#'
#'  Note: when \code{comparisons} is specified, each pairwise test is computed
#'  independently and the displayed p-values are \strong{not} adjusted for
#'  multiple comparisons. For p-values corrected for multiple testing, use
#'  \code{\link{geom_pwc}()}, or \code{\link{stat_pvalue_manual}()} together with
#'  \code{\link{compare_means}(..., p.adjust.method = )}.
#' @param hide.ns logical value. If TRUE, hide ns symbol when displaying
#'  significance levels.
#' @param label character string specifying label type. Allowed values include
#'  \code{"p.signif"} (shows the significance levels), \code{"p.format"} (shows
#'  the formatted p-value), and \code{"p.format.signif"} (shows the formatted
#'  p-value followed by significance stars, e.g., "p = 0.01 **").
#' @param p.format.style character string specifying the p-value formatting style.
#'  One of: \code{"default"} (backward compatible, uses scientific notation),
#'  \code{"apa"} (APA style, no leading zero), \code{"nejm"} (NEJM style),
#'  \code{"lancet"} (Lancet style), \code{"ama"} (AMA style), \code{"graphpad"}
#'  (GraphPad style), or \code{"scientific"} (scientific notation for GWAS).
#'  See \code{\link{list_p_format_styles}} for details.
#' @param p.digits integer specifying the number of decimal places for p-values.
#'  If provided, overrides the style default.
#' @param p.leading.zero logical indicating whether to include leading zero before
#'  decimal point (e.g., "0.05" vs ".05"). If provided, overrides the style default.
#' @param p.min.threshold numeric specifying the minimum p-value to display exactly.
#'  Values below this threshold are shown as "< threshold". If provided, overrides
#'  the style default.
#' @param p.decimal.mark character string to use as the decimal mark. If NULL,
#'  uses \code{getOption("OutDec")}.
#' @param signif.cutoffs numeric vector of p-value cutoffs in descending order
#'  for assigning significance symbols. For example, \code{c(0.10, 0.05, 0.01)}
#'  means p < 0.10 gets "*", p < 0.05 gets "**", p < 0.01 gets "***".
#'  If \code{use.four.stars = TRUE}, can include a fourth level.
#'  Default is NULL, which uses the package defaults.
#' @param signif.symbols character vector of symbols corresponding to
#'  \code{signif.cutoffs}. If NULL, auto-generated as "*", "**", "***"
#'  (and "****" if \code{use.four.stars = TRUE}).
#' @param ns.symbol character string for non-significant results. Default is "ns".
#'  Use "" (empty string) to show nothing.
#' @param use.four.stars logical. If TRUE, allows four stars (****) for the most
#'  significant level. Default is FALSE.
#' @param show.signif logical. If TRUE (default), shows significance symbols when
#'  using \code{label = "p.format.signif"}. If FALSE, falls back to showing only
#'  the p-value (equivalent to \code{label = "p.format"}) with a warning.
#' @param label.sep a character string to separate the terms. Default is ", ", to
#'  separate the correlation coefficient and the p.value.
#' @param label.x.npc,label.y.npc can be \code{numeric} or \code{character}
#'  vector of the same length as the number of groups and/or panels. If too
#'  short they will be recycled. \itemize{ \item If \code{numeric}, value should
#'  be between 0 and 1. Coordinates to be used for positioning the label,
#'  expressed in "normalized parent coordinates". \item If \code{character},
#'  allowed values include: i) one of c('right', 'left', 'center', 'centre',
#'  'middle') for x-axis; ii) and one of c( 'bottom', 'top', 'center', 'centre',
#'  'middle') for y-axis.}
#' @param vjust move the text up or down relative to the bracket.
#' @param tip.length numeric vector with the fraction that the bracket tips go
#'  down to indicate the precise column. Default is 0.03. Can be of
#'  same length as the number of comparisons to adjust specifically the tip
#'  length of each comparison. For example tip.length = c(0.01, 0.03).
#'
#'  If too short they will be recycled.
#'
#'  Note: when \code{comparisons} is set, brackets are drawn via
#'  \code{ggsignif::geom_signif()} and \code{tip.length} is a fraction of the
#'  trained data range, so the absolute tip length scales with your data (plots
#'  with different scales get different tip lengths). To obtain visually constant
#'  tip lengths across plots, compute the tests with \code{\link{compare_means}()}
#'  and draw them with \code{\link{stat_pvalue_manual}(..., tip.length.ref = "axis")}
#'  (which makes \code{tip.length} a fraction of the y-axis range). Passing
#'  \code{tip.length.ref} directly to \code{stat_compare_means()} has no effect
#'  and emits an informative message.
#' @param label.x,label.y \code{numeric} Coordinates (in data units) to be used
#'  for absolute positioning of the label. If too short they will be recycled.
#' @param bracket.size Width of the lines of the bracket.
#' @param step.increase numeric vector with the increase in fraction of total
#'  height for every additional comparison to minimize overlap.
#' @param ... other arguments to pass to \code{\link[ggplot2]{geom_text}} or
#'  \code{\link[ggplot2:geom_text]{geom_label}}.
#' @param na.rm If FALSE (the default), removes missing values with a warning. If
#'  TRUE silently removes missing values.
#' @seealso \code{\link{compare_means}}
#' @details For grouped plots, if one or more subsets do not contain enough levels
#'  to identify the requested comparison, those subsets are skipped and valid
#'  subsets are still tested. This avoids fatal layer failures in sparse grouped
#'  data settings (Issue #663).
#' @examples
#' # Load data
#' data("ToothGrowth")
#' head(ToothGrowth)
#'
#' # Two independent groups
#' # :::::::::::::::::::::::::::::::::::::::::::::::::
#' p <- ggboxplot(ToothGrowth,
#'   x = "supp", y = "len",
#'   color = "supp", palette = "npg", add = "jitter"
#' )
#'
#' #  Add p-value
#' p + stat_compare_means()
#' # Change method
#' p + stat_compare_means(method = "t.test")
#'
#' # Paired samples
#' # :::::::::::::::::::::::::::::::::::::::::::::::::
#' ggpaired(ToothGrowth,
#'   x = "supp", y = "len",
#'   color = "supp", line.color = "gray", line.size = 0.4,
#'   palette = "npg"
#' ) +
#'   stat_compare_means(paired = TRUE)
#'
#' # More than two groups
#' # :::::::::::::::::::::::::::::::::::::::::::::::::
#' # Pairwise comparisons: Specify the comparisons you want
#' my_comparisons <- list(c("0.5", "1"), c("1", "2"), c("0.5", "2"))
#' ggboxplot(ToothGrowth,
#'   x = "dose", y = "len",
#'   color = "dose", palette = "npg"
#' ) +
#'   # Add pairwise comparisons p-value
#'   stat_compare_means(comparisons = my_comparisons, label.y = c(29, 35, 40)) +
#'   stat_compare_means(label.y = 45) # Add global Anova p-value
#'
#' # Multiple pairwise test against a reference group
#' ggboxplot(ToothGrowth,
#'   x = "dose", y = "len",
#'   color = "dose", palette = "npg"
#' ) +
#'   stat_compare_means(method = "anova", label.y = 40) + # Add global p-value
#'   stat_compare_means(aes(label = after_stat(p.signif)),
#'     method = "t.test", ref.group = "0.5"
#'   )
#'
#' # Multiple grouping variables
#' # :::::::::::::::::::::::::::::::::::::::::::::::::
#' # Box plot facetted by "dose"
#' p <- ggboxplot(ToothGrowth,
#'   x = "supp", y = "len",
#'   color = "supp", palette = "npg",
#'   add = "jitter",
#'   facet.by = "dose", short.panel.labs = FALSE
#' )
#' # Use only p.format as label. Remove method name.
#' p + stat_compare_means(
#'   aes(label = paste0("p = ", after_stat(p.format)))
#' )
#'
#' @export
stat_compare_means <- function(mapping = NULL, data = NULL,
                               method = NULL, paired = FALSE, method.args = list(), ref.group = NULL,
                               comparisons = NULL, hide.ns = FALSE, label.sep = ", ",
                               label = NULL, label.x.npc = "left", label.y.npc = "top",
                               label.x = NULL, label.y = NULL, vjust = 0, tip.length = 0.03,
                               bracket.size = 0.3, step.increase = 0,
                               symnum.args = list(),
                               p.format.style = "default", p.digits = NULL,
                               p.leading.zero = NULL, p.min.threshold = NULL,
                               p.decimal.mark = NULL,
                               signif.cutoffs = NULL, signif.symbols = NULL,
                               ns.symbol = "ns", use.four.stars = FALSE, show.signif = TRUE,
                               geom = "text", position = "identity", na.rm = FALSE, show.legend = NA,
                               inherit.aes = TRUE, ...) {
  # Handle show.signif = FALSE with label = "p.format.signif"
  if (!show.signif && identical(label, "p.format.signif")) {
    warning("show.signif = FALSE with label = 'p.format.signif': ",
      "falling back to 'p.format' (p-value only, no significance symbols)",
      call. = FALSE
    )
    label <- "p.format"
  }

  # Build symnum.args from new parameters
  symnum.args <- build_symnum_args(
    signif.cutoffs = signif.cutoffs,
    signif.symbols = signif.symbols,
    ns.symbol = ns.symbol,
    use.four.stars = use.four.stars,
    symnum.args = symnum.args
  )

  if (!is.null(comparisons)) {
    # The `comparisons` path draws brackets via ggsignif::geom_signif(), whose
    # tip length is always a fraction of the data range - ggpubr cannot alter it.
    # `tip.length.ref` (supported by geom_bracket()/stat_pvalue_manual()) is not
    # honored here, so warn once instead of silently ignoring it (#362).
    if ("tip.length.ref" %in% names(list(...))) {
      rlang::inform(
        c(
          paste0(
            "`tip.length.ref` is not supported by `stat_compare_means(comparisons = )`; ",
            "it is ignored here."
          ),
          i = paste0(
            "Bracket tips are drawn by `ggsignif::geom_signif()` and are always a ",
            "fraction of the data range. For tip lengths that stay constant across ",
            "plots, compute the tests with `compare_means()` and draw them with ",
            "`stat_pvalue_manual(..., tip.length.ref = \"axis\")`."
          )
        ),
        .frequency = "once",
        .frequency_id = "ggpubr_stat_compare_means_tip_length_ref"
      )
    }
    # The `comparisons` path draws each pairwise test independently (via
    # ggsignif::geom_signif) and shows UNADJUSTED p-values. Warn once per session
    # when there is more than one comparison, so users aren't misled into thinking
    # the p-values are corrected for multiple testing (#293).
    if (length(comparisons) > 1) {
      rlang::inform(
        c(
          paste0(
            "`stat_compare_means()` with `comparisons` displays *unadjusted* ",
            "p-values (no correction for multiple comparisons)."
          ),
          i = paste0(
            "For p-values adjusted for multiple comparisons, use `geom_pwc()`, ",
            "or `stat_pvalue_manual()` together with ",
            "`compare_means(..., p.adjust.method = )`."
          )
        ),
        .frequency = "once",
        .frequency_id = "ggpubr_stat_compare_means_comparisons_unadjusted"
      )
    }
    method.info <- .method_info(method)
    method <- method.info$method

    method.args <- .add_item(method.args, paired = paired)

    pms <- list(...)
    size <- ifelse(is.null(pms$size), 3.88, pms$size)
    color <- ifelse(is.null(pms$color), "black", pms$color)
    # Forward the font family to the bracket labels; without this it was dropped
    # in the comparisons path while it worked in the default path (#592, #624).
    family <- ifelse(is.null(pms$family), "", pms$family)

    if (is.null(label)) {
      mapped_label <- .get_stat_compare_means_label_from_mapping(mapping)
      label <- if (is.null(mapped_label)) "p.format" else mapped_label
    }
    map_signif_level <- .make_stat_compare_means_map_signif_level(
      label = label,
      symnum.args = symnum.args,
      hide.ns = hide.ns,
      p.format.style = p.format.style,
      p.digits = p.digits,
      p.leading.zero = p.leading.zero,
      p.min.threshold = p.min.threshold,
      p.decimal.mark = p.decimal.mark
    )

    if (missing(step.increase)) {
      step.increase <- ifelse(is.null(label.y), 0.12, 0)
    }
    ggsignif::geom_signif(
      comparisons = comparisons, y_position = label.y,
      test = method, test.args = method.args,
      step_increase = step.increase, size = bracket.size, textsize = size, color = color,
      family = family,
      map_signif_level = map_signif_level, tip_length = tip.length,
      data = data, vjust = vjust
    )
  } else {
    mapping <- .update_mapping(mapping, label)
    layer(
      stat = StatCompareMeans, data = data, mapping = mapping, geom = geom,
      position = position, show.legend = show.legend, inherit.aes = inherit.aes,
      params = list(
        label.x.npc = label.x.npc, label.y.npc = label.y.npc,
        label.x = label.x, label.y = label.y, label.sep = label.sep,
        method = method, method.args = method.args,
        paired = paired, ref.group = ref.group,
        symnum.args = symnum.args,
        p.format.style = p.format.style, p.digits = p.digits,
        p.leading.zero = p.leading.zero, p.min.threshold = p.min.threshold,
        p.decimal.mark = p.decimal.mark,
        hide.ns = hide.ns, na.rm = na.rm, vjust = vjust, ...
      )
    )
  }
}


StatCompareMeans <- ggproto("StatCompareMeans", Stat,
  required_aes = c("x", "y"),
  default_aes = aes(hjust = after_stat(hjust), vjust = after_stat(vjust)),
  compute_panel = function(data, scales, method, method.args,
                           paired, ref.group, symnum.args,
                           p.format.style, p.digits,
                           p.leading.zero, p.min.threshold, p.decimal.mark,
                           hide.ns, label.x.npc, label.y.npc,
                           label.x, label.y, label.sep) {
    . <- x <- NULL
    .is.multiple.grouping.vars <- !all(data$x == data$group)

    if (!is.null(ref.group)) {
      if (ref.group != ".all.") ref.group <- scales$x$map(ref.group)
    }

    # Guess the number of group to be compared
    # ::::::::::::::::::::::::::::::::::::::::::::::::::
    if (.is.multiple.grouping.vars) {
      x.levels <- .levels(data$group)
    } else {
      x.levels <- .levels(data$x)
    }
    two.groups <- length(x.levels) == 2 | !is.null(ref.group)
    multi.groups <- length(x.levels) > 2

    # Guess the test to be performed
    # ::::::::::::::::::::::::::::::::::::::::::::::::::
    if (two.groups & is.null(method)) {
      method <- "wilcox.test"
    } else if (multi.groups & is.null(method)) {
      method <- "kruskal.test"
    }

    # Perform group comparisons
    # ::::::::::::::::::::::::::::::::::::::::::::::::::
    if (!is.null(ref.group)) {
      ref.group <- as.character(ref.group)
    }

    method.args <- method.args %>%
      .add_item(
        data = data, method = method,
        paired = paired, ref.group = ref.group,
        symnum.args = symnum.args,
        p.format.style = p.format.style, p.digits = p.digits,
        p.leading.zero = p.leading.zero, p.min.threshold = p.min.threshold,
        p.decimal.mark = p.decimal.mark
      )

    if (.is.multiple.grouping.vars) {
      method.args <- method.args %>%
        .add_item(formula = y ~ group, group.by = "x")
      .test <- do.call(compare_means, method.args)
    } else {
      method.args <- method.args %>%
        .add_item(formula = y ~ x)
      .test <- do.call(compare_means, method.args)
    }

    if (nrow(.test) == 0) {
      .test$label <- character(0)
      .test$x <- numeric(0)
      .test$y <- numeric(0)
      .test$hjust <- numeric(0)
      .test$vjust <- numeric(0)
      if (!("p.format.signif" %in% names(.test))) {
        .test$p.format.signif <- character(0)
      }
      return(.test)
    }

    # Format p-value for label using the specified style
    p_formatted <- format_p_value(.test$p,
      style = p.format.style,
      digits = p.digits,
      leading.zero = p.leading.zero,
      min.threshold = p.min.threshold,
      decimal.mark = p.decimal.mark
    )
    .test$p.format <- p_formatted
    .test <- add_p_format_signif(.test)
    pvaltxt <- create_p_label(p_formatted)
    .test$label <- paste(.test$method, pvaltxt, sep = label.sep)

    # Options for label positioning
    # ::::::::::::::::::::::::::::::::::::::::::::::::::
    label.opts <- list(
      data = data, scales = scales,
      label.x.npc = label.x.npc, label.y.npc = label.y.npc,
      label.x = label.x, label.y = label.y,
      symnum.args = symnum.args, .by = "panel"
    )

    if (.is.multiple.grouping.vars) {
      if (is.null(label.x) & length(label.x.npc) == 1) {
        label.opts$label.x <- .test$x
      }

      .label.pms <- label.opts %>%
        .add_item(group.ids = .test$x) %>%
        do.call(.label_params_by_group, .) # Returns a data frame with label: x, y, hjust, vjust
      # .test <- dplyr::select(.test, -x)
      .label.pms <- dplyr::select(.label.pms, -x)
    } else {
      .label.pms <- label.opts %>%
        do.call(.label_params, .) %>% # Returns a data frame with label: x, y, hjust, vjust
        dplyr::mutate(hjust = 0.2)
    }
    if (!is.null(ref.group)) {
      group.ids <- as.numeric(.test$group2)
      if (!is.null(label.y) & ref.group != ".all.") {
        if (length(label.y) == length(group.ids)) {
          label.opts$label.y <- c(0, label.y)
        }
      }
      .label.pms <- label.opts %>%
        .add_item(group.ids = group.ids) %>%
        do.call(.label_params_by_group, .)
    }

    res <- cbind(.test, .label.pms)

    if (!is.null(ref.group)) {
      # Set label x value to group names
      other.group.index <- as.numeric(res$group2)
      res$x <- scales$x$range$range[other.group.index]
      res <- res %>% dplyr::mutate(hjust = 0.5)
    }

    if (hide.ns) {
      p.signif <- res$p.signif
      p.format <- res$p.format
      p.signif[p.signif == "ns"] <- " "
      res$p.signif <- p.signif
    }
    if (all(c("p.format", "p.signif") %in% names(res))) {
      res[["p.format.signif"]] <- add_stat_label(res, label = "p.format.signif")$label
    }
    res
  }
)


# Check if p.signif is in mapping
.is_p.signif_in_mapping <- function(mapping) {
  res <- FALSE
  if (!is.null(mapping)) {
    if (!is.null(mapping$label)) {
      .label <- rlang::as_label(mapping$label)
      res <- grepl(pattern = "p\\.signif", .label)
    }
  }
  return(res)
}

.get_stat_compare_means_label_from_mapping <- function(mapping) {
  if (is.null(mapping) || is.null(mapping$label)) {
    return(NULL)
  }
  .label <- rlang::as_label(mapping$label)
  if (grepl("p\\.format\\.signif", .label)) {
    return("p.format.signif")
  }
  if (grepl("p\\.signif", .label)) {
    return("p.signif")
  }
  if (grepl("p\\.format", .label)) {
    return("p.format")
  }
  if (grepl("after_stat\\(p\\)|\\.\\.p\\.\\.", .label)) {
    return("p")
  }
  NULL
}

.make_stat_compare_means_map_signif_level <- function(label, symnum.args, hide.ns,
                                                      p.format.style, p.digits,
                                                      p.leading.zero, p.min.threshold,
                                                      p.decimal.mark) {
  p_to_signif <- function(p) {
    signif_label <- as.character(stats::symnum(
      p,
      cutpoints = symnum.args$cutpoints,
      symbols = symnum.args$symbols
    ))
    if (hide.ns) signif_label[signif_label == "ns"] <- ""
    signif_label
  }
  p_to_format <- function(p) {
    format_p_value(
      p,
      style = p.format.style,
      digits = p.digits,
      leading.zero = p.leading.zero,
      min.threshold = p.min.threshold,
      decimal.mark = p.decimal.mark
    )
  }

  if (label %in% c("p.signif", "..p.signif..")) {
    return(p_to_signif)
  }
  if (label %in% c("p.format", "..p.format..")) {
    return(p_to_format)
  }
  if (label %in% c("p", "..p..")) {
    return(function(p) create_p_label(p_to_format(p)))
  }
  if (label %in% c("p.format.signif", "..p.format.signif..")) {
    return(function(p) create_p_label(p_to_format(p), p_to_signif(p)))
  }
  FALSE
}

# Update mapping with label
.update_mapping <- function(mapping, label) {
  allowed.label <- list(
    "p.signif" = quote(ggplot2::after_stat(p.signif)),
    "..p.signif.." = quote(ggplot2::after_stat(p.signif)),
    "p.format" = quote(ggplot2::after_stat(create_p_label(p.format))),
    "..p.format.." = quote(ggplot2::after_stat(create_p_label(p.format))),
    "p" = quote(ggplot2::after_stat(create_p_label(p.format))),
    "..p.." = quote(ggplot2::after_stat(create_p_label(p.format))),
    "p.format.signif" = quote(ggplot2::after_stat(p.format.signif))
  )

  if (!is.null(label)) {
    if (!label %in% names(allowed.label)) {
      stop("Allowed values for label are: ", .collapse(names(allowed.label), sep = ", "))
    }
  }

  if (!is.null(mapping) & is.character(label)) {
    mapping$label <- allowed.label[[label]]
  } else if (is.character(label)) {
    mapping <- aes()
    mapping$label <- allowed.label[[label]]
  }
  convert_label_dotdot_notation_to_after_stat(mapping)
}

# Hide NS in map_signif_level
.hide_ns <- function(x) {
  n <- names(x)
  ns.pos <- which(n == "ns" | n == "NS")
  if (!.is_empty(ns.pos)) n[ns.pos] <- " "
  names(x) <- n
  x
}
