#' @include utilities.R ggpar.R
NULL
#' Bar plot
#' @description Create a bar plot.
#' @inheritParams ggboxplot
#' @inheritParams ggplot2::geom_bar
#' @param x,y x and y variables for drawing.
#' @param color,fill outline and fill colors.
#' @param sort.val a string specifying whether the value should be sorted.
#' Allowed values are "none" (no sorting), "asc" (for ascending) or "desc" (for descending).
#' @param sort.by.groups logical value. If TRUE the data are sorted by groups.
#' Used only when sort.val != "none".
#' @param top a numeric value specifying the number of top elements to be shown.
#' @param numeric.x.axis logical. If TRUE, x axis will be treated as numeric.
#'   Default is FALSE. Useful, for example, to plot bars at their numeric x
#'   positions (e.g. a time axis) instead of at equally-spaced discrete
#'   categories. Ignored when \code{order} is set or \code{sort.val != "none"},
#'   which require a discrete x axis.
#' @param label specify whether to add labels on the bar plot. Allowed values
#'   are: \itemize{ \item \strong{logical value}: If TRUE, y values are added as
#'   labels on the bar plot \item \strong{character vector}: Used as text
#'   labels; must be the same length as y. }
#' @param lab.col,lab.size text color and size for labels.
#' @param lab.pos character specifying the position for labels. Allowed values
#'   are "out" (for outside) or "in" (for inside). Ignored when lab.vjust !=
#'   NULL.
#' @param lab.vjust numeric, vertical justification of labels. Provide negative
#'   value (e.g.: -0.4) to put labels outside the bars or positive value to put
#'   labels inside (e.g.: 2).
#' @param lab.hjust numeric, horizontal justification of labels.
#' @param lab.nb.digits integer indicating the number of decimal places (round) to be used.
#' @param ... other arguments to be passed to be passed to ggpar().
#' @details The plot can be easily customized using the function ggpar(). Read
#'   ?ggpar for changing: \itemize{ \item main title and axis labels: main,
#'   xlab, ylab \item axis limits: xlim, ylim (e.g.: ylim = c(0, 30)) \item axis
#'   scales: xscale, yscale (e.g.: yscale = "log2") \item color palettes:
#'   palette = "Dark2" or palette = c("gray", "blue", "red") \item legend title,
#'   labels and position: legend = "right" \item plot orientation : orientation
#'   = c("vertical", "horizontal", "reverse") }
#' @seealso \code{\link{ggpar}}, \code{\link{ggline}}
#' @examples
#' # Data
#' df <- data.frame(
#'   dose = c("D0.5", "D1", "D2"),
#'   len = c(4.2, 10, 29.5)
#' )
#' print(df)
#'
#' # Basic plot with label outsite
#' # +++++++++++++++++++++++++++
#' ggbarplot(df,
#'   x = "dose", y = "len",
#'   label = TRUE, label.pos = "out"
#' )
#'
#' # Change width
#' ggbarplot(df, x = "dose", y = "len", width = 0.5)
#'
#' # Change the plot orientation: horizontal
#' ggbarplot(df, "dose", "len", orientation = "horiz")
#'
#' # Change the default order of items
#' ggbarplot(df, "dose", "len",
#'   order = c("D2", "D1", "D0.5")
#' )
#'
#'
#' # Change colors
#' # +++++++++++++++++++++++++++
#'
#' # Change fill and outline color
#' # add labels inside bars
#' ggbarplot(df, "dose", "len",
#'   fill = "steelblue", color = "steelblue",
#'   label = TRUE, lab.pos = "in", lab.col = "white"
#' )
#'
#' # Change colors by groups: dose
#' # Use custom color palette
#' ggbarplot(df, "dose", "len",
#'   color = "dose",
#'   palette = c("#00AFBB", "#E7B800", "#FC4E07")
#' )
#'
#' # Change fill and outline colors by groups
#' ggbarplot(df, "dose", "len",
#'   fill = "dose", color = "dose",
#'   palette = c("#00AFBB", "#E7B800", "#FC4E07")
#' )
#'
#'
#' # Plot with multiple groups
#' # +++++++++++++++++++++
#'
#' # Create some data
#' df2 <- data.frame(
#'   supp = rep(c("VC", "OJ"), each = 3),
#'   dose = rep(c("D0.5", "D1", "D2"), 2),
#'   len = c(6.8, 15, 33, 4.2, 10, 29.5)
#' )
#' print(df2)
#'
#' # Plot "len" by "dose" and change color by a second group: "supp"
#' # Add labels inside bars
#' ggbarplot(df2, "dose", "len",
#'   fill = "supp", color = "supp", palette = "Paired",
#'   label = TRUE, lab.col = "white", lab.pos = "in"
#' )
#'
#' # Change position: Interleaved (dodged) bar plot
#' ggbarplot(df2, "dose", "len",
#'   fill = "supp", color = "supp", palette = "Paired",
#'   label = TRUE,
#'   position = position_dodge(0.9)
#' )
#'
#' # Add points and errors
#' # ++++++++++++++++++++++++++
#'
#' # Data: ToothGrowth data set we'll be used.
#' df3 <- ToothGrowth
#' head(df3, 10)
#'
#' # It can be seen that for each group we have
#' # different values
#' ggbarplot(df3, x = "dose", y = "len")
#'
#' # Visualize the mean of each group
#' ggbarplot(df3,
#'   x = "dose", y = "len",
#'   add = "mean"
#' )
#'
#' # Add error bars: mean_se
#' # (other values include: mean_sd, mean_ci, median_iqr, ....)
#' # Add labels
#' ggbarplot(df3,
#'   x = "dose", y = "len",
#'   add = "mean_se", label = TRUE, lab.vjust = -1.6
#' )
#'
#' # Use only "upper_errorbar"
#' ggbarplot(df3,
#'   x = "dose", y = "len",
#'   add = "mean_se", error.plot = "upper_errorbar"
#' )
#'
#' # Change error.plot to "pointrange"
#' ggbarplot(df3,
#'   x = "dose", y = "len",
#'   add = "mean_se", error.plot = "pointrange"
#' )
#'
#' # Add jitter points and errors (mean_se)
#' ggbarplot(df3,
#'   x = "dose", y = "len",
#'   add = c("mean_se", "jitter")
#' )
#'
#' # Add dot and errors (mean_se)
#' ggbarplot(df3,
#'   x = "dose", y = "len",
#'   add = c("mean_se", "dotplot")
#' )
#'
#' # Multiple groups with error bars and jitter point
#' ggbarplot(df3,
#'   x = "dose", y = "len", color = "supp",
#'   add = "mean_se", palette = c("#00AFBB", "#E7B800"),
#'   position = position_dodge()
#' )
#' #
#'
#' @section Faceting a summarized bar plot:
#' When the bars show a computed summary (e.g. \code{add = "mean_se"}), facet the
#' plot with the \code{facet.by} argument - \strong{not} by appending
#' \code{+ facet_wrap()} / \code{+ facet_grid()}. The summaries are pre-computed,
#' grouping by \code{x}, \code{color}/\code{fill} and \code{facet.by}; a facet added
#' afterwards is not part of that grouping, so the bars (and, for stacked bars, the
#' error bars) are pooled over the whole data set and repeated identically in every
#' panel. Use \code{ggbarplot(..., facet.by = "group")} for correct per-panel
#' summaries.
#'
#' @export
ggbarplot <- function(data, x, y, combine = FALSE, merge = FALSE,
                      color = "black", fill = "white", palette = NULL,
                      size = NULL, width = NULL,
                      title = NULL, xlab = NULL, ylab = NULL,
                      facet.by = NULL, panel.labs = NULL, short.panel.labs = TRUE,
                      select = NULL, remove = NULL, order = NULL,
                      add = "none", add.params = list(), error.plot = "errorbar",
                      label = FALSE, lab.col = "black", lab.size = 4,
                      lab.pos = c("out", "in"), lab.vjust = NULL, lab.hjust = NULL,
                      lab.nb.digits = NULL,
                      sort.val = c("none", "desc", "asc"), sort.by.groups = TRUE,
                      top = Inf,
                      position = position_stack(),
                      numeric.x.axis = FALSE,
                      ggtheme = theme_pubr(),
                      ...) {
  # Default options
  # :::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::
  .opts <- list(
    combine = combine, merge = merge,
    color = color, fill = fill, palette = palette,
    size = size, width = width,
    title = title, xlab = xlab, ylab = ylab,
    facet.by = facet.by, panel.labs = panel.labs, short.panel.labs = short.panel.labs,
    select = select, remove = remove, order = order,
    add = add, add.params = add.params, error.plot = error.plot,
    label = label, lab.col = lab.col, lab.size = lab.size,
    lab.pos = lab.pos, lab.vjust = lab.vjust, lab.hjust = lab.hjust,
    lab.nb.digits = lab.nb.digits,
    sort.val = sort.val, sort.by.groups = sort.by.groups, top = top,
    position = position, numeric.x.axis = numeric.x.axis, ggtheme = ggtheme, ...
  )

  if (!missing(data)) .opts$data <- data
  if (!missing(x)) .opts$x <- x
  if (!missing(y)) .opts$y <- y

  # User options
  # :::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::
  .user.opts <- as.list(match.call(expand.dots = TRUE))
  .user.opts[[1]] <- NULL # Remove the function name
  # keep only user arguments
  for (opt.name in names(.opts)) {
    if (is.null(.user.opts[[opt.name]])) {
      .opts[[opt.name]] <- NULL
    }
  }

  if (is.logical(merge)) {
    if (merge & missing(position)) {
      .opts$position <- position_dodge(0.8)
    }
    if (merge & missing(lab.col)) {
      .opts$lab.col <- ".y."
    }
  } else if (is.character(merge)) {
    .opts$position <- position_dodge(0.8)
  }

  .opts$fun <- ggbarplot_core
  .opts$fun_name <- "barplot"
  if (missing(ggtheme) & (!is.null(facet.by) | combine)) {
    .opts$ggtheme <- theme_pubr(border = TRUE)
  }
  # Honor an explicit `ggtheme = NULL` (skip theming). The NULL-filter loop above
  # drops it like an unset argument, so restore any explicitly passed value here,
  # keeping an explicit NULL intact via single-bracket list assignment (#561).
  if (!missing(ggtheme)) .opts["ggtheme"] <- list(ggtheme)
  p <- do.call(.plotter, .opts)

  if (.is_list(p) & length(p) == 1) p <- p[[1]]
  return(p)
}

ggbarplot_core <- function(data, x, y,
                           color = "black", fill = "white", palette = NULL,
                           size = NULL, width = 0.7,
                           title = NULL, xlab = NULL, ylab = NULL,
                           label = FALSE, lab.col = "black", lab.size = 4,
                           lab.pos = c("out", "in"), lab.vjust = NULL, lab.hjust = NULL,
                           lab.nb.digits = NULL,
                           select = NULL, order = NULL, facet.by = NULL,
                           sort.val = c("none", "desc", "asc"), sort.by.groups = TRUE,
                           merge = FALSE,
                           top = Inf,
                           add = "none",
                           add.params = list(),
                           error.plot = "errorbar",
                           position = position_stack(),
                           numeric.x.axis = FALSE,
                           ggtheme = theme_pubr(),
                           ...) {
  sort.val <- match.arg(sort.val)
  xx <- .select_vec(data, x)
  if (!is.null(order)) {
    data[[x]] <- factor(data[[x]], levels = order)
  } else if (inherits(xx, c("character", "numeric")) & !numeric.x.axis) {
    data[[x]] <- as.factor(data[[x]])
  }
  error.plot <- error.plot[1]
  lab.pos <- match.arg(lab.pos)
  label <- as.vector(label)
  if ("none" %in% add) add <- "none"

  . <- NULL

  # #404: an `alpha` aesthetic mapped to a discrete data column defines an extra
  # dodge subgroup (e.g. fill = cut, alpha = clarity -> 2 bars per cut). Detect it
  # so the summary keeps that column and the error layer dodges by it too. Scoped
  # to plain position_dodge(): the alpha subgroup adds a second dodge dimension,
  # which position_stack()/position_dodge2() do not resolve here (they are left
  # exactly as before, unchanged by this fix).
  alpha.var <- list(...)[["alpha"]]   # [[ ]] avoids $ partial-matching a `...` arg
  has.alpha.group <- !is.null(alpha.var) && length(alpha.var) == 1 &&
    is.character(alpha.var) && alpha.var %in% names(data) &&
    !is.numeric(.select_vec(data, alpha.var)) &&
    inherits(position, "PositionDodge") && !inherits(position, "PositionDodge2")

  grouping.vars <- intersect(c(x, color, fill, facet.by), names(data))
  # Include the alpha subgroup in the summary grouping. Otherwise the summarized
  # data drops the alpha column, which (a) makes geom_exec pass alpha as a static
  # value -> the "alpha * 255" draw error (#404), and (b) collapses the mean/CI
  # across the subgroups. Left unchanged when no discrete alpha var is mapped.
  if (has.alpha.group) grouping.vars <- unique(c(grouping.vars, alpha.var))

  # static summaries for computing mean/median and adding errors
  if (is.null(add.params$fill)) add.params$fill <- "white"
  if (is.null(add.params$group)) {
    if (fill %in% names(data)) {
      add.params$group <- fill
    } else if (color %in% names(data)) add.params$group <- color
  }
  # #404: the bars dodge by the interaction of fill/x and the alpha subgroup, so
  # the error layer must dodge by that same interaction to stay aligned; otherwise
  # the error bars are centered on each x while the bars are split (misaligned). We
  # materialise the interaction as a real column with a safe name (rather than an
  # "interaction(a, b)" mapping string), so it is robust to special characters in
  # the variable names and to options(ggpubr.parse_aes = FALSE).
  if (has.alpha.group) {
    base.group <- add.params$group %||% x
    data[[".ggpubr.alpha.group."]] <- interaction(
      .select_vec(data, base.group), .select_vec(data, alpha.var), drop = TRUE
    )
    add.params$group <- ".ggpubr.alpha.group."
  }
  add.params <- .check_add.params(add, add.params, error.plot, data, color, fill, ...)

  if (any(.summary_functions() %in% add)) {
    data_sum <- desc_statby(data, measure.var = y, grps = grouping.vars)
    summary.funcs <- intersect(.summary_functions(), add)
    if (length(summary.funcs) > 1) {
      stop(
        "Only one summary function is allowed. ",
        "Choose one of ", .collapse(.summary_functions(), sep = ", ")
      )
    }
    .center <- .get_errorbar_center_func(summary.funcs)

    add <- setdiff(add, .center)
    names(data_sum)[which(names(data_sum) == .center)] <- y
    if (inherits(xx, c("character", "numeric")) & !numeric.x.axis) {
      data_sum[, x] <- .select_vec(data_sum, x) %>% as.factor()
    }
  } else {
    data_sum <- data
  }

  # Sorting
  # %%%%%%%%%%%%%%%%%%%%%%%%%%%%%
  if (top != Inf & sort.val == "none") sort.val <- "desc"
  if (top != Inf) {
    data_sum <- data_sum[order(-data_sum[, y]), ]
    data_sum <- utils::head(data_sum, n = top)
  }
  grps <- unique(intersect(c(color, fill), names(data)))
  if (length(grps) > 0) grps <- .get_not_numeric_vars(data[, grps, drop = FALSE])
  ngrps <- length(grps)
  if (!sort.by.groups) ngrps <- 0
  # Variables for ordering
  if (ngrps > 0) {
    dd <- data_sum[, c(grps, y)]
  } else {
    dd <- data_sum[, y, drop = FALSE]
  }
  if (sort.val == "desc") dd[, y] <- -dd[, y]
  # Sorting
  if (sort.val != "none") {
    if (ngrps == 0) {
      data_sum <- data_sum[order(dd[, y]), ]
    } else if (ngrps == 1) {
      data_sum <- data_sum[order(dd[, 1], dd[, y]), ]
    } else if (ngrps == 2) data_sum <- data_sum[order(dd[, 1], dd[, 2], dd[, y]), ]
    data_sum[, x] <- factor(data_sum[, x], levels = unique(data_sum[, x]))
  }

  # Main plot
  # %%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
  if (inherits(position, "PositionDodge") & is.null(position$width)) position$width <- 0.95
  p <- ggplot(data, create_aes(list(x = x, y = y)))
  p <- p +
    geom_exec(geom_bar,
      data = data_sum,
      stat = "identity",
      color = color, fill = fill,
      position = position,
      size = size, width = width, ...
    )

  # Add errors
  add.params <- add.params %>% .add_item(p = p, error.plot = error.plot)
  is.stacked.position <- inherits(position, "PositionStack")
  stack.groups <- unique(c(x, facet.by))
  nb.bars.by.xposition <- data_sum %>%
    group_by(!!!syms(stack.groups)) %>%
    dplyr::count() %>%
    dplyr::pull(.data$n) %>%
    max()
  if (is.stacked.position) {
    add.position <- "identity"
  } else {
    add.position <- position
  }
  if (is.stacked.position & nb.bars.by.xposition >= 2) {
    p <- add.params %>%
      .add_item(add = .remove_errorbar_func(add), position = add.position) %>%
      do.call(ggadd, .)
    if (any(.errorbar_functions() %in% add)) {
      p <- p + .geom_stacked_errorbar(
        data_sum, x, y,
        color = add.params$color, fill = add.params$fill,
        group = add.params$group, facet.by = facet.by,
        func = .get_summary_func(add), error.plot = error.plot
      )
    }
  } else if (inherits(position, "PositionDodge2") &&
             nb.bars.by.xposition >= 2 &&
             any(.errorbar_functions() %in% add) &&
             error.plot %in% .narrow_error_plots()) {
    # position_dodge2() misplaces thin error bars relative to the dodged bars
    # (#363). Draw any non-error add layers normally, then re-center the error
    # bars on the actual bar positions.
    p <- add.params %>%
      .add_item(add = .remove_errorbar_func(add), position = add.position) %>%
      do.call(ggadd, .)
    cap.width <- (add.params$width %||% 0.1) / nb.bars.by.xposition
    eb <- .geom_dodge2_errorbar(
      p, data_sum, x, y,
      color = add.params$color, fill = add.params$fill,
      group = add.params$group, facet.by = facet.by,
      func = .get_summary_func(add), error.plot = error.plot,
      width = cap.width
    )
    if (!is.null(eb)) {
      p <- p + eb
    } else {
      # Could not match centres -> keep the standard (unaligned) error layer
      p <- add.params %>%
        .add_item(add = .get_summary_func(add), position = add.position) %>%
        do.call(ggadd, .)
    }
  } else {
    p <- add.params %>%
      .add_item(add = add, position = add.position) %>%
      do.call(ggadd, .)
  }


  # Add labels
  add.label <- FALSE
  if (is.logical(label)) {
    .lab <- y
    add.label <- label
  } else {
    # Add user specified labels as data column
    data_sum$.ulabel. <- label
    .lab <- ".ulabel."
    add.label <- TRUE
  }

  if (add.label) {
    if (is.null(lab.vjust)) lab.vjust <- ifelse(lab.pos == "out", -0.4, 2)
    if (is.null(lab.hjust)) lab.hjust <- 0.5
    if (!is.null(lab.nb.digits)) {
      if (is.numeric(.lab)) {
        .lab <- round(.lab, digits = lab.nb.digits)
      } else if (.lab[1] %in% colnames(data_sum)) {
        data_sum[, .lab] <- dplyr::pull(data_sum, .lab) %>%
          round(digits = lab.nb.digits)
      }
    }

    # pos <- "identity"
    # if color or fill by groups
    .cols <- unique(c(color, fill))
    if (any(.cols %in% names(data))) {
      .in <- which(.cols %in% names(data))
      lab.fill <- color.var <- .cols[.in]
      data_sum <- data_sum %>%
        dplyr::arrange(!!!syms(x), desc(!!!syms(color.var)))

      group <- intersect(.cols, names(data))[1] # You should specify group for dodging text

      p <- p + geom_exec(geom_text,
        data = data_sum, label = .lab, # fill = lab.fill
        vjust = lab.vjust, hjust = lab.hjust, size = lab.size, color = lab.col,
        fontface = "plain", position = position, group = group
      )
    } else {
      p <- p + geom_exec(geom_text,
        data = data_sum, label = .lab,
        vjust = lab.vjust, hjust = lab.hjust, size = lab.size, color = lab.col,
        fontface = "plain", position = position
      )
    }
  }
  # To do
  # top10, visualizing error
  p <- ggpar(p,
    palette = palette, ggtheme = ggtheme,
    title = title, xlab = xlab, ylab = ylab, ...
  )

  p
}

# Stacked error bar ----------------------------
.geom_stacked_errorbar <- function(data_sum, x, y, color = NULL, fill = NULL, facet.by = NULL, group = NULL,
                                   func = "mean_se", error.plot = "errorbar") {
  stack.groups <- unique(c(x, facet.by))
  legend.var <- intersect(unique(c(color, fill, group)), colnames(data_sum))
  error <- .get_errorbar_error_func(func)
  error.value <- data_sum %>% dplyr::pull(!!error)
  desc <- dplyr::desc
  errorbar.position <- data_sum %>%
    group_by(!!!syms(stack.groups)) %>%
    dplyr::arrange(!!sym(x), desc(!!sym(legend.var))) %>%
    dplyr::mutate(
      # position_stack() accumulates positive and negative segments SEPARATELY
      # (positives stack up from 0, negatives stack down from 0), so a mixed-sign
      # stack must cumulate each sign on its own. A single cumsum() over both signs
      # places the error bars of one sign on the wrong side (#426). For single-sign
      # data one of the two cumulative sums is identically zero, so this reduces to
      # the original cumsum() and the output is byte-identical.
      .ggpubr_cum_pos = cumsum(pmax(!!sym(y), 0)),
      .ggpubr_cum_neg = cumsum(pmin(!!sym(y), 0)),
      y = ifelse(!!sym(y) >= 0, .data$.ggpubr_cum_pos, .data$.ggpubr_cum_neg),
      ymin = .data$y - !!sym(error),
      ymax = .data$y + !!sym(error)
    ) %>%
    dplyr::select(-".ggpubr_cum_pos", -".ggpubr_cum_neg") %>%
    dplyr::ungroup()
  geom_error <- .get_geom_error_function(error.plot)

  args <- geom_exec(
    data = errorbar.position, color = color,
    group = group,
    x = x, ymin = "ymin", ymax = "ymax"
  )
  mapping <- args$mapping
  option <- args$option
  if (error.plot == "errorbar") option$width <- 0.15
  option[["mapping"]] <- create_aes(mapping)
  do.call(geom_error, option)
}


.get_geom_error_function <- function(error.plot = "errorbar") {
  error.plot <- error.plot[1]
  geom_func <- ggplot2::geom_errorbar
  if (error.plot %in% c("pointrange", "lower_pointrange", "upper_pointrange")) {
    geom_func <- ggplot2::geom_pointrange
  } else if (error.plot %in% c("linerange", "lower_linerange", "upper_linerange")) {
    geom_func <- ggplot2::geom_linerange
  } else if (error.plot %in% c("errorbar", "lower_errorbar", "upper_errorbar")) {
    geom_func <- ggplot2::geom_errorbar
  }
  geom_func
}

.is_stacked <- function(p) {
  inherits(p$layers[[1]]$position, "PositionStack")
}

# Error plots that draw as a thin element (vertical line/cap or a point) and so
# do NOT self-align with the wide bars under position_dodge2(). These are the
# ones that need the manual re-centering done by .geom_dodge2_errorbar() (#363).
.narrow_error_plots <- function() {
  c(
    "errorbar", "lower_errorbar", "upper_errorbar",
    "pointrange", "lower_pointrange", "upper_pointrange",
    "linerange", "lower_linerange", "upper_linerange"
  )
}

# Aligned error bars for position_dodge2() bars (#363).
# position_dodge2() packs elements according to their own width, so a thin error
# bar is not placed on the centre of the wide bar it belongs to: bars and error
# bars end up at different x (the reported bug). Here we read the actual dodged
# bar centres from the built plot and redraw the error layer at those x with
# position = "identity", so each error bar sits exactly on its bar while keeping
# a normal thin cap. Only used for position_dodge2(); every other position keeps
# the standard ggadd() path unchanged. Returns NULL (caller falls back) if the
# centres cannot be matched.
.geom_dodge2_errorbar <- function(p, data_sum, x, y, color = NULL, fill = NULL,
                                  group = NULL, facet.by = NULL, func = "mean_se",
                                  error.plot = "errorbar", width = 0.1) {
  legend.var <- intersect(unique(c(color, fill, group)), colnames(data_sum))
  if (length(legend.var) == 0) return(NULL)
  legend.var <- legend.var[1]

  # Error limits (centre +/- error), honouring upper_/lower_/both, from data_sum.
  # Only symmetric summaries whose error half-width is a data_sum column are
  # handled here (mean_se/sd/ci/range, median_iqr/mad/range). Asymmetric quantile
  # summaries (median_hilow(_)/median_q1q3) have no such column and are NOT a
  # centre +/- error, so we bail out (return NULL) and let the caller keep the
  # standard path, which draws their correct (if unaligned) interval.
  error <- .get_errorbar_error_func(func)
  if (is.null(error) || !(error %in% colnames(data_sum))) return(NULL)
  err.val <- data_sum[[error]]
  limit <- unlist(strsplit(error.plot, "_", fixed = TRUE))[1]
  if (!(limit %in% c("upper", "lower"))) limit <- "both"
  yc <- data_sum[[y]]
  ds <- data_sum
  ds$.yc. <- yc
  ds$.ymin. <- if (limit %in% c("both", "lower")) yc - err.val else yc
  ds$.ymax. <- if (limit %in% c("both", "upper")) yc + err.val else yc

  # Actual dodged bar centres from the built plot. Faceting is applied later (by
  # .plotter), so build against a temporarily-faceted copy: dodge2 positions
  # depend on which groups are present *within each panel*.
  build.p <- p
  if (!is.null(facet.by)) build.p <- p + ggplot2::facet_wrap(facet.by)
  built <- ggplot2::ggplot_build(build.p)
  bar.layer <- which(vapply(p$layers, function(l) inherits(l$geom, "GeomBar"), logical(1)))
  if (length(bar.layer) == 0) return(NULL)
  bd <- built$data[[bar.layer[1]]]
  bd <- bd[order(bd$PANEL, bd$x), , drop = FALSE]

  # Map each summary row to its panel, then to a bar centre. Bars are laid out
  # per panel left-to-right by x tick then group; sorting data_sum the same way
  # aligns it 1:1 with the ordered bar centres (robust to input row order).
  layout <- built$layout$layout
  if (!is.null(facet.by) && all(facet.by %in% colnames(layout))) {
    ds <- dplyr::left_join(ds, layout[, c("PANEL", facet.by), drop = FALSE], by = facet.by)
  } else {
    ds$PANEL <- factor(1L)
  }
  as.int <- function(v) if (is.factor(v)) as.integer(v) else as.integer(factor(v))
  ds <- ds[order(as.int(ds$PANEL), as.int(ds[[x]]), as.int(ds[[legend.var]])), , drop = FALSE]
  if (nrow(ds) != nrow(bd)) return(NULL)
  ds$.ebx. <- bd$x

  geom.error <- .get_geom_error_function(error.plot)
  color.is.var <- !is.null(color) && length(color) == 1 && color %in% colnames(ds)
  if (color.is.var) {
    mapping <- ggplot2::aes(
      x = .data$.ebx., ymin = .data$.ymin., ymax = .data$.ymax.,
      colour = .data[[color]]
    )
  } else {
    mapping <- ggplot2::aes(x = .data$.ebx., ymin = .data$.ymin., ymax = .data$.ymax.)
  }
  if (identical(geom.error, ggplot2::geom_pointrange)) mapping$y <- ggplot2::aes(y = .data$.yc.)$y
  opts <- list(data = ds, mapping = mapping, inherit.aes = FALSE, position = "identity")
  if (!color.is.var && !is.null(color) && length(color) == 1) opts$colour <- color
  if (identical(geom.error, ggplot2::geom_errorbar)) opts$width <- width
  do.call(geom.error, opts)
}

# remove "mean_se", "mean_sd", etc
.remove_errorbar_func <- function(add) {
  setdiff(add, .errorbar_functions())
}
# return "mean_se"
.get_summary_func <- function(add) {
  intersect(.errorbar_functions(), add)
}


# Returns: mean or median
.get_errorbar_center_func <- function(func = "mean_se") {
  . <- NULL
  func %>%
    strsplit("_", fixed = TRUE) %>%
    unlist() %>%
    .[1]
}

# Returns se, sd, iqr
.get_errorbar_error_func <- function(func = "mean_se") {
  res <- func %>%
    strsplit("_", fixed = TRUE) %>%
    unlist()
  if (length(res) >= 2) {
    res <- res[2]
  } else {
    res <- NULL
  }
  res
}
