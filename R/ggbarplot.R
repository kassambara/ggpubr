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
  # so the summary keeps that column and the error layer dodges by it too. Both
  # plain position_dodge() and position_dodge2() are covered; every other
  # position keeps its released behaviour, including its draw errors.
  alpha.var <- list(...)[["alpha"]]   # [[ ]] avoids $ partial-matching a `...` arg
  alpha.is.col <- !is.null(alpha.var) && length(alpha.var) == 1 &&
    is.character(alpha.var) && alpha.var %in% names(data) &&
    !is.numeric(.select_vec(data, alpha.var))
  # Carrying the alpha column into the summary is what makes the subgroup drawable
  # at all: without it the summarised frame loses the column, geom_exec can no
  # longer map it, and the column NAME reaches grid as a static opacity - the
  # "alpha * 255" draw error of #404. But carrying it also SPLITS the summary into
  # one row per (x, legend, alpha) cell, and only the interaction dodge key built
  # below can place that many rows on the right bars.
  #
  # position_dodge2() re-centres its error layer on the bars itself (#363) by
  # matching summary rows to bars by SORT POSITION. With the alpha column carried
  # the released key (PANEL, x, legend) is no longer total - the alpha subgroup is
  # only a stable tie - so anything that reorders the summary (`sort.val`, `top`,
  # `sort.by.groups`) or that resolves a different key (`add.params$color` naming
  # another column) permutes the match. It is handed the FULL discrete key below
  # and then CHECKS the pairing it produced against the bars it is about to draw
  # on, so a key that does not describe the layout falls back to the standard
  # layer rather than drawing one cell's interval on another cell's bar.
  # PositionDodge2 subclasses PositionDodge, so the one test covers both.
  has.alpha.group <- alpha.is.col && inherits(position, "PositionDodge")

  grouping.vars <- intersect(c(x, color, fill, facet.by), names(data))

  # static summaries for computing mean/median and adding errors
  if (is.null(add.params$fill)) add.params$fill <- "white"
  if (is.null(add.params$group)) {
    if (fill %in% names(data)) {
      add.params$group <- fill
    } else if (color %in% names(data)) add.params$group <- color
  }
  # #404: with a discrete `alpha` the bars are split into more groups than the
  # error layer knows about, so the error bars are centred on each x while the
  # bars are dodged apart. The error layer has to dodge by the SAME key ggplot2
  # groups the bars by. We materialise it as a real column with a safe name
  # (rather than an "interaction(a, b)" mapping string), so it survives special
  # characters in the variable names and options(ggpubr.parse_aes = FALSE).
  # Mirrors how add.label is resolved further down: a non-logical `label` is a
  # column of user labels and is always drawn.
  draws.labels <- if (is.logical(label)) isTRUE(label[1]) else TRUE
  # Anything in `add` that is not the summary itself draws the RAW observations
  # (jitter, point, dotplot, boxplot, violin). Those layers are placed by ggadd()
  # under the same position, and position_dodge2() packs by each element's own
  # width - a point has none - so they do not take the bar's slot.
  draws.raw.layers <- length(setdiff(
    add, c(.summary_functions(), .errorbar_functions(), "none")
  )) > 0
  alpha.order.vars <- NULL
  if (has.alpha.group) {
    base.group <- add.params$group %||% x
    # Key on EVERY mapped discrete aesthetic, in the order ggplot2 lays them out
    # in the layer data - `colour` before `fill` - not on a pair chosen here.
    # ggplot2's add_group() calls id() over the layer's discrete columns in that
    # order, first column slowest. Keying on (base.group, alpha) alone is
    # fill-slowest, so as soon as `color` also names a column the two orderings
    # are transposed and half the error bars are drawn on a neighbour's bar
    # carrying ITS mean and ITS error - which released ggpubr got right.
    #
    # lex.order = TRUE is load-bearing: interaction() otherwise varies the FIRST
    # factor fastest, the opposite of id().
    #
    # addNA(): interaction() returns NA for a row whose key column is NA, so that
    # row gets no dodge rank, while id_var(drop = TRUE) sorts na.last = TRUE and
    # keeps NA as a real trailing level. Without it the orderings diverge from the
    # NA cell onward. A missing value in a grouping column is ordinary data.
    #
    # base.group only belongs in the key when it is itself one of the bar's mapped
    # aesthetics. add.params$group defaults to the fill or colour column, but a
    # user may point it at a column mapped to nothing; that column does not split
    # the bars, so keying on it would split the error layer finer than the bars
    # and leave every interval off-centre.
    key.vars <- intersect(c(color, fill), names(data))
    if (!is.null(base.group) && base.group %in% c(key.vars, x, alpha.var)) {
      key.vars <- c(key.vars, base.group)
    }
    key.vars <- unique(c(key.vars, alpha.var))
    key.vars <- intersect(key.vars, names(data))
    # ggplot2 ids the bars over the layer's DISCRETE columns only - its
    # is_discrete() is factor/character/logical. If EVERY column we would key on
    # is discrete, the bars and this key describe the same partition and the
    # error bars can be placed exactly. If any of them is not (a numeric, integer
    # or Date column mapped to colour/fill/alpha), ggplot2 does not group the bars
    # by it while desc_statby() still splits the summary on it, so the layer draws
    # more rects than there are dodge slots and two bars share a slot: there is no
    # one-to-one bar-to-row mapping left for any key to hit. Rather than trade one
    # wrong arrangement for another, keep the released key untouched there.
    key.discrete <- vapply(key.vars, function(k) {
      v <- .select_vec(data, k)
      is.factor(v) || is.character(v) || is.logical(v)
    }, logical(1))
    # Same degeneracy by a different route: desc_statby() names its own output
    # columns after the statistics it computes, so any column it groups by that
    # shares one of those names is REPLACED in the summary by the computed
    # numeric statistic. geom_exec() then resolves the bar layer's aesthetic
    # against that statistic, ggplot2 sees a continuous column and does not group
    # the bars by it, and again no key can match one error bar to one bar.
    # Released behaviour stands. The test covers grouping.vars too, not just the
    # key: a `facet.by` column named after a statistic is destroyed by the same
    # collision even though it never enters the key.
    stat.cols <- c(
      "length", "min", "max", "median", "mean", "iqr", "mad", "sd", "se",
      "ci", "range", "cv", "var"
    )
    key.exact <- all(key.discrete) && !any(c(key.vars, grouping.vars) %in% stat.cols)
    # Under position_dodge2() the alpha column is carried ONLY when that key is
    # exact. In the degenerate cases the key describes a partition the bars are
    # not drawn on, so no ordering can place one interval per bar - and unlike
    # plain dodge, dodge2 has no correctly-valued layout to fall back to: it
    # would draw an interval next to a bar that is not the one it was computed
    # from. Released behaviour (which refuses to draw at all) is the honest
    # outcome there, so leave that path exactly as it was.
    if (inherits(position, "PositionDodge2") && !key.exact) has.alpha.group <- FALSE
    # Same reasoning for `label`. The value labels are placed by their own layer,
    # which dodges on the legend key alone, so with the alpha subgroup carried
    # they land between the bars - measured 4 of 8 over the bar whose value they
    # show. Aligning the error bars while half the numbers float between bars
    # would make the figure look trustworthy and read wrong, so a labelled call
    # keeps the released behaviour until the label layer is keyed too.
    if (inherits(position, "PositionDodge2") && draws.labels) has.alpha.group <- FALSE
    # And for a raw-data layer. Under position_dodge2() those points already sit
    # off their own bar without any alpha (8 of 12 - pre-existing, and unchanged
    # here); splitting the bars finer makes it 12 of 24, i.e. half the
    # observations drawn over a bar they are not from. That is the same
    # misleading figure the labels would give, so the same disposition: this
    # combination keeps the released path until dodge2's raw layers are placed
    # on the bars the way its error layer now is.
    if (inherits(position, "PositionDodge2") && draws.raw.layers) {
      has.alpha.group <- FALSE
    }
    # And for an ASYMMETRIC summary. The re-centred layer is built from the
    # summary's own half-width column, so median_q1q3 / median_hilow - which are
    # quantile pairs, not centre +/- error - have no such column and the helper
    # cannot place them. Their intervals are correct but unpaired, and with the
    # subgroup carried "unpaired" means drawn inside another cell's bar.
    if (inherits(position, "PositionDodge2") && any(.errorbar_functions() %in% add)) {
      err.col <- .get_errorbar_error_func(.get_summary_func(add))
      if (is.null(err.col) ||
          !err.col %in% c("se", "sd", "ci", "range", "iqr", "mad")) {
        has.alpha.group <- FALSE
      }
    }
  }
  # Include the alpha subgroup in the summary grouping. Otherwise the summarized
  # data drops the alpha column, which (a) makes geom_exec pass alpha as a static
  # value -> the "alpha * 255" draw error (#404), and (b) collapses the mean/CI
  # across the subgroups. Left unchanged when no discrete alpha var is mapped.
  if (has.alpha.group) {
    grouping.vars <- unique(c(grouping.vars, alpha.var))
    data[[".ggpubr.alpha.group."]] <- if (key.exact) {
      do.call(
        interaction,
        c(
          lapply(key.vars, function(k) {
            addNA(factor(.select_vec(data, k)), ifany = TRUE)
          }),
          list(drop = TRUE, lex.order = TRUE)
        )
      )
    } else {
      interaction(
        .select_vec(data, base.group), .select_vec(data, alpha.var), drop = TRUE
      )
    }
    add.params$group <- ".ggpubr.alpha.group."
    # The same columns, in the same order, are what position_dodge2()'s error
    # layer must sort on: collide2() lays each x's elements out by ascending
    # group id, and that id is id() over exactly these columns. Only offered when
    # the key is exact - in the degenerate cases above no key describes the
    # layout, so the helper is left on its released rank match.
    if (key.exact) alpha.order.vars <- key.vars
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
             (error.plot %in% .narrow_error_plots() ||
              (!is.null(alpha.order.vars) && error.plot == "crossbar"))) {
    # position_dodge2() misplaces thin error bars relative to the dodged bars
    # (#363). Draw any non-error add layers normally, then re-center the error
    # bars on the actual bar positions.
    #
    # A crossbar is wide, so it is normally left to dodge itself - but dodge2
    # packs elements by their OWN width, and the crossbar's width is not the
    # bar's, so the two only appear to agree while there are few enough
    # subgroups for the offset to stay inside the bar. Carrying an `alpha`
    # column doubles the subgroups and the offset then lands the crossbar on a
    # neighbouring bar, correctly valued and wrongly placed. On that path it is
    # re-centred like the others; without the alpha column nothing changes.
    p <- add.params %>%
      .add_item(add = .remove_errorbar_func(add), position = add.position) %>%
      do.call(ggadd, .)
    cap.width <- (add.params$width %||% 0.1) / nb.bars.by.xposition
    eb <- .geom_dodge2_errorbar(
      p, data_sum, x, y,
      color = add.params$color, fill = add.params$fill,
      group = add.params$group, facet.by = facet.by,
      func = .get_summary_func(add), error.plot = error.plot,
      width = cap.width, order.vars = alpha.order.vars,
      facet.scales = .facet_scales_from_dots(list(...)),
      # ggplot2's collide2() branches on `if (reverse)`, which is TRUE for 1 or
      # "TRUE" as well; isTRUE() alone would read those as FALSE and sort the
      # summary against mirrored bars.
      reverse = isTRUE(as.logical(position$reverse)[1]), bar.fill = fill
    )
    if (!is.null(eb)) {
      p <- p + eb
    } else if (is.null(alpha.order.vars)) {
      # Could not match centres -> keep the standard (unaligned) error layer.
      # Released behaviour, and safe here: without the alpha subgroup there are
      # two elements per x and each interval still lands on its own bar.
      p <- add.params %>%
        .add_item(add = .get_summary_func(add), position = add.position) %>%
        do.call(ggadd, .)
    } else {
      # On the alpha path that same fallback is NOT safe. position_dodge2()
      # packs by each element's own width, so the thin intervals collapse into a
      # cluster at the tick centre: with the subgroup carried they land INSIDE
      # neighbouring bars, and an interval drawn inside a bar is read as that
      # bar's. Master refuses to draw these calls at all, so there is no working
      # output to preserve - draw the bars without an error layer and say so,
      # rather than publish a figure whose intervals belong to other bars.
      warning(
        "Could not align the error bars with the dodged bars, so they were not ",
        "drawn. This combination of `alpha`, `position_dodge2()` and the ",
        "requested summary is not supported; use `position_dodge()` instead.",
        call. = FALSE
      )
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

# The `scales` a faceted plot will actually be drawn with. It reaches facet()
# through `...`, so R matches it PARTIALLY against facet()'s formals: `scale =`
# and `scal =` all arrive as `scales`, while `s =` is ambiguous with
# `short.panel.labs`/`strip.position` and reaches nothing. Testing names with
# `==` would miss every abbreviation - the class of bug that has bitten this
# package repeatedly. pmatch(duplicates.ok = TRUE) resolves each name the way R
# itself will; without it one exact name consumes the formal and every other
# abbreviation of it returns NA.
.facet_scales_from_dots <- function(dots) {
  nms <- names(dots)
  if (is.null(nms) || !length(nms)) return("fixed")
  target <- names(formals(facet))
  # R matches an EXACT name first, and an abbreviation of a formal that is
  # already matched exactly is then left over and swallowed by `...`. So with
  # both `scales = "free_x"` and `scale = "fixed"`, facet() uses "free_x" and
  # ignores the other - taking the last partial hit read "fixed" and probed a
  # layout the plot never draws, putting the intervals on the wrong bars.
  exact <- which(nms == "scales")
  hit <- if (length(exact)) {
    exact
  } else {
    m <- pmatch(nms, target, duplicates.ok = TRUE)
    which(!is.na(m) & target[m] == "scales")
  }
  if (!length(hit)) return("fixed")
  val <- dots[[hit[length(hit)]]]
  if (is.character(val) && length(val) == 1L &&
      val %in% c("fixed", "free", "free_x", "free_y")) {
    val
  } else {
    "fixed"
  }
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
                                  error.plot = "errorbar", width = 0.1,
                                  order.vars = NULL, facet.scales = "fixed",
                                  reverse = FALSE, bar.fill = NULL) {
  legend.var <- intersect(unique(c(color, fill, group)), colnames(data_sum))
  # order.vars is the full set of discrete columns ggplot2 groups the bars by
  # (#404). It is supplied only when an `alpha` column has split the summary
  # finer than the legend variable, and it then replaces the legend-only sort
  # key; without it every step below is exactly the released path.
  order.vars <- intersect(order.vars, colnames(data_sum))
  if (length(legend.var) == 0 && length(order.vars) == 0) return(NULL)
  legend.var <- if (length(legend.var)) legend.var[1] else NULL

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
  #
  # The probe must be faceted the way the FINAL plot will be. With a free x
  # scale a panel that is missing an x level renumbers its remaining levels, so
  # a fixed-scale probe reads centres the drawn panel never uses; with two
  # facet variables facet() uses facet_grid(), not facet_wrap(). Released calls
  # keep the original fixed facet_wrap() probe (its imperfection there is
  # pre-existing behaviour, not something to change under this fix), so the
  # accurate probe is used only on the alpha path.
  build.p <- p
  if (!is.null(facet.by)) {
    build.p <- if (length(order.vars) && length(facet.by) == 2) {
      p + ggplot2::facet_grid(
        stats::as.formula(paste(glue::backtick(facet.by), collapse = " ~ ")),
        scales = facet.scales
      )
    } else if (length(order.vars)) {
      p + ggplot2::facet_wrap(
        stats::as.formula(paste0("~", glue::backtick(facet.by))),
        scales = facet.scales
      )
    } else {
      p + ggplot2::facet_wrap(facet.by)
    }
  }
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
  # Rank one key column the way collide2() lays the elements out: by ASCENDING
  # group id, or by DESCENDING group id under position_dodge2(reverse = TRUE),
  # which sorts on -group. Ordering ascending against reversed bars matches
  # every row to the MIRROR bar, so each interval is drawn on its neighbour
  # carrying that neighbour's mean and error (#783).
  #
  # NA is given the trailing rank explicitly rather than left to order(): that
  # is the trailing level interaction(addNA(...)) keeps and the na.last sort
  # ggplot2's id_var(drop = TRUE) does, and unlike order()'s na.last it mirrors
  # with everything else when the layout is reversed.
  # Negating is only right for a column ggplot2 actually GROUPS the bars by.
  # collide2() reverses via -group, and `group` is id() over the DISCRETE
  # aesthetics only (its is_discrete() is factor/character/logical). Map a
  # continuous column to colour or fill and every bar in an x shares one group
  # id, so the -group sort is a stable tie and the layout is NOT reversed -
  # negating there would mirror the summary against bars that never moved and
  # put every interval on its neighbour, which is the very defect #783 fixes.
  rank.key <- function(v) {
    discrete <- is.factor(v) || is.character(v) || is.logical(v)
    v <- as.int(v)
    # The all-NA arm keeps max() from returning -Inf with a warning. No input
    # was found that reaches it - desc_statby() drops an all-missing grouping
    # column before the key is built, checked through colour, fill, alpha and
    # add.params$group in both directions - so it is defensive only and no test
    # covers it. Left in because it costs nothing; do not read it as tested.
    if (anyNA(v)) v[is.na(v)] <- if (all(is.na(v))) 1L else max(v, na.rm = TRUE) + 1L
    if (isTRUE(reverse) && discrete) -v else v
  }
  ord.keys <- if (length(order.vars)) {
    c(list(as.int(ds$PANEL), as.int(ds[[x]])),
      lapply(order.vars, function(k) rank.key(ds[[k]])))
  } else {
    list(as.int(ds$PANEL), as.int(ds[[x]]), rank.key(ds[[legend.var]]))
  }
  ds <- ds[do.call(order, ord.keys), , drop = FALSE]
  if (nrow(ds) != nrow(bd)) return(NULL)

  # Check the pairing instead of trusting the sort. geom_bar(stat = "identity")
  # draws each summary row's own centre, so the bar a row has been matched to
  # must carry that row's centre; if it does not, the two orders disagree and we
  # are one line away from drawing one cell's interval on another cell's bar.
  # Only the alpha path is checked - the released key is left byte-identical,
  # including in the degenerate cases where it is imperfect.
  #
  # There is deliberately NO extra guard for two cells that share a centre within
  # one (panel, x). An earlier revision refused those, reasoning that a swap
  # between them would move a different half-width onto each bar and the centre
  # could not reveal it. That was backwards: the order above is built from the
  # same discrete key ggplot2 groups the bars by, so it is correct whether or not
  # the centres happen to tie - a tie defeats the VERIFICATION, never the
  # ordering. Refusing on it only sent ordinary rounded or count data down the
  # unpaired path, which is the one outcome worth avoiding.
  if (length(order.vars) &&
      !isTRUE(all.equal(as.numeric(bd$y), as.numeric(ds$.yc.),
                        tolerance = 1e-9, check.attributes = FALSE))) {
    return(NULL)
  }
  ds$.ebx. <- bd$x
  # Each bar's own drawn width, so a crossbar can be given the width of the bar
  # it belongs to instead of dodging itself to a different one.
  ds$.ebw. <- as.numeric(bd$xmax) - as.numeric(bd$xmin)

  # .get_geom_error_function() has no "crossbar" case and falls back to
  # geom_errorbar, which would silently draw the wrong geom. It is shared with
  # the stacked path, so resolve crossbar here rather than there and leave every
  # other caller on exactly the function it resolves today.
  is.crossbar <- identical(error.plot, "crossbar")
  geom.error <- if (is.crossbar) ggplot2::geom_crossbar else
    .get_geom_error_function(error.plot)
  color.is.var <- !is.null(color) && length(color) == 1 && color %in% colnames(ds)
  if (color.is.var) {
    mapping <- ggplot2::aes(
      x = .data$.ebx., ymin = .data$.ymin., ymax = .data$.ymax.,
      colour = .data[[color]]
    )
  } else {
    mapping <- ggplot2::aes(x = .data$.ebx., ymin = .data$.ymin., ymax = .data$.ymax.)
  }
  if (identical(geom.error, ggplot2::geom_pointrange) || is.crossbar) {
    mapping$y <- ggplot2::aes(y = .data$.yc.)$y
  }
  if (is.crossbar) mapping$width <- ggplot2::aes(width = .data$.ebw.)$width
  opts <- list(data = ds, mapping = mapping, inherit.aes = FALSE, position = "identity")
  # color.is.var tests membership in the SUMMARY, so a real data column that is
  # not one of the grouping variables falls through here and is handed to the
  # geom as a literal colour ("Unknown colour name: z"). That is released
  # behaviour for the thin error plots, which have always reached this helper,
  # so it is left alone. The CROSSBAR had not: it dodged itself, and such a call
  # drew (silently ignoring the argument). Routing it through here would have
  # turned that into a crash, so on that path the static colour is set only when
  # it really is one.
  if (!color.is.var && !is.null(color) && length(color) == 1 &&
      (!is.crossbar || .is_color(color))) {
    opts$colour <- color
  }
  if (is.crossbar) {
    # A crossbar is filled, and every other ggpubr crossbar takes its fill from
    # the MAPPED fill aesthetic (ggadd() never forwards add.params$fill to
    # add_summary(), so the mapping wins). ggbarplot_core() defaults
    # add.params$fill to "white" before .check_add.params() can set it, so
    # reading that here silently turned a released, working call's crossbars
    # from the group colours to white - and a white cap truncates the coloured
    # bar at centre - error, which reads as a lower bar. Follow the bar's own
    # fill, and only fall back to add.params$fill when it is not a column.
    crossbar.fill <- if (!is.null(bar.fill) && length(bar.fill) == 1 &&
                         bar.fill %in% colnames(ds)) {
      bar.fill
    } else if (!is.null(fill) && length(fill) == 1) {
      fill
    } else {
      NULL
    }
    if (!is.null(crossbar.fill)) {
      if (crossbar.fill %in% colnames(ds)) {
        mapping$fill <- ggplot2::aes(fill = .data[[crossbar.fill]])$fill
        opts$mapping <- mapping
      } else {
        opts$fill <- crossbar.fill
      }
    }
  }
  if (identical(geom.error, ggplot2::geom_errorbar)) opts$width <- width
  if (!is.crossbar) return(do.call(geom.error, opts))
  # `width` is not in GeomCrossbar$aesthetics(), so layer() warns "Ignoring
  # unknown aesthetics: width" - but GeomErrorbar$setup_data(), which
  # GeomCrossbar delegates to, reads `data$width` before falling back to
  # resolution(x) * 0.9, so the mapping IS honoured and the warning is simply
  # wrong. It has to be a mapping rather than a parameter, because each crossbar
  # takes the width of its own bar and those differ across x groups. Muffle that
  # one message only - every other condition, including ggplot2's own advice
  # about a discrete alpha, still reaches the user.
  withCallingHandlers(
    do.call(geom.error, opts),
    warning = function(w) {
      if (grepl("unknown aesthetics.*width", conditionMessage(w))) {
        invokeRestart("muffleWarning")
      }
    }
  )
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
