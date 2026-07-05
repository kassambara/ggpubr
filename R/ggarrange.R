#' @include utilities.R
NULL
#' Arrange Multiple ggplots
#'
#' @description Arrange multiple ggplots on the same page. Wrapper around
#'   \code{\link[cowplot]{plot_grid}()}. Can arrange multiple ggplots over
#'   multiple pages, compared to the standard
#'   \code{\link[cowplot]{plot_grid}()}. Can also create a common unique legend
#'   for multiple plots.
#' @inheritParams cowplot::plot_grid
#' @param ... list of plots to be arranged into the grid. The plots can be
#'   either ggplot2 plot objects or arbitrary gtables.
#' @param plotlist (optional) list of plots to display.
#' @param ncol (optional) number of columns in the plot grid.
#' @param nrow (optional) number of rows in the plot grid.
#' @param byrow logical. If \code{TRUE} (default), the plots are filled into the
#'   grid by row; set to \code{FALSE} to fill by column. Passed to
#'   \code{\link[cowplot]{plot_grid}()}.
#' @param labels (optional) list of labels to be added to the plots. You can
#'   also set labels="AUTO" to auto-generate upper-case labels or labels="auto"
#'   to auto-generate lower-case labels.
#' @param font.label a list of arguments for customizing labels. Allowed values
#'   are the combination of the following elements: size (e.g.: 14), face (e.g.:
#'   "plain", "bold", "italic", "bold.italic"), color (e.g.: "red") and family.
#'   For example font.label = list(size = 14, face = "bold", color ="red").
#' @param label.x (optional) Single value or vector of x positions for plot
#'   labels, relative to each subplot. Defaults to 0 for all labels. (Each label
#'   is placed all the way to the left of each plot.)
#' @param label.y (optional) Single value or vector of y positions for plot
#'   labels, relative to each subplot. Defaults to 1 for all labels. (Each label
#'   is placed all the way to the top of each plot.)
#' @param widths (optional) numerical vector of relative columns widths. For
#'   example, in a two-column grid, widths = c(2, 1) would make the first column
#'   twice as wide as the second column.
#' @param heights same as \code{widths} but for column heights.
#' @param legend character specifying legend position. Allowed values are one of
#'   c("top", "bottom", "left", "right", "none"). To remove the legend use
#'   legend = "none".
#' @param common.legend logical value, or one or several plot indices. Default is FALSE. If
#'   \code{TRUE}, a single shared legend is used for all the arranged plots.
#'   Note that this legend is \strong{not} merged or validated across plots: it
#'   is simply the legend of the \emph{first} plot, and the other legends are
#'   dropped. It is therefore only correct when every plot shares the same scale
#'   (same groups/levels, order and color range). If the first plot's legend is
#'   not representative - for example a group is missing in the first plot, or a
#'   continuous color scale spans a different range - the shared legend will
#'   misrepresent the other plots. In that case you can: (i) give the plots a
#'   consistent scale yourself (e.g. \code{scale_fill_manual(limits = ...)} or
#'   \code{scale_color_continuous(limits = ...)}) so a single legend is valid,
#'   and/or (ii) choose which plot's legend is shown by passing that plot's index,
#'   e.g. \code{common.legend = 2} to use the second plot's legend (equivalent to
#'   \code{legend.grob = get_legend(plots[[2]])}). You can also pass several indices,
#'   e.g. \code{common.legend = c(1, 2)}, to keep and combine the legends of those
#'   plots into a single shared block (side by side for \code{legend = "top"}/
#'   \code{"bottom"}, stacked for \code{"left"}/\code{"right"}) - useful when the
#'   plots genuinely need different legends. Note that (ii) only changes
#'   \emph{which} legend is displayed; it does not re-map the other plots' color
#'   scales, so for the legend keys to match every panel you still need a
#'   consistent scale as in (i). When the plots genuinely cannot be described by a
#'   single legend (e.g. a discrete fill in one plot and a continuous color bar in
#'   another), use \code{common.legend = FALSE} to keep a separate legend per plot.
#' @param legend.grob a legend grob as returned by the function
#'   \code{\link{get_legend}()}. If provided, it will be used as the common
#'   legend.
#' @param spacing numeric value giving the margin, in text-line units, set
#'   uniformly around each plot to increase the gap between the arranged plots.
#'   Default is 0, which leaves each plot's own margins untouched (existing
#'   arrangements are unchanged). A positive value sets a uniform margin of that
#'   many lines around every plot, replacing the plots' default margin; e.g.
#'   \code{spacing = 1} puts a one-line margin around each plot.
#' @return an object of class \code{ggarrange}, which is a ggplot or a
#'   list of ggplots.
#' @author Laszlo Erdey \email{erdey.laszlo@@econ.unideb.hu}
#' @seealso \code{\link{annotate_figure}()}
#' @examples
#' data("ToothGrowth")
#' df <- ToothGrowth
#' df$dose <- as.factor(df$dose)
#'
#' # Create some plots
#' # ::::::::::::::::::::::::::::::::::::::::::::::::::
#' # Box plot
#' bxp <- ggboxplot(df,
#'   x = "dose", y = "len",
#'   color = "dose", palette = "jco"
#' )
#' # Dot plot
#' dp <- ggdotplot(df,
#'   x = "dose", y = "len",
#'   color = "dose", palette = "jco"
#' )
#' # Density plot
#' dens <- ggdensity(df, x = "len", fill = "dose", palette = "jco")
#'
#' # Arrange
#' # ::::::::::::::::::::::::::::::::::::::::::::::::::
#' ggarrange(bxp, dp, dens, ncol = 2, nrow = 2)
#' # Use a common legend for multiple plots
#' ggarrange(bxp, dp, common.legend = TRUE)
#'
#' @export
ggarrange <- function(..., plotlist = NULL, ncol = NULL, nrow = NULL,
                      labels = NULL, label.x = 0, label.y = 1, hjust = -0.5, vjust = 1.5,
                      font.label = list(size = 14, color = "black", face = "bold", family = NULL),
                      align = c("none", "h", "v", "hv"),
                      widths = 1, heights = 1, byrow = TRUE,
                      legend = NULL, common.legend = FALSE, legend.grob = NULL,
                      spacing = 0) {
  # Open null device to avoid blank page before plot------
  # see cowplot:::as_grob.ggplot
  null_device <- base::getOption("ggpubr.null_device", default = cowplot::pdf_null_device)
  cur_dev <- grDevices::dev.cur()
  # Open null device to avoid blank page before plot
  null_device(width = 6, height = 6)
  null_dev <- grDevices::dev.cur()
  on.exit({
    grDevices::dev.off(null_dev)
    if (cur_dev > 1) grDevices::dev.set(cur_dev)
  })


  plots <- c(list(...), plotlist)
  plots <- .add_plot_spacing(plots, spacing)
  align <- match.arg(align)
  nb.plots <- length(plots)
  page.layout <- .get_layout(ncol, nrow, nb.plots)
  ncol <- page.layout$ncol
  nrow <- page.layout$nrow
  nb.plots.per.page <- .nbplots_per_page(ncol, nrow)

  # #347: `common.legend` may also be one or several plot indices, selecting which
  # plot(s) supply the shared legend (useful when the first plot's legend is not
  # representative, e.g. a group is missing in the first plot, or when the plots
  # genuinely need several different legends). A single index uses that plot's
  # legend; several indices combine those plots' legends into one block. Logical
  # TRUE/FALSE behave exactly as before: is.numeric(TRUE) is FALSE, so this branch
  # is skipped for them and the default output is unchanged.
  legend.plot.index <- NULL
  if (is.numeric(common.legend)) {
    idx <- common.legend
    if (length(idx) < 1L || anyNA(idx) || !all(is.finite(idx)) ||
        !all(idx == as.integer(idx)) || any(idx < 1) || any(idx > nb.plots)) {
      stop("When numeric, `common.legend` must be whole-number plot indices between ",
           "1 and the number of plots (", nb.plots, ").", call. = FALSE)
    }
    idx <- as.integer(idx)
    if (anyDuplicated(idx)) {
      stop("`common.legend` plot indices must be unique.", call. = FALSE)
    }
    if (any(vapply(plots[idx], is.null, logical(1)))) {
      stop("`common.legend` points to an empty (NULL) plot.", call. = FALSE)
    }
    legend.plot.index <- idx
    common.legend <- TRUE
  }

  if (!is.null(legend.grob)) {
    common.legend <- TRUE
  }
  if (is.null(legend) & common.legend) {
    legend <- "top"
  }
  legend <- .check_legend(legend)
  if (!is.null(legend)) {
    plots <- purrr::map(
      plots,
      function(x) {
        if (!is.null(x)) x + theme(legend.position = legend) else x
      }
    )
  }

  if (common.legend) {
    if (is.null(legend.grob)) {
      legend.grob <- if (!is.null(legend.plot.index)) {
        .combine_legends(plots, legend.plot.index, legend)
      } else {
        get_legend(plots)
      }
    }
    plots <- purrr::map(
      plots,
      function(x) {
        if (!is.null(x)) x + theme(legend.position = "none") else x
      }
    )
  }

  # Split plots over multiple pages
  if (nb.plots > nb.plots.per.page) {
    plots <- split(plots, ceiling(seq_along(plots) / nb.plots.per.page))
  }
  # One unique page
  else {
    plots <- list(plots)
  }

  # label arguments
  .lab <- .update_label_pms(font.label,
    label.x = label.x, label.y = label.y,
    hjust = hjust, vjust = vjust
  )

  res <- purrr::map(plots, .plot_grid,
    ncol = ncol, nrow = nrow, labels = labels,
    label_size = .lab$size, label_fontfamily = .lab$family,
    label_fontface = .lab$face, label_colour = .lab$color,
    label_x = .lab$label.x, label_y = .lab$label.y,
    hjust = .lab$hjust, vjust = .lab$vjust, align = align,
    rel_widths = widths, rel_heights = heights, byrow = byrow,
    legend = legend, common.legend.grob = legend.grob
  )


  if (length(res) == 1) res <- res[[1]]

  class(res) <- c(class(res), "ggarrange")
  res
}


.get_layout <- function(ncol, nrow, nb.plots) {
  if (!is.null(ncol) & !is.null(nrow)) {} else if (!is.null(ncol)) {
    if (ncol == 1) nrow <- nb.plots
  } else if (!is.null(nrow)) {
    if (nrow == 1) ncol <- nb.plots
  }
  list(ncol = ncol, nrow = nrow)
}

# Compute number of plots per page
.nbplots_per_page <- function(ncol = NULL, nrow = NULL) {
  if (!is.null(ncol) & !is.null(nrow)) {
    ncol * nrow
  } else if (!is.null(ncol)) {
    ncol
  } else if (!is.null(nrow)) {
    nrow
  } else {
    Inf
  }
}


# Increase the space between arranged plots by adding a uniform margin (in
# text-line units) around each ggplot. spacing = 0 (default) leaves the plots
# untouched, so the arrangement is unchanged (#151). Non-ggplot entries (NULL
# spacers, grobs) are returned as-is.
.add_plot_spacing <- function(plots, spacing = 0) {
  if (is.null(spacing)) {
    return(plots)
  }
  if (!is.numeric(spacing) || anyNA(spacing)) {
    stop("`spacing` must be a single non-missing numeric value.", call. = FALSE)
  }
  if (all(spacing == 0)) {
    return(plots)
  }
  margin <- grid::unit(rep(spacing[1], 4), "lines")
  purrr::map(plots, function(x) {
    if (inherits(x, "ggplot")) x + ggplot2::theme(plot.margin = margin) else x
  })
}

# #347: build the shared legend for a numeric `common.legend`. For a single index
# this is simply that plot's legend (identical to the scalar path). For several
# indices the selected plots' legends are combined into one grob: side by side for a
# horizontal legend strip (legend = "top"/"bottom") and stacked for a vertical one
# ("left"/"right"). gtable's cbind/rbind (dispatched via the base generics) keep the
# result a gtable with the same $height/$width interface a single legend has, so
# `.plot_grid` needs no change and the single-legend / TRUE / FALSE paths stay
# byte-identical. Selected plots whose legend is empty are skipped; if none remain
# the result is NULL (no shared legend, no error). The plots passed here already have
# `legend.position` set to `legend`, so the extracted legends have the right
# orientation for the strip.
.combine_legends <- function(plots, index, legend = "top") {
  legs <- lapply(index, function(i) get_legend(plots[[i]]))
  legs <- Filter(function(g) !is.null(g) && !inherits(g, "zeroGrob"), legs)
  if (length(legs) == 0) {
    return(NULL)
  }
  if (length(legs) == 1) {
    return(legs[[1]])
  }
  horizontal <- isTRUE(legend %in% c("top", "bottom"))
  combine2 <- if (horizontal) {
    function(a, b) cbind(a, b, size = "max")
  } else {
    function(a, b) rbind(a, b, size = "max")
  }
  Reduce(combine2, legs)
}

.plot_grid <- function(plotlist, legend = "top", common.legend.grob = NULL, ...) {
  res <- cowplot::plot_grid(plotlist = plotlist, ...)
  if (is.null(common.legend.grob)) {
    return(res)
  } else {
    leg <- common.legend.grob
    lheight <- sum(leg$height)
    lwidth <- sum(leg$width)
  }

  arrangeGrob <- gridExtra::arrangeGrob
  unit.c <- grid::unit.c
  .unit <- grid::unit(1, "npc")

  res <- switch(legend,
    top = arrangeGrob(leg, res,
      ncol = 1,
      heights = unit.c(lheight, .unit - lheight)
    ),
    bottom = arrangeGrob(res, leg,
      ncol = 1,
      heights = unit.c(unit(1, "npc") - lheight, lheight)
    ),
    left = arrangeGrob(leg, res,
      ncol = 2,
      widths = unit.c(lwidth, .unit - lwidth)
    ),
    right = arrangeGrob(res, leg,
      ncol = 2,
      widths = unit.c(.unit - lwidth, lwidth)
    )
  )

  p <- cowplot::ggdraw() + cowplot::draw_grob(grid::grobTree(res))
  p
}

# update label parameters for cowplot::plot_grid()
.update_label_pms <- function(font.label,
                              label.x = 0, label.y = 1, hjust = -0.5, vjust = 1.5) {
  .font <- list(size = 14, color = "black", face = "bold", family = NULL)
  new.font.names <- names(font.label)
  for (i in new.font.names) .font[[i]] <- font.label[[i]]

  pms <- .font
  list(
    size = pms$size,
    family = pms$family,
    face = pms$face,
    color = pms$color,
    label.x = label.x, label.y = label.y,
    hjust = hjust, vjust = vjust
  )
}
