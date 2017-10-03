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
#' @param labels (optional) list of labels to be added to the plots. You can
#'   also set labels="AUTO" to auto-generate upper-case labels or labels="auto"
#'   to auto-generate lower-case labels.
#' @param widths (optional) numerical vector of relative columns widths. For
#'   example, in a two-column grid, widths = c(2, 1) would make the first column
#'   twice as wide as the second column.
#' @param heights same as \code{widths} but for column heights.
#' @param legend character specifying legend position. Allowed values are one of
#'   c("top", "bottom", "left", "right", "none"). To remove the legend use
#'   legend = "none".
#' @param common.legend logical value. Default is FALSE. If TRUE, a common
#'   unique legend will be created for arranged plots.
#' @param label_size (optional) Numerical value indicating the label size. Default is 14.
#' @param label_fontfamily (optional) Font family of the plot labels. If not provided, is taken from the current theme.
#' @param label_fontface (optional) Font face of the plot labels. Default is "bold".
#' @param label_colour (optional) Color of the plot labels. If not provided, is taken from the current theme.
#' @param label_x (optional) Single value or vector of x positions for plot labels, relative to each subplot.
#'   Defaults to 0 for all labels. (Each label is placed all the way to the left of each plot.)
#' @param label_y (optional) Single value or vector of y positions for plot labels, relative to each subplot.
#'   Defaults to 1 for all labels. (Each label is placed all the way to the top of each plot.)
#' @param hjust Adjusts the horizontal position of each label. More negative values move the label further
#'   to the right on the plot canvas. Can be a single value (applied to all labels) or a vector of values
#'   (one for each label). Default is -0.5.
#' @param vjust Adjusts the vertical position of each label. More positive values move the label further
#'   down on the plot canvas. Can be a single value (applied to all labels) or a vector of values
#'   (one for each label). Default is 1.5.
#' @return return an object of class \code{ggarrange}, which is a ggplot or a
#'   list of ggplot.
#' @author Alboukadel Kassambara \email{alboukadel.kassambara@@gmail.com}
#' @seealso \code{\link{annotate_figure}()}
#' @examples
#' data("ToothGrowth")
#' df <- ToothGrowth
#' df$dose <- as.factor(df$dose)
#'
#' # Create some plots
#' # ::::::::::::::::::::::::::::::::::::::::::::::::::
#' # Box plot
#' bxp <- ggboxplot(df, x = "dose", y = "len",
#'     color = "dose", palette = "jco")
#' # Dot plot
#' dp <- ggdotplot(df, x = "dose", y = "len",
#'     color = "dose", palette = "jco")
#' # Density plot
#' dens <- ggdensity(df, x = "len", fill = "dose", palette = "jco")
#'
#' # Arrange
#' # ::::::::::::::::::::::::::::::::::::::::::::::::::
#' ggarrange(bxp, dp, dens, ncol = 2, nrow = 2)
#' # Use a common legend for multiple plots
#' ggarrange(bxp, dp,  common.legend = TRUE)
#'
#'
#' @export
ggarrange <- function(..., plotlist = NULL, ncol = NULL, nrow = NULL,
                      labels = NULL,
                      align = c("none", "h", "v", "hv"),
                      widths = 1, heights = 1,
                      legend = NULL, common.legend = FALSE,
                      label_size = 14, label_fontfamily = NULL,
                      label_fontface = "bold", label_colour = NULL,
                      label_x = 0, label_y = 1,
                      hjust = -0.5, vjust = 1.5)
  {
  align <- match.arg(align)
  plots <- c(list(...), plotlist)
  nb.plots <- length(plots)
  nb.plots.per.page <- .nbplots_per_page(ncol, nrow)

  if(is.null(legend) & common.legend)
    legend <- "top"
  legend <- .check_legend(legend)
  if(!is.null(legend))
    plots <- purrr::map(plots, function(x) x + theme(legend.position = legend))

  # Split plots over multiple pages
  if(nb.plots > nb.plots.per.page){
    plots <- split(plots, ceiling(seq_along(plots)/nb.plots.per.page))
  }

  # One unique page
  else plots <- list(plots)

  res <- purrr::map(plots, .plot_grid,
                    ncol = ncol, nrow = nrow, labels = labels, align = align,
                    rel_widths = widths, rel_heights = heights,
                    legend = legend, common.legend = common.legend,
                    label_size = label_size, label_fontfamily = label_fontfamily,
                    label_fontface = label_fontface, label_colour = label_colour,
                    label_x = label_x, label_y = label_y,
                    hjust = hjust, vjust = vjust)

  if(length(res) == 1) res <- res[[1]]

  class(res) <- c(class(res), "ggarrange")
  res
}




# Compute number of plots per page
.nbplots_per_page <- function(ncol = NULL, nrow = NULL){

  if(!is.null(ncol) & !is.null(nrow))
    ncol * nrow
  else if(!is.null(ncol))
    ncol
  else if(!is.null(nrow))
    nrow
  else Inf
}


.plot_grid <- function(plotlist, legend = "top", common.legend = FALSE,  ... ){


  if(common.legend){
    # Legend infos
    leg <- get_legend(plotlist[[1]])
    lheight <- sum(leg$height)
    lwidth <- sum(leg$width)
    plotlist <- purrr::map(plotlist, function(x) x + theme(legend.position = "none"))
  }

  res <- cowplot::plot_grid(plotlist = plotlist, ...)
  if(!common.legend) return(res)

  arrangeGrob <- gridExtra::arrangeGrob
  unit.c <- grid::unit.c
  .unit <- grid::unit(1, "npc")

  res <- switch(legend,
                top = arrangeGrob(leg, res, ncol = 1,
                                  heights = unit.c(lheight, .unit - lheight)),
                bottom = arrangeGrob(res, leg, ncol = 1,
                                     heights = unit.c(unit(1, "npc") - lheight, lheight)),
                left = arrangeGrob(leg, res, ncol = 2,
                                   widths = unit.c(lwidth, .unit - lwidth)),
                right = arrangeGrob(res, leg, ncol = 2,
                                    widths = unit.c(.unit - lwidth, lwidth))
                )

  p <- cowplot::ggdraw() + cowplot::draw_grob(grid::grobTree(res))
  p

}
