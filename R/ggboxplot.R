#' @include utilities.R ggpar.R
NULL
#' Box plot
#'
#' @param data a data frame
#' @param x,y x and y variables for drawing.
#' @param color,fill outline and fill colors.
#' @param palette the color palette to be used for coloring or filling by
#'   groups. Allowed values include "grey" for grey color palettes; RColorBrewer
#'   palettes e.g. "RdBu", "Blues", ...; or a character vector containing custom
#'   color palette e.g. c("blue", "red").
#' @param linetype line types.
#' @param size line size.
#' @param width box plot width.
#' @inheritParams ggplot2::geom_boxplot
#' @param select character vector specifying which items to display.
#' @param order character vector specifying the order of items.
#' @param add character vector for adding another plot element (e.g.: dot plot
#'   or error bars). Allowed values are one or the combination of: "none", "dotplot", "jitter",
#'   "mean", "mean_se", "mean_sd", "mean_ci", "mean_range", "median",
#'   "median_iqr", "median_mad", "median_range".
#' @param add.params parameters (color, shape, size, fill, linetype) for the
#'   argument 'add'; e.g.: add.params = list(color = "red").
#' @param ... other arguments to be passed to geom_boxplot including linetype,
#'   size, etc. (See ?geom_boxplot).
#' @details The plot can be easily customized using the function ggpar(). Read
#'   ?ggpar for changing: \itemize{ \item main title and axis labels: main,
#'   xlab, ylab \item axis limits: xlim, ylim (e.g.: ylim = c(0, 30)) \item axis
#'   scales: xscale, yscale (e.g.: yscale = "log2") \item color palettes:
#'   palette = "Dark2" or palette = c("gray", "blue", "red") \item legend title,
#'   labels and position: legend = "right" \item plot orientation : orientation
#'   = c("vertical", "horizontal", "reverse") }
#'
#' @section Suggestions for the argument "add":
#' Suggested values are one of c("dotplot", "jitter").
#'
#' @seealso \code{\link{ggpar}}
#' @examples
#' # Load data
#' data("ToothGrowth")
#' df <- ToothGrowth
#'
#' # Basic plot
#' # +++++++++++++++++++++++++++
#' ggboxplot(df, x = "dose", y = "len")
#'
#' # Change width
#' ggboxplot(df, "dose", "len", width = 0.2)
#'
#' # Change orientation: horizontal
#' ggboxplot(df, "dose", "len", orientation = "horizontal")
#'
#' # Notched box plot
#' ggboxplot(df, x = "dose", y = "len",
#'    notch = TRUE)
#'
#' # Add dot
#' # ++++++++++++++++++++++++++
#' ggboxplot(df, x = "dose", y = "len",
#'    add = "dotplot")
#'
#' # Add jitter
#' ggboxplot(df, x = "dose", y = "len",
#'    add = "jitter")
#'
#' # Change jitter shape by groups: "dose"
#' ggboxplot(df, x = "dose", y = "len",
#'   add = "jitter", shape = "dose")
#'
#'
#' # Select and order items
#' # ++++++++++++++++++++++++++++++
#'
#' # Select which items to display: "0.5" and "2"
#' ggboxplot(df, "dose", "len",
#'    select = c("0.5", "2"))
#'
#' # Change the default order of items
#' ggboxplot(df, "dose", "len",
#'    order = c("2", "1", "0.5"))
#'
#'
#' # Change colors
#' # +++++++++++++++++++++++++++
#' # Change outline and fill colors
#' # Change line size
#'
#'  ggboxplot(df, "dose", "len",
#'    color = "#BFD5E3", fill = "#6D9EC1", size = 1.2)
#'
#' # Change outline colors by groups: dose
#'  ggboxplot(df, "dose", "len", color = "dose")
#'
#' # Change fill color by groups: dose
#'  ggboxplot(df, "dose", "len", fill = "dose")
#'
#' # Use custom color palette
#'  ggboxplot(df, "dose", "len", color = "dose",
#'   palette = c('#999999','#E69F00','#56B4E9') )
#'
#' # Use brewer palette
#' ggboxplot(df, "dose", "len", color = "dose",
#'  palette = "Dark2", add = "jitter" )
#'
#' # Use grey palette
#' ggboxplot(df, "dose", "len", color = "dose",
#' palette = "grey" )
#'
#'
#' # Box plot with multiple groups
#' # +++++++++++++++++++++
#' # fill or color box plot by a second group : "supp"
#' ggboxplot(df, "dose", "len", color = "supp",
#'  palette = c('#999999','#E69F00') )
#'
#' # Add dot
#' ggboxplot(df, "dose", "len", color = "supp",
#'  palette = c('#999999','#E69F00'), add = "dotplot")
#'
#' @export
ggboxplot <- function(data, x, y,
                      color = "black", fill = "white", palette = NULL,
                      linetype = "solid", size = 1, width = 1,  notch = FALSE,
                      select = NULL, order = NULL,
                      add = "none", add.params = list(), ...)
{

  data[, x] <- factor(data[, x])

  p <- ggplot(data, aes_string(x, y)) +
      .geom_exec(geom_boxplot, data = data,
                color = color, fill = fill, linetype = linetype,
                size = size, width = width, notch = notch,
                position = position_dodge(0.8), size = size,...)

  # Add dot
  if(color %in% names(data) & is.null(add.params$color))  add.params$color <- color
  if(fill %in% names(data) & is.null(add.params$fill))  add.params$fill <- fill
  if(is.null(add.params$color)) add.params$color <- color
  if(is.null(add.params$fill)) add.params$fill <- add.params$color
  if(!is.null(list(...)$shape) & is.null(add.params$shape)) add.params$shape <- list(...)$shape
  p <- .add(p, add = add, add.params = add.params)

  # Select and order
  if(is.null(select)) select <- order
  if (!is.null(select) | !is.null(order))
    p <- p + scale_x_discrete(limits = as.character(select))
   p <- ggpar(p, palette = palette, ...)

  p
}


