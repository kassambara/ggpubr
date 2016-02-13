#' @include utilities.R ggpar.R
NULL
#'Violin plot
#'@description Create a violin plot with error bars. Violin plots are similar to
#'box plots, except that they also show the kernel probability density of the
#'data at different values.
#'@inheritParams ggboxplot
#'@param x,y x and y variables for drawing.
#'@param color,fill outline and fill colors.
#'@param linetype line types.
#'@param width violin width.
#'@inheritParams ggplot2::geom_violin
#'@param ... other arguments to be passed to geom_violin.
#'@details The plot can be easily customized using the function ggpar(). Read
#'  ?ggpar for changing: \itemize{ \item main title and axis labels: main, xlab,
#'  ylab \item axis limits: xlim, ylim (e.g.: ylim = c(0, 30)) \item axis
#'  scales: xscale, yscale (e.g.: yscale = "log2") \item color palettes: palette
#'  = "Dark2" or palette = c("gray", "blue", "red") \item legend title, labels
#'  and position: legend = "right" \item plot orientation : orientation =
#'  c("vertical", "horizontal", "reverse") }
#'@seealso \code{\link{ggpar}}
#' @examples
#' # Load data
#' data("ToothGrowth")
#' df <- ToothGrowth
#'
#' # Basic plot
#' # +++++++++++++++++++++++++++
#'ggviolin(df, x = "dose", y = "len")
#' # Change width
#'ggviolin(df, "dose", "len", width = 0.2)
#' # Change the plot orientation: horizontal
#'ggviolin(df, "dose", "len", orientation = "horiz")
#'
#' # Add summary statistics
#' # ++++++++++++++++++++++++++
#' # Draw quantiles
#' ggviolin(df, "dose", "len", add = "none",
#'    draw_quantiles = 0.5)
#'
#' # Add box plot
#' ggviolin(df, x = "dose", y = "len",
#'  add = "boxplot")
#'
#'ggviolin(df, x = "dose", y = "len",
#'  add = "dotplot")
#'
#' # Add jitter
#'ggviolin(df, x = "dose", y = "len",
#' add = "jitter")
#'
#'# Change shape by groups: "dose"
#'ggviolin(df, x = "dose", y = "len",
#' add = "jitter", shape = "dose")
#'
#' # Add mean_sd
#' ggviolin(df, x = "dose", y = "len",
#'  add = "mean_sd")
#'
#' # Change error.plot to "crossbar"
#' ggviolin(df, x = "dose", y = "len",
#'  add = "mean_sd", error.plot = "crossbar")
#'
#' # Combine jitter + mean_sd
#' ggviolin(df, x = "dose", y = "len",
#'  add = c("jitter", "mean_sd"))
#'
#'
#'
#' # Select and order items
#' # ++++++++++++++++++++++++++++++
#' # Select which items to display: "0.5" and "2"
#'ggviolin(df, "dose", "len",
#'    select = c("0.5", "2"))
#' # Change the default order of items
#'ggviolin(df, "dose", "len",
#'    order = c("2", "1", "0.5"))
#'
#'
#' # Change colors
#' # +++++++++++++++++++++++++++
#' # Change outline and fill colors
#' # Change line size
#' ggviolin(df, "dose", "len",
#'    color = "black", fill = "#6D9EC1", size = 1.2)
#'
#' # Change outline colors by groups: dose
#' ggviolin(df, "dose", "len", color = "dose")
#'
#' # Change fill color by groups: dose
#' ggviolin(df, "dose", "len", fill = "dose")
#'
#' # Use custom color palette
#' ggviolin(df, "dose", "len", color = "dose",
#'   palette = c('#999999','#E69F00','#56B4E9') )
#'
#' # Use brewer palette
#'ggviolin(df, "dose", "len", color = "dose",
#'  palette = "Dark2", add = "boxplot" )
#'
#' # Use grey palette
#'ggviolin(df, "dose", "len", color = "dose",
#' palette = "grey" )
#'
#'
#' # Plot with multiple groups
#' # +++++++++++++++++++++
#' # fill or color box plot by a second group : "supp"
#'ggviolin(df, "dose", "len", color = "supp",
#'  palette = c('#999999','#E69F00') )
#'
#' # Add dot
#'ggviolin(df, "dose", "len", color = "supp",
#'  palette = c('#999999','#E69F00'), add = "dotplot")
#'
#'@export
ggviolin <- function(data, x, y,
                      color = "black", fill = "white", palette = NULL,
                      linetype = "solid", trim = FALSE, size = 1, width = 1,
                      draw_quantiles = NULL,
                      select = NULL, order = NULL,
                      add = "mean_se", add.params = list(),
                      error.plot = "pointrange",
                      ggtheme = theme_pubr(),
                     ...)
{

  data[, x] <- factor(data[, x])
  pms <- .violin_params(...)

  p <- ggplot(data, aes_string(x, y)) +
      .geom_exec(geom_violin, data = data,
                color = color, fill = fill, linetype = linetype,
                trim = trim, size = size, width = width,
                position = position_dodge(0.8), draw_quantiles = draw_quantiles,
                stat = pms$stat, scale = pms$scale)

  # Add
  #+++++++++++++++++++
  add.params <- .check_add.params(add, add.params, error.plot, data, color, fill, ...)
  add.params$width = 0.2
  p <- .add(p, add = add, add.params = add.params, error.plot = error.plot)

  # Select and order
  if(is.null(select)) select <- order
  if (!is.null(select) | !is.null(order))
    p <- p + scale_x_discrete(limits = as.character(select))
   p <- ggpar(p, palette = palette, ggtheme = ggtheme, ...)

  p
}


