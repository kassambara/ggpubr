#' @include utilities.R ggpar.R
NULL
#' Violin plot
#'
#' @inheritParams ggboxplot
#' @param x,y x and y variables for drawing.
#' @param color,fill outline and fill colors.
#' @param linetype line types.
#' @param width violin width.
#' @param add character vector for adding another plot element. Allowed
#'   values are one of c("none", "dotplot", "jitter).
#' @param add.params parameters (color, shape, size, fill, jitter) for the
#'   argument 'add'; e.g.: add.params = list(color = "red").
#' @inheritParams ggplot2::geom_violin
#' @param ... other arguments to be passed to geom_violin.
#' @details The plot can be easily customized using the function ggpar(). Read
#'   ?ggpar for changing: \itemize{ \item main title and axis labels: main,
#'   xlab, ylab \item axis limits: xlim, ylim (e.g.: ylim = c(0, 30)) \item axis
#'   scales: xscale, yscale (e.g.: yscale = "log2") \item color palettes:
#'   palette = "Dark2" or palette = c("gray", "blue", "red") \item legend title,
#'   labels and position: legend = "right" \item plot orientation : orientation
#'   = c("vertical", "horizontal", "reverse") }
#' @seealso \code{\link{ggpar}}
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
#' # Add pointrange
#' ggviolin(df, x = "dose", y = "len",
#'  add = "pointrange")
#'
#' # Add crossbar
#' ggviolin(df, x = "dose", y = "len",
#'  add = "crossbar")
#'
#' # Combine jitter + pointrange
#' ggviolin(df, x = "dose", y = "len",
#'  add = c("jitter", "pointrange"))
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
#'    color = "#2E9FDF", fill = "#6D9EC1", size = 1.2)
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
#' @export
ggviolin <- function(data, x, y,
                      color = "black", fill = "white", palette = NULL,
                      linetype = "solid", trim = FALSE, size = 1, width = 1,
                      select = NULL, order = NULL,
                      add = c("none", "boxplot", "dotplot", "jitter", "pointrange", "crossbar"),
                      add.params = list(), ...)
{

  data[, x] <- factor(data[, x])
  # add <- match.arg(add)

  p <- ggplot(data, aes_string(x, y)) +
      .geom_exec(geom_violin, data = data,
                color = color, fill = fill, linetype = linetype,
                trim = trim, size = size, width = width,
                position = position_dodge(0.8), ...)

  # Add dot
  if(color %in% names(data) & is.null(add.params$color))  add.params$color <- color
  if(fill %in% names(data) & is.null(add.params$fill))  add.params$fill <- fill
  if(is.null(add.params$color)) add.params$color <- color
  if(is.null(add.params$fill) & any(c("boxplot", "crossbar") %in% add)) add.params$fill <- fill
  else add.params$fill <- add.params$color
  if(!is.null(list(...)$shape) & is.null(add.params$shape)) add.params$shape <- list(...)$shape

  add.params$width = 0.2
  p <- .add(p, add = add, add.params = add.params)

  # Select and order
  if(is.null(select)) select <- order
  if (!is.null(select) | !is.null(order))
    p <- p + scale_x_discrete(limits = as.character(select))
   p <- ggpar(p, palette = palette, ...)

  p
}


