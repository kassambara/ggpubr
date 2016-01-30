#' @include utilities.R ggpar.R
NULL
#' Stripcharts
#' @inheritParams ggboxplot
#' @param x,y x and y variables for drawing.
#' @param color,fill outline and fill colors.
#' @param shape point shape
#' @param position position adjustment, either as a string, or the result of a
#'   call to a position adjustment function. Used to adjust position
#'   for multiple groups.
#' @param ... other arguments to be passed to geom_jitter.
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
#' # Basic box plot
#' # +++++++++++++++++++++++++++
#' ggstripchart(df, x = "dose", y = "len")
#' # Change point shapes and size
#' ggstripchart(df, "dose", "len", shape = 17, size = 2)
#'
#'
#' # Select and order items
#' # ++++++++++++++++++++++++++++++
#' # Select which items to display: "0.5" and "2"
#' ggstripchart(df, "dose", "len",
#'    select = c("0.5", "2"))
#' # Change the default order of items
#' ggstripchart(df, "dose", "len",
#'    order = c("2", "1", "0.5"))
#'
#' # Add summary statistics
#' # ++++++++++++++++++++++++++
#' # Add box plot
#' ggstripchart(df, x = "dose", y = "len",
#' add = "boxplot")
#' # Add violin
#' ggstripchart(df, x = "dose", y = "len",
#' add = "violin")
#' # Add pointrange
#' ggstripchart(df, x = "dose", y = "len",
#' add = "pointrange")
#' # Add crossbar
#' ggstripchart(df, x = "dose", y = "len",
#' add = "crossbar", add.params = list(width = 0.5))
#' # Add violin + pointrange
#' ggstripchart(df, x = "dose", y = "len", color = "dose",
#' add = c("violin", "pointrange"))
#'
#'
#'
#' # Change colors
#' # +++++++++++++++++++++++++++
#' # Change colors
#'  ggstripchart(df, "dose", "len", color = "#2E9FDF",
#'   add = "pointrange", add.params = list(color = "red"))
#'
#' # Change colors by groups: dose
#'  ggstripchart(df, "dose", "len", color = "dose",
#'    add = "boxplot")
#'
#' # Use custom color palette
#'  ggstripchart(df, "dose", "len", color = "dose",
#'   palette = c('#999999','#E69F00','#56B4E9') )
#'
#' # Use brewer palette
#' ggstripchart(df, "dose", "len", color = "dose",
#'  palette = "Dark2")
#'
#' # Use grey palette
#' ggstripchart(df, "dose", "len", color = "dose",
#' palette = "grey" )
#'
#'
#' # Plot with multiple groups
#' # +++++++++++++++++++++
#' # Change shape and color by a second group : "supp"
#' ggstripchart(df, "dose", "len", color = "supp",
#'   shape = "supp" )
#'
#' # Add boxplot
#' ggstripchart(df, "dose", "len", color = "supp", shape = "supp",
#'  add = "boxplot", add.params = list(color = "black") )
#'
#' # Change point position
#' ggstripchart(df, "dose", "len", color = "supp",
#'   shape = "supp", add = "boxplot", position = position_dodge(0.8) )
#'
#'
#' @export
ggstripchart <- function(data, x, y,
                      color = "black", fill = "white", palette = NULL,
                      shape = 19,
                      select = NULL, order = NULL,
                      add = c("none", "boxplot", "violin", "pointrange", "crossbar"),
                      add.params = list(),
                      position = position_jitter(0.4),
                      ...)
{

  data[, x] <- factor(data[, x])
  p <- ggplot(data, aes_string(x, y))
  if("none" %in% add) add <- "none"

  # if Add = c("boxplot", "violin"), we first plot add then stripchart
  if(is.null(add.params$color)) add.params$color <- color
  if(is.null(add.params$fill)) add.params$fill <- fill

  if( any( c("boxplot", "violin", "crossbar") %in% add))
    p <- .add(p, add = intersect(add, c("boxplot", "violin", "crossbar")),
              add.params = add.params, data = data)

  p <- p +
      .geom_exec(geom_jitter, data = data,
                color = color, fill = fill, shape = shape,
                position = position, ...)

  if("pointrange" %in% add) p <- .add(p, add = "pointrange", add.params = add.params, data = data,
                                      position = position_dodge(0.3))


  # Select and order
  if(is.null(select)) select <- order
  if (!is.null(select) | !is.null(order))
    p <- p + scale_x_discrete(limits = as.character(select))
   p <- ggpar(p, palette = palette, ...)

  p
}


