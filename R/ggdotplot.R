#' @include utilities.R ggpar.R
NULL
#' Dot plot
#' @description Create a dot plot.
#' @inheritParams ggboxplot
#' @param x,y x and y variables for drawing.
#' @param color,fill outline and fill colors.
#' @param ... other arguments to be passed to geom_dotplot.
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
#' ggdotplot(df, x = "dose", y = "len")
#'
#' # Change the plot orientation: horizontal
#' ggdotplot(df, "dose", "len", orientation = "horiz")
#'
#'
#' # Select and order items
#' # ++++++++++++++++++++++++++++++
#' # Select which items to display: "0.5" and "2"
#' ggdotplot(df, "dose", "len",
#'    select = c("0.5", "2"))
#' # Change the default order of items
#' ggdotplot(df, "dose", "len",
#'    order = c("2", "1", "0.5"))
#'
#' # Add summary statistics
#' # ++++++++++++++++++++++++++
#'
#' # Add box plot
#' ggdotplot(df, x = "dose", y = "len",
#'  add = "boxplot")
#'
#' # Add violin
#' ggdotplot(df, x = "dose", y = "len",
#'  add = "violin")
#'
#' # Add mean_sd
#' ggdotplot(df, x = "dose", y = "len",
#'  add = "mean_sd")
#'
#' # Change error.plot to "crossbar"
#' ggdotplot(df, x = "dose", y = "len",
#'  add = "mean_sd", add.params = list(width = 0.5),
#'  error.plot = "crossbar")
#'
#' # Add violin + mean_sd
#' ggdotplot(df, x = "dose", y = "len",
#'  add = c("violin", "mean_sd"))
#'
#'
#'
#' # Change colors
#' # +++++++++++++++++++++++++++
#' # Change colors
#'  ggdotplot(df, "dose", "len", fill = "#2E9FDF",
#'   add = "pointrange", add.params = list(color = "red"))
#'
#' # Change fill colors by groups: dose
#'  ggdotplot(df, "dose", "len", fill = "dose",
#'    add = "boxplot")
#'
#' # Change fill and outline colors by groups
#'  ggdotplot(df, "dose", "len", fill = "dose", color = "dose",
#'    add = "boxplot")
#'
#' # Use custom color palette
#'  ggdotplot(df, "dose", "len", fill = "dose", color = "dose",
#'   palette = c('#999999','#E69F00','#56B4E9') )
#'
#' # Use brewer palette
#' ggdotplot(df, "dose", "len", fill = "dose", color = "dose",
#'  palette = "Dark2")
#'
#' # Use grey palette
#' ggdotplot(df, "dose", "len", fill = "dose", color = "dose",
#' palette = "grey", add.params = list(color = "red"))
#'
#'
#' # Plot with multiple groups
#' # +++++++++++++++++++++
#' # Change color by a second group : "supp"
#' # Change the transparency alpha = 0.5
#' ggdotplot(df, "dose", "len", fill = "supp", color = "supp",
#'    alpha = 0.5)
#'
#'
#' # Add boxplot
#' ggdotplot(df, "dose", "len", color = "supp", fill = "supp",
#'  add = "boxplot")
#'
#' @export
ggdotplot <- function(data, x, y,
                      color = "black", fill = "lightgray", palette = NULL,
                      size = 1,
                      select = NULL, order = NULL,
                      add = "mean_se",
                      add.params = list(),
                      error.plot = "pointrange",
                      ggtheme = theme_pubr(),
                      ...)
{

  data[, x] <- factor(data[, x])

  p <- ggplot(data, aes_string(x, y))
  if("none" %in% add) add <- "none"

  if(is.null(add.params$fill)) add.params$fill <- "white"
  add.params <- .check_add.params(add, add.params, error.plot, data, color, fill, ...)
  # plot boxplot | violin | crossbar before jitter
  if( any( c("boxplot", "violin") %in% add)){
    p <- .add(p, add = intersect(add, c("boxplot", "violin")),
              add.params = add.params, data = data)
  }
  if(error.plot == "crossbar"){
    p <- .add(p, add = setdiff(add, c("boxplot", "violin", "jitter")),
              add.params = add.params, data = data, error.plot = error.plot)
  }
  # Plot jitter
  p <- p +
      .geom_exec(geom_dotplot, data = data,
                binaxis = "y", stackdir = "center",
                color = color, fill = fill,
                position = position_dodge(0.8), stackratio = 1.2,
                dotsize = size, ...)

  # Add errors
  if(error.plot == "crossbar"){}
  else p <- .add(p, add = setdiff(add, c("boxplot", "violin", "jitter")),
            add.params = add.params, error.plot = error.plot)

  # Select and order
  if(is.null(select)) select <- order
  if (!is.null(select) | !is.null(order))
    p <- p + scale_x_discrete(limits = as.character(select))
   p <- ggpar(p, palette = palette, ggtheme = ggtheme, ...)

  p
}


