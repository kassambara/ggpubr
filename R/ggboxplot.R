#' @include utilities.R ggpar.R
NULL
#' Box plot
#' @description Create a box plot with points. Box plots display a group of
#'   numerical data through their quartiles.
#' @param data a data frame
#' @param x,y x and y variables for drawing.
#' @param color,fill outline and fill colors.
#' @param palette the color palette to be used for coloring or filling by
#'   groups. Allowed values include "grey" for grey color palettes; brewer
#'   palettes e.g. "RdBu", "Blues", ...; or custom color palette e.g. c("blue",
#'   "red"); and scientific journal palettes from ggsci R package, e.g.: "npg", "aaas", "lancet", "jco",
#'   "ucscgb", "uchicago", "simpsons" and "rickandmorty".
#' @param linetype line types.
#' @param size Numeric value (e.g.: size = 1). change the size of points and outlines.
#' @param width plot width.
#' @inheritParams ggplot2::geom_boxplot
#' @param select character vector specifying which items to display.
#' @param order character vector specifying the order of items.
#' @param add character vector for adding another plot element (e.g.: dot plot
#'   or error bars). Allowed values are one or the combination of: "none",
#'   "dotplot", "jitter", "boxplot", "point", "mean", "mean_se", "mean_sd", "mean_ci",
#'   "mean_range", "median", "median_iqr", "median_mad", "median_range"; see
#'   ?desc_statby for more details.
#' @param add.params parameters (color, shape, size, fill, linetype) for the
#'   argument 'add'; e.g.: add.params = list(color = "red").
#' @param error.plot plot type used to visualize error. Allowed values are one
#'   of c("pointrange", "linerange", "crossbar", "errorbar", "upper_errorbar",
#'   "lower_errorbar", "upper_pointrange", "lower_pointrange",
#'   "upper_linerange", "lower_linerange"). Default value is "pointrange" or "errorbar". Used
#'   only when add != "none" and add contains one "mean_*" or "med_*" where "*"
#'   = sd, se, ....
#' @param ggtheme function, ggplot2 theme name. Default value is theme_pubr().
#'   Allowed values include ggplot2 official themes: theme_gray(), theme_bw(),
#'   theme_minimal(), theme_classic(), theme_void(), ....
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
#' @section Suggestions for the argument "add": Suggested values are one of
#'   c("dotplot", "jitter").
#'
#' @seealso \code{\link{ggpar}}, \code{\link{ggviolin}}, \code{\link{ggdotplot}} and \code{\link{ggstripchart}}.
#' @examples
#' # Load data
#' data("ToothGrowth")
#' df <- ToothGrowth
#'
#' # Basic plot
#' # +++++++++++++++++++++++++++
#' # width: change box plots width
#' ggboxplot(df, x = "dose", y = "len", width = 0.8)
#'
#' # Change orientation: horizontal
#' ggboxplot(df, "dose", "len", orientation = "horizontal")
#'
#' # Notched box plot
#' ggboxplot(df, x = "dose", y = "len",
#'    notch = TRUE)
#'
#' # Add dots
#' # ++++++++++++++++++++++++++
#' ggboxplot(df, x = "dose", y = "len",
#'    add = "dotplot")
#'
#' # Add jitter points and change the shape by groups
#' ggboxplot(df, x = "dose", y = "len",
#'    add = "jitter", shape = "dose")
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
#'  ggboxplot(df, "dose", "len",
#'    color = "black", fill = "gray")
#'
#' # Change outline colors by groups: dose
#' # Use custom color palette
#' # Add jitter points and change the shape by groups
#'  ggboxplot(df, "dose", "len",
#'     color = "dose", palette =c("#00AFBB", "#E7B800", "#FC4E07"),
#'     add = "jitter", shape = "dose")
#'
#' # Change fill color by groups: dose
#'  ggboxplot(df, "dose", "len",
#'      fill = "dose", palette = c("#00AFBB", "#E7B800", "#FC4E07"))
#'
#'
#' # Box plot with multiple groups
#' # +++++++++++++++++++++
#' # fill or color box plot by a second group : "supp"
#' ggboxplot(df, "dose", "len", color = "supp",
#'  palette = c("#00AFBB", "#E7B800"))
#'
#' @export
ggboxplot <- function(data, x, y,
                      color = "black", fill = "white", palette = NULL,
                      linetype = "solid", size = NULL, width = 1,  notch = FALSE,
                      select = NULL, order = NULL,
                      add = "none", add.params = list(),
                      error.plot = "pointrange",
                      ggtheme = theme_classic2(),
                      ...)
{

  # Check data
  .dd <- .check_data(data, x, y)
  data <- .dd$data
  x <- .dd$x
  y <- .dd$y

  data[, x] <- factor(data[, x])
  p <- ggplot(data, aes_string(x, y)) +
      geom_exec(geom_boxplot, data = data,
                color = color, fill = fill, linetype = linetype,
                size = size, width = width, notch = notch,
                position = position_dodge(0.8), size = size,...)

  # Add
  add.params <- .check_add.params(add, add.params, error.plot, data, color, fill, ...)
  p <- .add(p, add = add, add.params = add.params, error.plot = error.plot)

  # Select and order
  if(is.null(select)) select <- order
  if (!is.null(select) | !is.null(order))
    p <- p + scale_x_discrete(limits = as.character(select))
   p <- ggpar(p, palette = palette, ggtheme = ggtheme, ...)

  p
}


