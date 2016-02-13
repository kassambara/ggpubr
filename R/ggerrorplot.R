#'@include utilities.R ggpar.R
NULL
#'Visualizing Error
#'@description Visualizing error.
#'@inheritParams ggboxplot
#'@inheritParams ggplot2::geom_errorbar
#'@param x,y x and y variables for drawing.
#'@param color,fill outline and fill colors.
#'@param desc_stat descriptive statistics to be used for visualizing errors. Default value is "mean_se".
#'Allowed values are one of , "mean", "mean_se", "mean_sd", "mean_ci", "mean_range",
#'"median", "median_iqr", "median_mad", "median_range"; see \code{\link{desc_statby}} for more details.
#'@param ... other arguments to be passed to be passed to ggpar().
#'@details The plot can be easily customized using the function ggpar(). Read
#'   ?ggpar for changing: \itemize{ \item main title and axis labels: main,
#'   xlab, ylab \item axis limits: xlim, ylim (e.g.: ylim = c(0, 30)) \item axis
#'   scales: xscale, yscale (e.g.: yscale = "log2") \item color palettes:
#'   palette = "Dark2" or palette = c("gray", "blue", "red") \item legend title,
#'   labels and position: legend = "right" \item plot orientation : orientation
#'   = c("vertical", "horizontal", "reverse") }
#' @seealso \code{\link{ggpar}}, \code{\link{ggline}}
#' @examples
#'
#' # Data: ToothGrowth data set we'll be used.
#' df<- ToothGrowth
#' head(df, 10)
#'
#' # Plot mean_se
#' ggerrorplot(df, x = "dose", y = "len")
#'
#'
#' # Change desc_stat to mean_sd
#' # (other values include: mean_sd, mean_ci, median_iqr, ....)
#' # Add labels
#' ggerrorplot(df, x = "dose", y = "len",
#'  desc_stat = "mean_sd")
#'
#' # Change error.plot to "errorbar" and add mean point
#' # Visualize the mean of each group
#' ggerrorplot(df, x = "dose", y = "len",
#'  add = "mean", error.plot = "errorbar")
#'
#' # Horizontal plot
#' ggerrorplot(df, x = "dose", y = "len",
#'  add = "mean", error.plot = "errorbar",
#'  orientation = "horizontal")
#'
#'
#' # Change error.plot to "crossbar"
#' ggerrorplot(df, x = "dose", y = "len",
#'  error.plot = "crossbar", width = 0.5)
#'
#'
#' # Add jitter points and errors (mean_se)
#' ggerrorplot(df, x = "dose", y = "len",
#'  add = "jitter")
#'
#' # Add dot and errors (mean_se)
#' ggerrorplot(df, x = "dose", y = "len",
#'  add = "dotplot")
#'
#' # Multiple groups with error bars and jitter point
#' ggerrorplot(df, x = "dose", y = "len",
#'  color = "supp", palette = "Paired",
#'  error.plot = "pointrange",
#'  position = position_dodge(0.5))
#
#'
#'
#' @export
ggerrorplot <- function(data, x, y, desc_stat = "mean_se",
                      color = "black", fill = "white", palette = NULL,
                      size = 1, width = NULL,
                      select = NULL, order = NULL,
                      add = "none",
                      add.params = list(),
                      error.plot = "pointrange",
                      position = position_dodge(),
                      ggtheme = theme_pubr(),
                      ...)
{

  data[, x] <- factor(data[, x])
  error.plot = error.plot[1]
  if("none" %in% add) add <- "none"

  # static summaries for computing mean/median and adding errors
  if(is.null(add.params$fill)) add.params$fill <- "white"
  add.params <- .check_add.params(add, add.params, error.plot, data, color, fill, ...)

  add <- setdiff(add, desc_stat)
  if(inherits(position, "PositionDodge") & is.null(position$width)) position$width = 0.95
  p <- ggplot(data, aes_string(x, y))
  p <- .add(p, add = add, data = data,
            add.params = add.params, error.plot = error.plot,
            position = position)

  # Main plot
  add.params$color <- color
  add.params$fill <- fill
  add.params$size <- size
  add.params$width <- width
  add.params$position <- position
  p <- .add(p, add = desc_stat, data = data,
            add.params = add.params, error.plot = error.plot,
            position = position)

  # Select and order
  if(is.null(select)) select <- order
  if (!is.null(select) | !is.null(order))
    p <- p + scale_x_discrete(limits = as.character(select))
   p <- ggpar(p, palette = palette, ggtheme = ggtheme, ...)

  p
}

