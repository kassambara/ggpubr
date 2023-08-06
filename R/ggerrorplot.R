#'@include utilities.R ggpar.R
NULL
#'Visualizing Error
#'@description Visualizing error.
#'@inheritParams ggboxplot
#'@inheritParams ggplot2::geom_errorbar
#'@param ci the percent range of the confidence interval (default is 0.95).
#'@param x,y x and y variables for drawing.
#'@param numeric.x.axis logical. If TRUE, x axis will be treated as numeric. Default is FALSE.
#'@param order character vector specifying the order of items. Considered only when x axis is a factor variable.
#'@param color,fill outline and fill colors.
#'@param desc_stat descriptive statistics to be used for visualizing errors. Default value is "mean_se".
#'Allowed values are one of , "mean", "mean_se", "mean_sd", "mean_ci", "mean_range",
#'"median", "median_iqr", "median_hilow", "median_q1q3", "median_mad", "median_range"; see \code{\link{desc_statby}} for more details.
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
                        numeric.x.axis = FALSE,
                        combine = FALSE, merge = FALSE,
                        color = "black", fill = "white", palette = NULL,
                        size = NULL, width = NULL,
                        title = NULL, xlab = NULL, ylab = NULL,
                        facet.by = NULL, panel.labs = NULL, short.panel.labs = TRUE,
                        select = NULL, remove = NULL, order = NULL,
                        add = "none",
                        add.params = list(),
                        error.plot = "pointrange", ci = 0.95,
                        position = position_dodge(),
                        ggtheme = theme_pubr(),
                        ...)
{

  # Default options
  #:::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::
  .opts <- list(
    desc_stat = desc_stat,
    numeric.x.axis = numeric.x.axis,
    combine = combine, merge = merge,
    color = color, fill = fill, palette = palette,
    size = size, width = width,
    title = title, xlab = xlab, ylab = ylab,
    facet.by = facet.by, panel.labs = panel.labs, short.panel.labs = short.panel.labs,
    select = select , remove = remove, order = order,
    add = add, add.params = add.params, error.plot = error.plot, ci = ci,
    position = position, ggtheme = ggtheme, ...)
  if(!missing(data)) .opts$data <- data
  if(!missing(x)) .opts$x <- x
  if(!missing(y)) .opts$y <- y

  # User options
  #:::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::
  .user.opts <- as.list(match.call(expand.dots = TRUE))
  .user.opts[[1]] <- NULL # Remove the function name
  # keep only user arguments
  for(opt.name in names(.opts)){
    if(is.null(.user.opts[[opt.name]]))
      .opts[[opt.name]] <- NULL
  }
  .opts$fun <- ggerrorplot_core
  if(missing(ggtheme) & (!is.null(facet.by) | combine))
    .opts$ggtheme <- theme_pubr(border = TRUE)
  p <- do.call(.plotter, .opts)

  if(.is_list(p) & length(p) == 1) p <- p[[1]]
  return(p)
}


ggerrorplot_core <- function(data, x, y, desc_stat = "mean_se",
                      numeric.x.axis = FALSE,
                      color = "black", fill = "white", palette = NULL,
                      title = NULL, xlab = NULL, ylab = NULL,
                      size = NULL, width = NULL,
                      select = NULL, order = NULL,
                      add = "none",
                      add.params = list(),
                      error.plot = "pointrange", ci = 0.95,
                      position = position_dodge(0.8),
                      ggtheme = theme_pubr(),
                      ...)
{
  if(numeric.x.axis){}
  else{
    if(!is.null(order)) data[[x]] <- factor(data[[x]], levels = order)
    else if(!is.factor(data[[x]])) data[[x]] <- as.factor(data[[x]])
  }
  error.plot = error.plot[1]
  if("none" %in% add) add <- "none"

  # static summaries for computing mean/median and adding errors
  if(is.null(add.params$fill)) add.params$fill <- "white"
  add.params <- .check_add.params(add, add.params, error.plot, data, color, fill, ...)

  add <- setdiff(add, desc_stat)
  if(inherits(position, "PositionDodge") & is.null(position$width)) position$width = 0.8
  p <- ggplot(data, create_aes(list(x = x, y = y)))
  add.params <- add.params %>%
    .add_item(add = add, data = data, error.plot = error.plot, ci = ci, position = position, p = p)
  p <- do.call(ggadd, add.params)

  # Main plot
  add.params <- add.params %>%
    .add_item(color = color, fill = fill, size = size, width = width,
              add = desc_stat, p = p)
  p <- do.call(ggadd, add.params)

   p <- ggpar(p, palette = palette, ggtheme = ggtheme,
              title = title, xlab = xlab, ylab = ylab,...)

  p
}

