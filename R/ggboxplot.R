#' @include utilities.R ggpar.R
NULL
#'Box plot
#'@description Create a box plot with points. Box plots display a group of
#'  numerical data through their quartiles.
#'@param data a data frame
#'@param x character string containing the name of x variable.
#'@param y character vector containing one or more variables to plot
#'@param combine logical value. Default is FALSE. Used only when y is a vector
#'  containing multiple variables to plot. If TRUE, create a multi-panel plot by
#'  combining the plot of y variables.
#'@param merge logical or character value. Default is FALSE. Used only when y is
#'  a vector containing multiple variables to plot. If TRUE, merge multiple y
#'  variables in the same plotting area. Allowed values include also "asis"
#'  (TRUE) and "flip". If merge = "flip", then y variables are used as x tick
#'  labels and the x variable is used as grouping variable.
#'@param color outline color.
#'@param fill fill color.
#'@param palette the color palette to be used for coloring or filling by groups.
#'  Allowed values include "grey" for grey color palettes; brewer palettes e.g.
#'  "RdBu", "Blues", ...; or custom color palette e.g. c("blue", "red"); and
#'  scientific journal palettes from ggsci R package, e.g.: "npg", "aaas",
#'  "lancet", "jco", "ucscgb", "uchicago", "simpsons" and "rickandmorty".
#'@param bxp.errorbar logical value. If TRUE, shows error bars of box plots.
#'@param bxp.errorbar.width numeric value specifying the width of box plot error
#'  bars. Default is 0.4.
#'@param linetype line types.
#'@param size Numeric value (e.g.: size = 1). change the size of points and
#'  outlines.
#'@param width numeric value between 0 and 1 specifying box width.
#'@inheritParams ggplot2::geom_boxplot
#'@param outlier.shape point shape of outlier. Default is 19. To hide outlier,
#'  specify \code{outlier.shape = NA}. When jitter is added, then outliers will
#'  be automatically hidden.
#'@inheritParams facet
#'@inheritParams ggpar
#'@inheritParams ggtext
#'@param select character vector specifying which items to display.
#'@param remove character vector specifying which items to remove from the plot.
#'@param order character vector specifying the order of items.
#'@param add character vector for adding another plot element (e.g.: dot plot or
#'  error bars). Allowed values are one or the combination of: "none",
#'  "dotplot", "jitter", "boxplot", "point", "mean", "mean_se", "mean_sd",
#'  "mean_ci", "mean_range", "median", "median_iqr", "median_hilow",
#'  "median_q1q3", "median_mad", "median_range"; see ?desc_statby for more
#'  details.
#'@param add.params parameters (color, shape, size, fill, linetype) for the
#'  argument 'add'; e.g.: add.params = list(color = "red").
#'@param error.plot plot type used to visualize error. Allowed values are one of
#'  c("pointrange", "linerange", "crossbar", "errorbar", "upper_errorbar",
#'  "lower_errorbar", "upper_pointrange", "lower_pointrange", "upper_linerange",
#'  "lower_linerange"). Default value is "pointrange" or "errorbar". Used only
#'  when add != "none" and add contains one "mean_*" or "med_*" where "*" = sd,
#'  se, ....
#'@param font.label a list which can contain the combination of the following
#'  elements: the size (e.g.: 14), the style (e.g.: "plain", "bold", "italic",
#'  "bold.italic") and the color (e.g.: "red") of labels. For example font.label
#'  = list(size = 14, face = "bold", color ="red"). To specify only the size and
#'  the style, use font.label = list(size = 14, face = "plain").
#'@param ggtheme function, ggplot2 theme name. Default value is theme_pubr().
#'  Allowed values include ggplot2 official themes: theme_gray(), theme_bw(),
#'  theme_minimal(), theme_classic(), theme_void(), ....
#'@param ... other arguments to be passed to
#'  \code{\link[ggplot2]{geom_boxplot}}, \code{\link{ggpar}} and
#'  \code{\link{facet}}.
#'@details The plot can be easily customized using the function ggpar(). Read
#'  ?ggpar for changing: \itemize{ \item main title and axis labels: main, xlab,
#'  ylab \item axis limits: xlim, ylim (e.g.: ylim = c(0, 30)) \item axis
#'  scales: xscale, yscale (e.g.: yscale = "log2") \item color palettes: palette
#'  = "Dark2" or palette = c("gray", "blue", "red") \item legend title, labels
#'  and position: legend = "right" \item plot orientation : orientation =
#'  c("vertical", "horizontal", "reverse") }
#'
#'@section Suggestions for the argument "add": Suggested values are one of
#'  c("dotplot", "jitter").
#'
#'@seealso \code{\link{ggpar}}, \code{\link{ggviolin}}, \code{\link{ggdotplot}}
#'  and \code{\link{ggstripchart}}.
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
#'@export
ggboxplot <- function(data, x, y, combine = FALSE, merge = FALSE,
                      color = "black", fill = "white", palette = NULL,
                      title = NULL, xlab = NULL, ylab = NULL,
                      bxp.errorbar = FALSE, bxp.errorbar.width = 0.4,
                      facet.by = NULL, panel.labs = NULL, short.panel.labs = TRUE,
                      linetype = "solid", size = NULL, width = 0.7,  notch = FALSE,
                      outlier.shape = 19,
                      select = NULL, remove = NULL, order = NULL,
                      add = "none", add.params = list(),
                      error.plot = "pointrange",
                      label = NULL, font.label = list(size = 11, color = "black"),
                      label.select = NULL, repel = FALSE, label.rectangle = FALSE,
                      ggtheme = theme_pubr(),...)
{

  # Default options
  #:::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::
  .opts <- list(
    combine = combine, merge = merge,
    color = color, fill = fill, palette = palette,
    title = title, xlab = xlab, ylab = ylab,
    bxp.errorbar = bxp.errorbar, bxp.errorbar.width = bxp.errorbar.width,
    facet.by = facet.by, panel.labs = panel.labs, short.panel.labs = short.panel.labs,
    linetype = linetype, size = size, width = width,  notch = notch, outlier.shape = outlier.shape,
    select = select , remove = remove, order = order,
    add = add, add.params = add.params, error.plot = error.plot,
    label = label, font.label = font.label, label.select = label.select,
    repel = repel, label.rectangle = label.rectangle, ggtheme = ggtheme, ...)
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

  .opts$fun <- ggboxplot_core
  if(missing(ggtheme) & (!is.null(facet.by) | combine))
    .opts$ggtheme <- theme_pubr(border = TRUE)
  p <- do.call(.plotter, .opts)

  if(.is_list(p) & length(p) == 1) p <- p[[1]]
  return(p)
}



ggboxplot_core <- function(data, x, y,
                      color = "black", fill = "white", palette = NULL,
                      linetype = "solid", size = NULL, width = 0.7,  notch = FALSE,
                      outlier.shape = 19,
                      title = NULL, xlab = NULL, ylab = NULL,
                      bxp.errorbar = FALSE, bxp.errorbar.width = 0.4,
                      add = "none", add.params = list(),
                      error.plot = "pointrange",
                      ggtheme = theme_pubr(),
                      ...)
{

  if(!is.factor(data[, x])) data[, x] <- as.factor(data[, x])
  if("jitter" %in% add) outlier.shape <- NA

  p <- ggplot(data, create_aes(list(x = x, y = y)))
  if(bxp.errorbar){
    p <- p + stat_boxplot(geom = "errorbar", width = bxp.errorbar.width)
  }

  p <- p + geom_exec(geom_boxplot, data = data,
              color = color, fill = fill, linetype = linetype,
              size = size, width = width, notch = notch,
              outlier.shape = outlier.shape,
              position = position_dodge(0.8), size = size,...)

  # Add
  add.params <- .check_add.params(add, add.params, error.plot, data, color, fill, ...) %>%
    .add_item(p = p, add = add, error.plot = error.plot)
  p <- do.call(ggadd, add.params) %>%
    ggpar(palette = palette, ggtheme = ggtheme,
          title = title, xlab = xlab, ylab = ylab, ...)
  p
}


