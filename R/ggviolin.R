#' @include utilities.R ggpar.R
NULL
#'Violin plot
#'@description Create a violin plot with error bars. Violin plots are similar to
#'box plots, except that they also show the kernel probability density of the
#'data at different values.
#'@inheritParams ggboxplot
#'@param width violin width.
#'@param alpha color transparency. Values should be between 0 and 1.
#'@inheritParams ggplot2::geom_violin
#' @param ... other arguments to be passed to
#'   \code{\link[ggplot2]{geom_violin}}, \code{\link{ggpar}} and
#'   \code{\link{facet}}.
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
#' # Add jitter points and
#' # change point shape by groups ("dose")
#'ggviolin(df, x = "dose", y = "len",
#' add = "jitter", shape = "dose")
#'
#'
#' # Add mean_sd + jittered points
#' ggviolin(df, x = "dose", y = "len",
#'  add = c("jitter", "mean_sd"))
#'
#' # Change error.plot to "crossbar"
#' ggviolin(df, x = "dose", y = "len",
#'  add = "mean_sd", error.plot = "crossbar")
#'
#'
#' # Change colors
#' # +++++++++++++++++++++++++++
#' # Change outline and fill colors
#' ggviolin(df, "dose", "len",
#'    color = "black", fill = "gray")
#'
#' # Change outline colors by groups: dose
#' # Use custom color palette and add boxplot
#' ggviolin(df, "dose", "len",  color = "dose",
#'    palette = c("#00AFBB", "#E7B800", "#FC4E07"),
#'    add = "boxplot")
#'
#' # Change fill color by groups: dose
#' # add boxplot with white fill color
#' ggviolin(df, "dose", "len", fill = "dose",
#'    palette = c("#00AFBB", "#E7B800", "#FC4E07"),
#'    add = "boxplot", add.params = list(fill = "white"))
#'
#'
#' # Plot with multiple groups
#' # +++++++++++++++++++++
#' # fill or color box plot by a second group : "supp"
#'ggviolin(df, "dose", "len", color = "supp",
#'  palette = c("#00AFBB", "#E7B800"), add = "boxplot")
#'
#'@export
ggviolin <- function(data, x, y, combine = FALSE, merge = FALSE,
                     color = "black", fill = "white", palette = NULL, alpha = 1,
                     title = NULL, xlab = NULL, ylab = NULL,
                     facet.by = NULL, panel.labs = NULL, short.panel.labs = TRUE,
                     linetype = "solid", trim = FALSE, size = NULL, width = 1,
                     draw_quantiles = NULL,
                     select = NULL, remove = NULL, order = NULL,
                     add = "mean_se", add.params = list(),
                     error.plot = "pointrange",
                     label = NULL, font.label = list(size = 11, color = "black"),
                     label.select = NULL, repel = FALSE, label.rectangle = FALSE,
                     position = position_dodge(0.8), ggtheme = theme_pubr(),
                     ...)
{
  # Default options
  #:::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::
  .opts <- list(
    combine = combine, merge = merge,
    color = color, fill = fill, palette = palette, alpha = alpha,
    title = title, xlab = xlab, ylab = ylab,
    facet.by = facet.by, panel.labs = panel.labs, short.panel.labs = short.panel.labs,
    linetype = linetype, trim = trim, size = size, width = width, draw_quantiles = draw_quantiles,
    select = select , remove = remove, order = order,
    add = add, add.params = add.params, error.plot = error.plot,
    label = label, font.label = font.label, label.select = label.select,
    repel = repel, label.rectangle = label.rectangle, position = position, ggtheme = ggtheme, ...)
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

  .opts$fun <- ggviolin_core
  if(missing(ggtheme) & (!is.null(facet.by) | combine))
    .opts$ggtheme <- theme_pubr(border = TRUE)
  if(missing(ggtheme) & !is.null(facet.by))
    .opts$ggtheme <- theme_pubr(border = TRUE)

  p <- do.call(.plotter, .opts)

  if(.is_list(p) & length(p) == 1) p <- p[[1]]
  return(p)

}


ggviolin_core <- function(data, x, y,
                      color = "black", fill = "white", palette = NULL, alpha = 1,
                      title = NULL, xlab = NULL, ylab = NULL,
                      linetype = "solid", trim = FALSE, size = NULL, width = 1,
                      draw_quantiles = NULL,
                      add = "mean_se", add.params = list(),
                      error.plot = "pointrange",
                      ggtheme = theme_pubr(),
                      position = position_dodge(0.8),
                     ...)
{

  if(!is.factor(data[, x])) data[, x] <- as.factor(data[, x])

  pms <- .violin_params(...)

  p <- ggplot(data, aes_string(x, y)) +
      geom_exec(geom_violin, data = data,
                color = color, fill = fill, linetype = linetype,
                trim = trim, size = size, width = width, alpha = alpha,
                position = position, draw_quantiles = draw_quantiles,
                stat = pms$stat, scale = pms$scale)

  # Add
  #+++++++++++++++++++
  if(is.null(add.params$group)){
    if(fill %in% names(data)) add.params$group <- fill
  }
   add.params <- .check_add.params(add, add.params, error.plot, data, color, fill, ...) %>%
     .add_item(p = p, add = add, error.plot = error.plot, position = position)
   p <- do.call(ggadd, add.params) %>%
     ggpar(palette = palette, ggtheme = ggtheme,
           title = title, xlab = xlab, ylab = ylab,...)

  p
}


