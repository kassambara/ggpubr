#' @include utilities.R ggpar.R
NULL
#' Dot plot
#' @description Create a dot plot.
#' @inheritParams ggboxplot
#' @param binwidth numeric value specifying bin width. use value between 0 and 1
#'   when you have a strong dense dotplot. For example binwidth = 0.2 .
#' @param ... other arguments to be passed to
#'   \code{\link[ggplot2]{geom_dotplot}}, \code{\link{ggpar}} and
#'   \code{\link{facet}}.
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
#' # Basic plot with summary statistics : mean_sd
#' # +++++++++++++++++++++++++++
#' ggdotplot(df, x = "dose", y = "len",
#'    add = "mean_sd")
#'
#' # Change error.plot to "crossbar"
#' ggdotplot(df, x = "dose", y = "len",
#'  add = "mean_sd", add.params = list(width = 0.5),
#'  error.plot = "crossbar")
#'
#'
#' # Add box plot
#' ggdotplot(df, x = "dose", y = "len",
#'  add = "boxplot")
#'
#' # Add violin + mean_sd
#' ggdotplot(df, x = "dose", y = "len",
#'  add = c("violin", "mean_sd"))
#'
#'
#' # Change colors
#' # +++++++++++++++++++++++++++
#' # Change fill and outline colors by groups: dose
#' # Use custom color palette
#'  ggdotplot(df, "dose", "len",
#'      add = "boxplot",
#'       color = "dose", fill = "dose",
#'       palette = c("#00AFBB", "#E7B800", "#FC4E07"))
#'
#'
#' # Plot with multiple groups
#' # +++++++++++++++++++++
#' # Change color by a second group : "supp"
#' ggdotplot(df, "dose", "len", fill = "supp", color = "supp",
#'     palette = c("#00AFBB", "#E7B800"))
#'
#'
#' @export
ggdotplot <- function(data, x, y, combine = FALSE, merge = FALSE,
                      color = "black", fill = "lightgray", palette = NULL,
                      title = NULL, xlab = NULL, ylab = NULL,
                      facet.by = NULL, panel.labs = NULL, short.panel.labs = TRUE,
                      size = NULL, binwidth = NULL,
                      select = NULL, remove = NULL, order = NULL,
                      add = "mean_se",
                      add.params = list(),
                      error.plot = "pointrange",
                      label = NULL, font.label = list(size = 11, color = "black"),
                      label.select = NULL, repel = FALSE, label.rectangle = FALSE,
                      ggtheme = theme_pubr(),
                      ...)
{
  # Default options
  #:::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::
  .opts <- list(
    combine = combine, merge = merge,
    color = color, fill = fill, palette = palette,
    title = title, xlab = xlab, ylab = ylab,
    facet.by = facet.by, panel.labs = panel.labs, short.panel.labs = short.panel.labs,
    size = size, binwidth = binwidth,
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

  .opts$fun <- ggdotplot_core
  if(missing(ggtheme) & (!is.null(facet.by) | combine))
    .opts$ggtheme <- theme_pubr(border = TRUE)
  p <- do.call(.plotter, .opts)

  if(.is_list(p) & length(p) == 1) p <- p[[1]]
  return(p)
}


ggdotplot_core <- function(data, x, y,
                      color = "black", fill = "lightgray", palette = NULL,
                      size = NULL, dotsize = size,
                      binwidth = NULL,
                      add = "mean_se",
                      add.params = list(),
                      error.plot = "pointrange",
                      ggtheme = theme_pubr(),
                      ...)
{
  if(!is.factor(data[, x])) data[, x] <- as.factor(data[, x])
  . <- NULL

  p <- ggplot(data, aes_string(x, y))
  if("none" %in% add) add <- "none"

  if(is.null(add.params$fill)) add.params$fill <- "white"
  add.params <- .check_add.params(add, add.params, error.plot, data, color, fill, ...)
  # plot boxplot | violin | crossbar before jitter
  if( any( c("boxplot", "violin") %in% add)){
    p <- add.params %>%
      .add_item(p = p, add = intersect(add, c("boxplot", "violin") )) %>%
      do.call(ggadd, .)
  }
  if(error.plot == "crossbar"){
    p <- add.params %>%
      .add_item(p = p, error.plot = error.plot,
                add = setdiff(add, c("boxplot", "violin", "jitter"))) %>%
      do.call(ggadd, .)
  }
  # Plot jitter
  p <- p +
      geom_exec(geom_dotplot, data = data,
                binaxis = "y", stackdir = "center",
                color = color, fill = fill,
                position = position_dodge(0.8), stackratio = 1,
                dotsize = dotsize, binwidth = binwidth, ...)

  # Add errors
  if(error.plot == "crossbar"){}
  else p <- add.params %>%
    .add_item(p = p, error.plot = error.plot,
              add = setdiff(add, c("boxplot", "violin", "jitter"))) %>%
    do.call(ggadd, .)

   p <- ggpar(p, palette = palette, ggtheme = ggtheme, ...)

  p
}


