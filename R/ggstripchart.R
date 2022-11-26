#' @include utilities.R ggpar.R
NULL
#' Stripcharts
#' @description Create a stripchart, also known as one dimensional scatter
#'   plots. These plots are suitable compared to box plots when sample sizes are
#'   small.
#' @inheritParams ggboxplot
#' @param shape point shape
#' @param jitter the amount of jitter.
#' @param position position adjustment, either as a string, or the result of a
#'   call to a position adjustment function. Used to adjust position for
#'   multiple groups.
#' @param ... other arguments to be passed to
#'   \code{\link[ggplot2]{geom_jitter}}, \code{\link{ggpar}} and
#'   \code{\link{facet}}.
#' @details The plot can be easily customized using the function ggpar(). Read
#'   ?ggpar for changing: \itemize{ \item main title and axis labels: main,
#'   xlab, ylab \item axis limits: xlim, ylim (e.g.: ylim = c(0, 30)) \item axis
#'   scales: xscale, yscale (e.g.: yscale = "log2") \item color palettes:
#'   palette = "Dark2" or palette = c("gray", "blue", "red") \item legend title,
#'   labels and position: legend = "right" \item plot orientation : orientation
#'   = c("vertical", "horizontal", "reverse") }
#'@seealso \code{\link{ggpar}}, \code{\link{ggviolin}}, \code{\link{ggdotplot}}
#'  and \code{\link{ggboxplot}}.
#' @examples
#' # Load data
#' data("ToothGrowth")
#' df <- ToothGrowth
#'
#' # Basic plot with summary statistics: mean_se
#' # +++++++++++++++++++++++++++
#' # Change point shapes by groups: "dose"
#' ggstripchart(df, x = "dose", y = "len",
#'    shape = "dose", size = 3,
#'    add = "mean_se")
#'
#' # Use mean_sd
#' # Change error.plot to "crossbar"
#' ggstripchart(df, x = "dose", y = "len",
#'    shape = "dose", size = 3,
#'    add = "mean_sd", add.params = list(width = 0.5),
#'    error.plot = "crossbar")
#'
#'
#'
#' # Add summary statistics
#' # ++++++++++++++++++++++++++
#'
#' # Add box plot
#' ggstripchart(df, x = "dose", y = "len",
#'  shape = "dose", add = "boxplot")
#'
#' # Add violin + mean_sd
#' ggstripchart(df, x = "dose", y = "len",
#'  shape = "dose", add = c("violin", "mean_sd"))
#'
#'
#' # Change colors
#' # +++++++++++++++++++++++++++
#' # Change colors by groups: dose
#' # Use custom color palette
#'  ggstripchart(df, "dose", "len",  shape = "dose",
#'    color = "dose", palette = c("#00AFBB", "#E7B800", "#FC4E07"),
#'    add = "mean_sd")
#'
#'
#'
#' # Plot with multiple groups
#' # +++++++++++++++++++++
#' # Change shape and color by a second group : "supp"
#' ggstripchart(df, "dose", "len", shape = "supp",
#'   color = "supp", palette = c("#00AFBB", "#E7B800"))
#'
#' # Adjust point position
#' ggstripchart(df, "dose", "len", shape = "supp",
#'   color = "supp", palette = c("#00AFBB", "#E7B800"),
#'   position = position_dodge(0.8) )
#'
#' # You can also use position_jitterdodge()
#' # but fill aesthetic is required
#' ggstripchart(df, "dose", "len",  shape = "supp",
#'    color = "supp", palette = c("#00AFBB", "#E7B800"),
#'    position = position_jitterdodge() )
#'
#' # Add boxplot
#' ggstripchart(df, "dose", "len", shape = "supp",
#'  color = "supp", palette = c("#00AFBB", "#E7B800"),
#'  add = "boxplot", add.params = list(color = "black") )
#'
#' @export
ggstripchart <- function(data, x, y, combine = FALSE, merge = FALSE,
                         color = "black", fill = "white", palette = NULL,
                         title = NULL, xlab = NULL, ylab = NULL,
                         facet.by = NULL, panel.labs = NULL, short.panel.labs = TRUE,
                         shape = 19, size = NULL,
                         select = NULL, remove = NULL, order = NULL,
                         add = "mean_se",
                         add.params = list(),
                         error.plot = "pointrange",
                         label = NULL, font.label = list(size = 11, color = "black"),
                         label.select = NULL, repel = FALSE, label.rectangle = FALSE,
                         jitter = 0.2,
                         position = position_jitter(jitter, seed = 123),
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
    shape = shape, size = size,
    select = select , remove = remove, order = order,
    add = add, add.params = add.params, error.plot = error.plot,
    label = label, font.label = font.label, label.select = label.select,
    repel = repel, label.rectangle = label.rectangle,
    jitter = jitter, position = position, ggtheme = ggtheme, ...)
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

  .opts$fun <- ggstripchart_core
  if(missing(ggtheme) & (!is.null(facet.by) | combine))
    .opts$ggtheme <- theme_pubr(border = TRUE)
  p <- do.call(.plotter, .opts)

  if(.is_list(p) & length(p) == 1) p <- p[[1]]
  return(p)

}

ggstripchart_core <- function(data, x, y,
                      color = "black", fill = "white", palette = NULL,
                      title = NULL, xlab = NULL, ylab = NULL,
                      shape = 19, size = NULL,
                      add = "mean_se",
                      add.params = list(),
                      error.plot = "pointrange",
                      jitter = 0.2,
                      position = position_jitter(jitter, seed = 123),
                      ggtheme = theme_pubr(),
                      ...)
{

  if(!is.factor(data[, x])) data[, x] <- as.factor(data[, x])
  . <- NULL

  p <- ggplot(data, create_aes(list(x = x, y = y)))
  if("none" %in% add) add <- "none"

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
      geom_exec(geom_jitter, data = data,
                color = color, fill = fill, shape = shape,
                position = position, size = size, ...)
  # Add errors
  if(error.plot == "crossbar"){}
  else p <- add.params %>%
    .add_item(p = p, error.plot = error.plot,
              add = setdiff(add, c("boxplot", "violin", "jitter"))) %>%
    do.call(ggadd, .)

   p <- ggpar(p, palette = palette, ggtheme = ggtheme,
              title = title, xlab = xlab, ylab = ylab,...)

  p
}


