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
#' @seealso \code{\link{ggpar}}
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
                         title = NULL, xlab = NULL, ylab = NULL,
                         facet.by = NULL, panel.labs = NULL, short.panel.labs = TRUE,
                         color = "black", fill = "white", palette = NULL,
                         shape = 19, size = NULL,
                         select = NULL, remove = NULL, order = NULL,
                         add = "mean_se",
                         add.params = list(),
                         error.plot = "pointrange",
                         label = NULL, font.label = list(size = 11, color = "black"),
                         label.select = NULL, repel = FALSE, label.rectangle = FALSE,
                         jitter = 0.2,
                         position = position_jitter(jitter),
                         ggtheme = theme_pubr(),
                         ...)
{

  .opts <- match.call(expand.dots = TRUE)
  .opts <- as.list(.opts)
  .opts[[1]] <- NULL
  .opts$fun <- ggstripchart_core
  if(missing(ggtheme) & (!is.null(facet.by) | combine))
    .opts$ggtheme <- theme_pubr(border = TRUE)
  p <- do.call(.plotter, .opts)

  if(.is_list(p) & length(p) == 1) p <- p[[1]]
  return(p)

}

ggstripchart_core <- function(data, x, y,
                      color = "black", fill = "white", palette = NULL,
                      shape = 19, size = NULL,
                      add = "mean_se",
                      add.params = list(),
                      error.plot = "pointrange",
                      jitter = 0.2,
                      position = position_jitter(jitter),
                      ggtheme = theme_pubr(),
                      ...)
{

  if(!is.factor(data[, x])) data[, x] <- as.factor(data[, x])
  . <- NULL

  p <- ggplot(data, aes_string(x, y))
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
  set.seed(123)
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

   p <- ggpar(p, palette = palette, ggtheme = ggtheme, ...)

  p
}


