#' @include utilities.R ggpar.R
NULL
#' Stripcharts
#' @description Create a stripchart, also known as one dimensional scatter
#'   plots. These plots are suitable compared to box plots when sample sizes are
#'   small.
#' @inheritParams ggboxplot
#' @param x,y x and y variables for drawing.
#' @param color,fill outline and fill colors.
#' @param shape point shape
#' @param position position adjustment, either as a string, or the result of a
#'   call to a position adjustment function. Used to adjust position for
#'   multiple groups.
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
ggstripchart <- function(data, x, y,
                      color = "black", fill = "white", palette = NULL,
                      shape = 19, size = 2,
                      select = NULL, order = NULL,
                      add = "mean_se",
                      add.params = list(),
                      error.plot = "pointrange",
                      position = position_jitter(0.4),
                      ggtheme = theme_classic2(),
                      ...)
{

  # Check data
  .dd <- .check_data(data, x, y)
  data <- .dd$data
  x <- .dd$x
  y <- .dd$y


  data[, x] <- factor(data[, x])

  p <- ggplot(data, aes_string(x, y))
  if("none" %in% add) add <- "none"

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
      .geom_exec(geom_jitter, data = data,
                color = color, fill = fill, shape = shape,
                position = position, size = size, ...)
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


