#' @include utilities.R
NULL
#'Storing grid.arrange() arrangeGrob() and plots
#'
#'@description Transform the output of
#'  \code{\link[gridExtra:arrangeGrob]{arrangeGrob}()} and
#'  \code{\link[gridExtra:arrangeGrob]{grid.arrange}()} to a an object of class
#'  ggplot.
#'@param x an object of class gtable or grob as returned by the functions
#'  \code{\link[gridExtra:arrangeGrob]{arrangeGrob}()} and
#'  \code{\link[gridExtra:arrangeGrob]{grid.arrange}()}.
#'@return an object of class ggplot.
#'
#' @examples
#' # Creat some plots
#'bxp <- ggboxplot(iris, x = "Species", y = "Sepal.Length")
#'vp <- ggviolin(iris, x = "Species", y = "Sepal.Length",
#'               add = "mean_sd")
#'
#'# Arrange the plots in one page
#'# Returns a gtable (grob) object
#'library(gridExtra)
#'gt <- arrangeGrob(bxp, vp, ncol = 2)
#'
#'# Transform to a ggplot and print
#'as_ggplot(gt)
#'
#'@export
as_ggplot <- function(x){

  # Open null device to avoid blank page before plot------
  # see cowplot:::as_grob.ggplot
  null_device <- base::getOption(
    "ggpubr.null_device",
    default = cowplot::pdf_null_device
  )
  cur_dev <- grDevices::dev.cur()
  # Open null device to avoid blank page before plot
  null_device(width = 6, height = 6)
  null_dev <- grDevices::dev.cur()
  on.exit({
    grDevices::dev.off(null_dev)
    if (cur_dev > 1) grDevices::dev.set(cur_dev)
  })

  # Convert to ggplot-------------
  cowplot::ggdraw() +
    cowplot::draw_grob(grid::grobTree(x))
}
