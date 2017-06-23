#' @include utilities.R
NULL
#' Storing grid.arrange() arrangeGrob() and plots
#'
#' @description Transform the output of \code{\link[gridExtra]{arrangeGrob}()}
#'   and \code{\link[gridExtra]{grid.arrange}()} to a an object of class ggplot.
#' @param x an object of class gtable or grob as returned by the functions
#'   \code{\link[gridExtra]{arrangeGrob}()} and
#'   \code{\link[gridExtra]{grid.arrange}()}.
#' @return an object of class ggplot.
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
  cowplot::ggdraw() +
    cowplot::draw_grob(grid::grobTree(x))
}
