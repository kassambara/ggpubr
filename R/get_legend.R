#' @include utilities.R
NULL
#' Extract Legends from a ggplot object
#' @description Extract the legend labels from a ggplot object.
#' @param p an object of class ggplot.
#' @return an object of class gtable.
#' @examples
#' # Create a scatter plot
#' p <- ggscatter(iris, x = "Sepal.Length", y = "Sepal.Width",
#'         color = "Species", palette = "jco",
#'         ggtheme = theme_minimal())
#' p
#'
#' # Extract the legend. Returns a gtable
#' leg <- get_legend(p)
#'
#' # Convert to a ggplot and print
#' as_ggplot(leg)
#'
#' @export
get_legend <- function(p){
  tmp <- ggplot_gtable(ggplot_build(p))
  leg <- which(sapply(tmp$grobs, function(x) x$name) == "guide-box")
  if(length(leg) > 0) leg <- tmp$grobs[[leg]]
  else leg <- NULL
  leg
}
