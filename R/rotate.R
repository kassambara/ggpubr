#' Rotate a ggplot Horizontally
#'
#' @description Rotate a ggplot to create horizontal plots. Wrapper around
#'  \code{\link[ggplot2]{coord_flip}}.
#'  Read more: \href{https://www.datanovia.com/learn/data-visualization/ggpubr/customize}{Customize ggpubr Plots in R with ggpar()}.
#' @param ... other arguments to pass to \code{\link[ggplot2]{coord_flip}}.
#'
#' @examples
#' # Load data
#' data("ToothGrowth")
#' df <- ToothGrowth
#'
#' # Basic plot
#' p <- ggboxplot(df,
#'   x = "dose", y = "len",
#'   color = "dose", palette = "jco"
#' )
#' p
#' # Create horizontal plots
#' p + rotate()
#' @export
rotate <- function(...) {
  coord_flip(...)
}
