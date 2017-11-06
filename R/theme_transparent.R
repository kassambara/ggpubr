#' @include utilities.R
#' @import ggplot2
NULL
#'Create a ggplot with Transparent Background
#'
#'@description Create a ggplot with transparent background.
#'@param base_size base font size
#'@param base_family base font family
#'@seealso \code{\link{theme_pubr}}
#' @examples
#'# Create a scatter plot
#'sp <- ggscatter(iris, x = "Sepal.Length", y = "Sepal.Width",
#'                color = "Species", palette = "jco",
#'                size = 3, alpha = 0.6)
#'sp
#'
#'# Transparent theme
#'sp + theme_transparent()
#'
#'@export
theme_transparent <- function (base_size = 12, base_family = "")
{
   theme_classic(base_size = base_size, base_family = base_family) +
    theme(
      panel.background = element_rect(fill = "transparent", colour = NA),
      plot.background = element_rect(fill = "transparent", colour = NA),
      legend.background = element_rect(fill = "transparent", colour = NA),
      legend.box.background = element_rect(fill = "transparent", colour = NA),
      axis.line = element_blank(),
      axis.title = element_blank(),
      plot.title = element_blank(),
      axis.text = element_blank(),
      axis.ticks = element_blank()
    )
}
