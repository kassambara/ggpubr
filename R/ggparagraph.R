#' @include utilities.R
NULL
#'Draw a Paragraph of Text
#'@description Draw a paragraph o text. Splits a long text into multiple lines
#'  (by inserting line breaks) so that the output will fit within the current
#'  viewport.
#'@param text the text to plot.
#'@param color font color, example: color = "black"
#'@param size font size, example: size = 12
#'@param face font face. Allowed values are one of "plain", "italic", "bold",
#'  "bold.italic".
#' @param family font family
#' @param lineheight Line height, example: lineheight = 2.
#'@param ... other arguments passed to \link[grid]{grob}.
#'@author Alboukadel Kassambara <alboukadel.kassambara@@gmail.com>
#' @examples
#' # Density plot
#'density.p <- ggdensity(iris, x = "Sepal.Length",
#'                       fill = "Species", palette = "jco")
#'
#'# Text plot
#'text <- paste("iris data set gives the measurements in cm",
#'              "of the variables sepal length and width",
#'              "and petal length and width, respectively,",
#'              "for 50 flowers from each of 3 species of iris.",
#'              "The species are Iris setosa, versicolor, and virginica.", sep = " ")
#'text.p <- ggparagraph(text, face = "italic", size = 12)
#'
#'# Arrange the plots on the same page
#'ggarrange(density.p, text.p,
#'          ncol = 1, nrow = 2,
#'          heights = c(1, 0.3))
#'
#'@export
ggparagraph <- function(text, color = NULL, size = NULL, face = NULL,
                        family = NULL, lineheight = NULL)
{

  style <- grid::gpar(col = color, fontsize = size,
                      fontface = face, fontfamily = family,
                      lineheight = lineheight)

  p <- grid::grob(text = text, cl = "splitText", gp = style)
  p <- as_ggplot(p)
  return(p)
}
