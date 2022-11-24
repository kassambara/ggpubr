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




# Helper functions
# Extracted from RGraphics package
# :::::::::::::::::::::::::::::::::::::::::::
splitString <- function(text) {
  strings <- strsplit(text, " ")[[1]]
  if (length(strings) < 2)
    return(text)
  newstring <- strings[1]
  linewidth <- grid::stringWidth(newstring)
  gapwidth <- grid::stringWidth(" ")
  availwidth <-
    grid::convertWidth(unit(1, "npc"),
                 "in", valueOnly=TRUE)
  for (i in 2:length(strings)) {
    width <- grid::stringWidth(strings[i])
    if (grid::convertWidth(linewidth + gapwidth + width,
                     "in", valueOnly=TRUE) <
        availwidth) {
      sep <- " "
      linewidth <- linewidth + gapwidth + width
    } else {
      sep <- "\n"
      linewidth <- width
    }
    newstring <- paste(newstring, strings[i], sep=sep)
  }
  newstring
}

#' @method drawDetails splitText
#' @rdname ggparagraph
#' @param x a grid grob
#' @param recording a logical value indicating whether a grob is being added to
#'   the display list or redrawn from the display list.
#' @export
drawDetails.splitText <- function(x, recording) {
  grid::grid.text(splitString(x$text),
            x=0, y=1, just=c("left", "top"))
}
