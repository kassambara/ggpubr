#' @include utilities.R
NULL

#' Create a Text Graphical object
#' @description Create easily a customized text grob (graphical object). Wrapper around
#'   \code{\link[grid]{textGrob}}.
#' @inheritParams grid::grid.text
#' @param color text font color.
#' @param face font face. Allowed values include one of \code{"plain", "bold",
#'   "italic", "bold.italic"}.
#' @param size font size (e.g.: size = 12)
#' @param lineheight line height (e.g.: lineheight = 2).
#' @param family font family.
#' @param ... other arguments passed to \link[grid]{textGrob}.
#' @return  a text grob.
#' @examples
#'text <- paste("iris data set gives the measurements in cm",
#'              "of the variables sepal length and width",
#'              "and petal length and width, respectively,",
#'              "for 50 flowers from each of 3 species of iris.",
#'              "The species are Iris setosa, versicolor, and virginica.", sep = "\n")
#'
#'# Create a text grob
#'tgrob <- text_grob(text, face = "italic", color = "steelblue")
#'# Draw the text
#'as_ggplot(tgrob)
#'
#' @export
text_grob <- function(label, just = "centre", hjust = NULL, vjust = NULL, rot = 0,
                      color = "black", face = "plain", size = NULL, lineheight = NULL,
                      family = NULL, ...)
{

  gp <- grid::gpar(col = color, fontface = face, fontsize = size,
                   lineheight = lineheight, fontfamily = family)

  tgrob <- grid::textGrob(label = label, just = just, hjust = hjust, vjust = vjust,
                          rot = rot, gp = gp, ...)
  tgrob
}
