#' @include utilities_color.R
NULL
#'Set Gradient Color
#'
#' @description Change gradient color.
#' \itemize{
#' \item \code{gradient_color()}: Change gradient color.
#' \item \code{gradient_fill()}: Change gradient fill.
#' }
#' @param palette the color palette to be used for coloring or filling by
#'   groups. Allowed values include "grey" for grey color palettes; brewer
#'   palettes e.g. "RdBu", "Blues", ...; or custom color palette e.g. c("blue",
#'   "red"); and scientific journal palettes from ggsci R package, e.g.: "npg",
#'   "aaas", "lancet", "jco", "ucscgb", "uchicago", "simpsons" and
#'   "rickandmorty". Can be also a numeric vector; in this
#'   case a basic color palette is created using the function
#'   \link[grDevices]{palette}.
#'
#' @seealso \link{set_palette}.
#'
#'
#'@examples
#' df <- mtcars
#' p <- ggscatter(df, x = "wt", y = "mpg",
#'                color = "mpg")
#'
#' # Change gradient color
#' # Use one custom color
#' p + gradient_color("red")
#'
#' # Two colors
#' p + gradient_color(c("blue",  "red"))
#'
#' # Three colors
#' p + gradient_color(c("blue", "white", "red"))
#'
#' # Use RColorBrewer palette
#' p + gradient_color("RdYlBu")
#'
#' # Use ggsci color palette
#' p + gradient_color("npg")
#' @rdname gradient_color
#' @export
gradient_color <- function(palette){

  if(.is_col_palette(palette)) palette <- .get_pal(palette, k = 3)
  n_palette <- length(palette)

  if(n_palette == 1) {
    palette <- grDevices::colorRampPalette(c("white", palette))(10)
    palette <- palette[c(1, 10)]
    n_palette <- 2
  }
  ggplot2::scale_color_gradientn(colours = palette)
}

#' @rdname gradient_color
#' @export
gradient_fill <- function(palette){

  if(.is_col_palette(palette)) palette <- .get_pal(palette, k = 3)
  n_palette <- length(palette)

  if(n_palette == 1) {
    palette <- grDevices::colorRampPalette(c("white", palette))(10)
    palette <- palette[c(1, 10)]
    n_palette <- 2
  }
  ggplot2::scale_fill_gradientn(colours = palette)
}
