#' @include utilities_color.R
NULL
#'Set Color Palette
#'
#'@description \itemize{ \item \code{change_palette(), set_palette()}: Change
#'both color and fill palettes. \item \code{color_palette()}: change color
#'palette only. \item \code{fill_palette()}: change fill palette only.
#'
#'}
#'@inheritParams get_palette
#'@param p a ggplot
#'@param ... other arguments passed to ggplot2 scale_color_xxx() and
#'  scale_fill_xxx() functions.
#'
#'@seealso \link{get_palette}.
#'
#'
#'@examples
#'# Load data
#'data("ToothGrowth")
#'df <- ToothGrowth
#'
#'# Basic plot
#'p <- ggboxplot(df, x = "dose", y = "len",
#'    color = "dose")
#'p
#'
#'# Change the color palette
#' set_palette(p, "jco")
#'@name set_palette
#'@rdname set_palette
#'@export
set_palette <- function(p, palette){
  p + .ggcolor(palette)+
    .ggfill(palette)
}

#'@rdname set_palette
#'@export
change_palette <- function(p, palette){
  set_palette(p, palette)
}

#'@rdname set_palette
#'@export
color_palette <- function(palette = NULL, ...) {
  brewerpal <- .brewerpal()
  ggscipal <- .ggscipal()

  res <- NULL
  if (is.null(palette))
    palette <- ""
  if (length(palette) == 1) {
    if (palette %in% brewerpal)
      ggplot2::scale_color_brewer(..., palette = palette)
    else if (palette %in% ggscipal)
      .scale_color_ggsci(palette = palette)
    else if (palette == "grey")
      ggplot2::scale_color_grey(..., start = 0.8, end = 0.2)
    else if (palette == "hue")
      ggplot2::scale_color_hue(...)
    else if(.is_color(palette))
      ggplot2::scale_color_manual(..., values = palette)
  }
  else if (palette[1] != "")
    ggplot2::scale_color_manual(..., values = palette)
}

#'@rdname set_palette
#'@export
fill_palette <- function(palette = NULL, ...){

  brewerpal <- .brewerpal()
  ggscipal <- .ggscipal()

  res <- NULL
  if (is.null(palette))
    palette <- ""
  if (length(palette) == 1) {
    if (palette %in% brewerpal)
      ggplot2::scale_fill_brewer(..., palette = palette)
    else if (palette %in% ggscipal)
      .scale_fill_ggsci(palette = palette)
    else if (palette == "grey")
      ggplot2::scale_fill_grey(..., start = 0.8, end = 0.2)
    else if (palette == "hue")
      ggplot2::scale_fill_hue(...)
    else if(.is_color(palette))
      ggplot2::scale_fill_manual(..., values = palette)
  }
  else if (palette[1] != "")
    ggplot2::scale_fill_manual(..., values = palette)
}
