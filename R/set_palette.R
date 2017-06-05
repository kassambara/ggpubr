#' @include utilities_color.R
NULL
#'Set Color Palette
#'
#' @description Change color palette. \code{change_palette()} is an alias of \code{set_palette()}.
#' @inheritParams get_palette
#' @param p a ggplot
#'
#' @seealso \link{get_palette}.
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

#'@name set_palette
#'@rdname set_palette
#'@export
change_palette <- function(p, palette){
  set_palette(p, palette)
}
