#'Rotate a ggplot Horizontally
#'
#'@description Rotate a ggplot to create horizontal plots. Wrapper around
#'  \code{\link[ggplot2]{coord_flip}}.
#'@param ... other arguments to pass to \code{\link[ggplot2]{coord_flip}}.
#'
#'@examples
#'# Load data
#'data("ToothGrowth")
#'df <- ToothGrowth
#'
#'# Basic plot
#'p <- ggboxplot(df, x = "dose", y = "len",
#'    color = "dose", palette = "jco")
#'p
#'# Create horizontal plots
#'p + rotate()
#'@export
rotate <- function(...){
  coord_flip(...)
}
