#'Rotate X-axis Text
#'
#'@description Rotate the x-axis text (tick mark labels).
#'\itemize{
#'\item \code{rotate_x_text}(): Rotate x axis text.
#'\item \code{rotate_y_text}(): Rotate y axis text.
#'}
#'
#'@param angle numeric value specifying the rotation angle. Default is 90 for vertical x-axis text.
#'@param hjust horizontal justification (in [0, 1]).
#'@param vjust vertical justification (in [0, 1]).
#'@param ... other arguments to pass to the function
#'  \code{\link[ggplot2]{element_text}()}.
#'
#'@examples
#'# Load data
#'data("ToothGrowth")
#'df <- ToothGrowth
#'
#'# Basic plot
#'p <- ggboxplot(df, x = "dose", y = "len")
#'p
#'# Vertical x axis text
#'p + rotate_x_text()
#'# Set rotation angle to 45
#'p + rotate_x_text(45)
#'p + rotate_y_text(45)
#'
#'@name rotate_axis_text
#'@rdname rotate_axis_text
#'@export
rotate_x_text <- function(angle = 90, hjust = NULL, vjust = NULL,  ...){

  if(missing(hjust) & angle > 5)
    hjust <- 1
  if(missing(vjust) & angle == 90)
    vjust <- 0.5

  theme(
    axis.text.x = element_text(angle = angle, hjust = hjust, vjust = vjust, ...)
      )
}

#'@name rotate_axis_text
#'@rdname rotate_axis_text
#'@export
rotate_y_text <- function(angle = 90, hjust = NULL, vjust = NULL,  ...){

  if(missing(hjust) & angle == 90)
    hjust <- 0.5
  else if(missing(hjust) & angle > 5)
    hjust <- 1

  theme(
    axis.text.y = element_text(angle = angle, hjust = hjust, vjust = vjust, ...)
  )
}
