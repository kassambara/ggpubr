#'Set ggplot Panel Border Line
#'
#'@description Change or set ggplot panel border.
#'@param color border line color.
#'@param size numeric value specifying border line size.
#'@param linetype line type. An integer (0:8), a name (blank, solid, dashed,
#'  dotted, dotdash, longdash, twodash). Sess \code{\link{show_line_types}}.
#'
#'
#'@examples
#'# Load data
#'data("ToothGrowth")
#'df <- ToothGrowth
#'
#'# Basic plot
#'p <- ggboxplot(df, x = "dose", y = "len")
#'p
#'
#'# Add border
#' p + border()
#'@export
border <- function(color = "black", size = 1, linetype = NULL){
   theme(panel.background = element_rect(color = color, size = size, linetype = linetype),
            axis.line = element_blank())
}
