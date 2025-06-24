#'Add Grids to a ggplot
#'
#'@description Add grids to ggplot.
#'@param axis axis for which grid should be added. Allowed values include \code{c("xy", "x", "y")}.
#'@param color grid line color.
#'@param size numeric value specifying grid line size.
#'@param linetype line type. An integer (0:8), a name (blank, solid, dashed,
#'  dotted, dotdash, longdash, twodash). Sess \code{\link{show_line_types}}.
#'
#'
#'@examples
#'# Load data
#'data("ToothGrowth")
#'
#'# Basic plot
#'p <- ggboxplot(ToothGrowth, x = "dose", y = "len")
#'p
#'
#'# Add border
#' p + grids(linetype = "dashed")
#'@export
grids <- function(axis = c("xy", "x", "y"), color = "grey92", size = NULL, linetype = NULL)
{
  axis <- match.arg(axis)
  grid.major <- element_line(color = color, linewidth = size,
                             linetype = linetype)
  grid.minor <- element_line(color = color, linewidth = 0.25,
                             linetype = linetype)

  switch(axis,
         xy = theme(panel.grid.major = grid.major, panel.grid.minor = grid.minor),
         x = theme(panel.grid.major.x = grid.major, panel.grid.minor.x = grid.minor),
         y = theme(panel.grid.major.y = grid.major, panel.grid.minor.y = grid.minor)
         )
}
