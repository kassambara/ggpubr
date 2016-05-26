#' @include utilities.R ggpar.R
NULL
#' Plot convex hull of a set of points
#' @description Plot convex hull of a set of points.
#' @inheritParams ggplot2::layer
#' @inheritParams ggplot2::stat_ellipse
#' @seealso \code{\link{ggpar}}, \code{\link{ggscatter}}
#' @examples
#' # Load data
#' data("mtcars")
#' df <- mtcars
#' df$cyl <- as.factor(df$cyl)
#'
#' # scatter plot with convex hull
#' ggscatter(df, x = "wt", y = "mpg", color = "cyl")+
#'  stat_chull(aes(color = cyl))
#'
#' ggscatter(df, x = "wt", y = "mpg", color = "cyl")+
#'  stat_chull(aes(color = cyl, fill = cyl), alpha = 0.1, geom = "polygon")
#'
#' @export
stat_chull <- function(mapping = NULL, data = NULL, geom = "path",
                       position = "identity", na.rm = FALSE, show.legend = NA,
                       inherit.aes = TRUE, ...) {
  layer(
    stat = StatChull, data = data, mapping = mapping, geom = geom,
    position = position, show.legend = show.legend, inherit.aes = inherit.aes,
    params = list(na.rm = na.rm, ...)
  )
}

# Convex ellipse
StatChull <- ggproto("StatChull", Stat,
                     compute_group = function(data, scales) {
                       data[chull(data$x, data$y), , drop = FALSE]
                     },
                    required_aes = c("x", "y")
)

