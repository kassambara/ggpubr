#' @include utilities.R
NULL
#' Draw group mean points
#' @description Draw the mean point of each group.
#' @inheritParams ggplot2::layer
#' @param ... other arguments to pass to \code{\link[ggplot2]{geom_point}}.
#' @param na.rm If FALSE (the default), removes missing values with a warning.
#'   If TRUE silently removes missing values.
#' @seealso \code{\link{stat_conf_ellipse}}, \code{\link{stat_chull}} and
#'   \code{\link{ggscatter}}
#' @examples
#' # Load data
#' data("mtcars")
#' df <- mtcars
#' df$cyl <- as.factor(df$cyl)
#'
#' # Scatter plot with ellipses and group mean points
#' ggscatter(df, x = "wt", y = "mpg", color = "cyl", ellipse = TRUE)+
#'  stat_mean(aes(color = cyl, shape = cyl))
#'
#' @export
stat_mean <- function(mapping = NULL, data = NULL, geom = "point",
                    position = "identity", na.rm = FALSE, show.legend = NA,
                    inherit.aes = TRUE, ...) {
  layer(
    stat = StatMean, data = data, mapping = mapping, geom = geom,
    position = position, show.legend = show.legend, inherit.aes = inherit.aes,
    params = list(na.rm = na.rm, ...)
  )
}


StatMean<- ggproto("StatMean", Stat,
                  required_aes = c("x", "y"),
                  compute_group = function(data, scales) {
                    res <- data.frame(x = mean(data$x, na.rm = TRUE),
                                      y = mean(data$y, na.rm = TRUE))
                    res
                  }
)
