#' @include utilities.R
NULL
#' Add Stars to a Scatter Plot
#' @description Create a star plot by drawing segments from group centroid to each points.
#' @inheritParams ggpubr-common-params
#' @inheritParams ggplot2::layer
#' @param ... other arguments to pass to \code{\link[ggplot2]{geom_segment}}.
#' @param na.rm If FALSE (the default), removes missing values with a warning.
#'   If TRUE silently removes missing values.
#' @seealso \code{\link{ggscatter}}
#' @examples
#' # Load data
#' data("mtcars")
#' df <- mtcars
#' df$cyl <- as.factor(df$cyl)
#'
#' # Scatter plot with ellipses and group mean points
#' ggscatter(df, x = "wt", y = "mpg",
#'    color = "cyl", shape = "cyl",
#'    mean.point = TRUE, ellipse = TRUE)+
#'  stat_stars(aes(color = cyl))
#'
#' @export
stat_stars <- function(mapping = NULL, data = NULL, geom = "segment",
                    position = "identity",  na.rm = FALSE, show.legend = NA,
                    inherit.aes = TRUE, ...) {
  layer(
    stat = StatStars, data = data, mapping = mapping, geom = geom,
    position = position, show.legend = show.legend, inherit.aes = inherit.aes,
    params = list(na.rm = na.rm, ...)
  )
}


StatStars<- ggproto("StatStars", Stat,
                  required_aes = c("x", "y"),
                  compute_group = function(data, scales) {
                    x.mean <- mean(data$x, na.rm = TRUE)
                    y.mean <- mean(data$y, na.rm = TRUE)
                    res <- data %>% mutate(x = x.mean, y = y.mean)
                    res$xend <- data$x
                    res$yend <- data$y
                    res
                  }
)
