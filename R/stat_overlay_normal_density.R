#' @include utilities.R
NULL
#' Overlay Normal Density Plot
#'
#' @description Overlay normal density plot (with the same mean and SD) to the
#'   density distribution of 'x'. This is useful for visually inspecting the
#'   degree of deviance from normality.
#' @inheritParams ggplot2::layer
#' @param ... other arguments to pass to \code{\link[ggplot2]{geom_line}}.
#' @param na.rm If FALSE (the default), removes missing values with a warning.
#'   If TRUE silently removes missing values.
#' @seealso \code{\link{ggdensity}}
#' @examples
#' # Load data
#' data("mtcars")
#' ggdensity(mtcars, x = "mpg", fill = "red") +
#'   stat_overlay_normal_density(color = "red", linetype = 2)
#'
#' @export
stat_overlay_normal_density <- function(mapping = NULL, data = NULL, geom = "line",
                    position = "identity", na.rm = FALSE, show.legend = NA,
                    inherit.aes = TRUE, ...) {
  if(is.null(mapping)){
    mapping <- ggplot2::aes(y = NULL)
  }else{
    mapping["y"] <- list(NULL)
  }
  layer(
    stat = StatOverlayNormalDensity, data = data, mapping = mapping, geom = geom,
    position = position, show.legend = show.legend, inherit.aes = inherit.aes,
    params = list(na.rm = na.rm, ...)
  )
}


StatOverlayNormalDensity<- ggproto("StatOverlayNormalDensity", Stat,
                  required_aes = c("x"),
                  compute_group = function(data, scales) {
                    x <- data$x
                    .mean <- mean(x, na.rm = TRUE)
                    .sd <- stats::sd(x, na.rm = TRUE)
                    probability.points <- stats::ppoints(length(x[!(is.na(x))]))
                    res.density <- stats::density(stats::qnorm(probability.points, .mean, .sd))
                    res.density <- data.frame(x = res.density$x, y = res.density$y)
                    res.density
                  }
)
