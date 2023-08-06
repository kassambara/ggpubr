#' @include utilities.R
NULL
#' Plot confidence ellipses.
#' @description Plot confidence ellipses around barycenters. The method for
#'   computing confidence ellipses has been modified from \code{FactoMineR::coord.ellipse()}.
#' @inheritParams ggplot2::layer
#' @inheritParams ggplot2::stat_ellipse
#' @param level confidence level used to construct the ellipses. By
#'   default, 0.95.
#' @param npoint number of points used to draw the ellipses.
#' @param bary logical value. If TRUE, the coordinates of the ellipse around the
#'   barycentre of individuals are calculated.
#' @seealso \code{\link{stat_conf_ellipse}}
#' @examples
#' # Load data
#' data("mtcars")
#' df <- mtcars
#' df$cyl <- as.factor(df$cyl)
#'
#' # scatter plot with confidence ellipses
#' ggscatter(df, x = "wt", y = "mpg", color = "cyl")+
#'  stat_conf_ellipse(aes(color = cyl))
#'
#' ggscatter(df, x = "wt", y = "mpg", color = "cyl")+
#'  stat_conf_ellipse(aes(color = cyl, fill = cyl), alpha = 0.1, geom = "polygon")
#'
#' @export
stat_conf_ellipse <- function(mapping = NULL, data = NULL, geom = "path",
                    position = "identity", na.rm = FALSE, show.legend = NA,
                    inherit.aes = TRUE, level = 0.95, npoint = 100, bary = TRUE,
                    ...) {
  layer(
    stat = StatConfEllipse, data = data, mapping = mapping, geom = geom,
    position = position, show.legend = show.legend, inherit.aes = inherit.aes,
    params = list(na.rm = na.rm,  level = level, npoint = npoint, bary = bary, ...)
  )
}


StatConfEllipse <- ggproto("StatConfEllipse", Stat,
                   required_aes = c("x", "y"),

                  compute_group = function(data, scales, params, level = 0.95,
                                           npoint = 100, bary = TRUE) {

                    .coord_ellipse (data$x, data$y, level = level,
                                                npoint = npoint, bary = bary)
                  }
)

# Compute confidence ellipses.
#  x,y x and y variables for drawing.
#  level confidence level used to construct the ellipses. By
#   default, 0.95.
#  npoint number of points used to draw the ellipses.
#  bary logical value. If TRUE, the coordinates of the ellipse around the
#   barycentre of individuals are calculated.
.coord_ellipse <- function ( x, y, level = 0.95,
          npoint = 100, bary = FALSE)
{

  .ellipse <- function(x, scale = c(1, 1), centre = c(0, 0),
                      level = 0.95, t = sqrt(stats::qchisq(level, 2)), which = c(1,
                                                                          2), npoints = 100) {
    names <- c("x", "y")
    if (is.matrix(x)) {
      xind <- which[1]
      yind <- which[2]
      r <- x[xind, yind]
      if (missing(scale)) {
        scale <- sqrt(c(x[xind, xind], x[yind, yind]))
        if (scale[1] > 0)
          r <- r/scale[1]
        if (scale[2] > 0)
          r <- r/scale[2]
      }
      if (!is.null(dimnames(x)[[1]]))
        names <- dimnames(x)[[1]][c(xind, yind)]
    }
    else r <- x
    r <- min(max(r, -1), 1)
    d <- acos(r)
    a <- seq(0, 2 * pi, len = npoints)
    matrix(c(t * scale[1] * cos(a + d/2) + centre[1], t *
               scale[2] * cos(a - d/2) + centre[2]), npoints, 2,
           dimnames = list(NULL, names))
  }


  center <- c(mean(x, na.rm = TRUE), mean(y, na.rm = TRUE))
  tab <- data.frame(x = x, y = y)
  mat.cov <- stats::cov(tab)
  if (bary)
  mat.cov = mat.cov/nrow(tab)
  res <- .ellipse(mat.cov, centre = center, level = level, npoints = npoint)
  return(data.frame(res, stringsAsFactors = FALSE))
}
