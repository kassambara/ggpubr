#' @include utilities.R
NULL
#' Point shapes available in R
#' @description Show point shapes available in R.
#' @return a ggplot.
#'
#' @seealso \code{\link{ggpar}} and \code{\link{ggline}}.
#' @examples
#' show_point_shapes()+
#'  theme_minimal()
#' @export
show_point_shapes <- function()
{
  d=data.frame(p=c(0:25))
  p <- ggplot() +
    scale_y_continuous(name="") +
    scale_x_continuous(name="") +
    scale_shape_identity() +
    geom_point(data=d, mapping=aes(x=p%%6, y=p%/%6, shape=p), size=5, fill="blue") +
    geom_text(data=d, mapping=aes(x=p%%6, y=p%/%6+0.25, label=p), size=3)
  ggpar(p, ticks = FALSE, tickslab = FALSE,
        main = "Point shapes available in R")+
    theme_gray()
}


