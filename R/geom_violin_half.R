#' @include utilities.R
NULL
#' Half Violin Plot
#' @description Draws a one-sided (half) violin plot: the density is drawn on a
#'   single side of each group position, with a flat edge at the center. This is
#'   the building block of raincloud plots (see \code{\link{ggraincloud}()}),
#'   where the half violin is the \dQuote{cloud}.
#' @details ggplot2 has no built-in half violin, so \code{GeomViolinHalf}
#'   extends \code{\link[ggplot2]{geom_violin}()} and keeps only one side of the
#'   density polygon. The idea of the half (\dQuote{flat}) violin comes from the
#'   raincloud-plot literature (Allen et al., 2019) and prior implementations in
#'   the \code{ggdist} and \code{gghalves} packages.
#' @param mapping Set of aesthetic mappings created by \code{\link[ggplot2]{aes}()}.
#' @param data The data to be displayed in this layer.
#' @param stat the statistical transformation to use on the data. Default
#'   \code{"ydensity"}.
#' @param position Position adjustment, e.g. \code{\link[ggplot2]{position_nudge}()}.
#' @param side the side on which the density is drawn, \code{"left"} (default) or
#'   \code{"right"}.
#' @param trim if \code{TRUE}, trim the tails of the violins to the range of the
#'   data. Default is \code{TRUE}.
#' @param scale passed to \code{\link[ggplot2]{geom_violin}()}.
#' @param na.rm If \code{FALSE} (default), missing values are removed with a
#'   warning.
#' @param show.legend logical. Should this layer be included in the legends?
#' @param inherit.aes If \code{FALSE}, overrides the default aesthetics.
#' @param ... other arguments passed to \code{\link[ggplot2]{layer}()}.
#' @references Allen, M., Poggiali, D., Whitaker, K., Marshall, T. R., & Kievit,
#'   R. A. (2019). Raincloud plots: a multi-platform tool for robust data
#'   visualization. Wellcome Open Research, 4, 63.
#' @seealso \code{\link{ggraincloud}}, \code{\link{ggviolin}}.
#' @examples
#' df <- ToothGrowth
#' df$dose <- as.factor(df$dose)
#' library(ggplot2)
#' ggplot(df, aes(dose, len)) +
#'   geom_violin_half(side = "right", fill = "#00AFBB")
#'
#' @rdname geom_violin_half
#' @export
GeomViolinHalf <- ggplot2::ggproto("GeomViolinHalf", ggplot2::GeomViolin,
  extra_params = c("na.rm", "side"),
  setup_data = function(self, data, params) {
    data <- ggplot2::ggproto_parent(ggplot2::GeomViolin, self)$setup_data(data, params)
    side <- params$side %||% "left"
    # Flatten one edge of the density polygon to the group centre so only one
    # side is drawn. geom_violin's draw_group builds the polygon from
    # xminv = x - violinwidth*(x - xmin) up to xmaxv = x + violinwidth*(xmax - x);
    # setting xmin (or xmax) to x collapses that edge onto the centre line.
    if (identical(side, "right")) {
      data$xmin <- data$x
    } else {
      data$xmax <- data$x
    }
    data
  }
)

#' @rdname geom_violin_half
#' @export
geom_violin_half <- function(mapping = NULL, data = NULL, stat = "ydensity",
                             position = "dodge", ..., side = "left",
                             trim = TRUE, scale = "area", na.rm = FALSE,
                             show.legend = NA, inherit.aes = TRUE) {
  side <- match.arg(side, c("left", "right"))
  ggplot2::layer(
    data = data, mapping = mapping, stat = stat, geom = GeomViolinHalf,
    position = position, show.legend = show.legend, inherit.aes = inherit.aes,
    params = list(
      trim = trim, scale = scale, side = side, na.rm = na.rm, ...
    )
  )
}
