#' @include utilities.R geom_violin_half.R
NULL
#' Raincloud Plot
#' @description Draws a raincloud plot: a half violin (the \dQuote{cloud}) showing
#'   the distribution, a narrow box plot, and the raw jittered data points (the
#'   \dQuote{rain}) offset to the opposite side. Rainclouds combine the summary of
#'   a box plot, the distribution shape of a violin, and the raw data in one
#'   publication-ready figure.
#' @details The half violin is drawn with \code{\link{geom_violin_half}()}. The
#'   raincloud idiom follows Allen et al. (2019); prior implementations exist in
#'   the \code{ggdist}, \code{gghalves} and \code{ggrain} packages.
#' @inheritParams ggboxplot
#' @param data a data frame.
#' @param x,y x and y variables, where \code{x} is a grouping variable and
#'   \code{y} is numeric.
#' @param color the color of the rain (jittered) points. If \code{NULL}
#'   (default), it follows \code{fill} (so a single \code{fill} gives a
#'   monochrome raincloud and a by-group \code{fill} keeps the rain colored by
#'   group). Use \code{"x"} to color by the \code{x} group, or a single color
#'   (e.g. \code{"black"}) for all points. The box outline is always black.
#' @param fill the fill color of the cloud (half violin) and the box. If
#'   \code{NULL} (default) or \code{"x"}, filled by the \code{x} group;
#'   otherwise a single fill color (e.g. \code{"#00AFBB"}).
#' @param palette the color palette to be used for coloring or filling by groups.
#' @param alpha transparency of the cloud and box fill. Default 0.7.
#' @param width the width of the half violin (cloud). Default 0.9.
#' @param boxplot.width the width of the box plot. Default 0.1.
#' @param jitter.width the amount of horizontal jitter for the rain points.
#'   Default 0.06.
#' @param jitter.size the size of the rain points. Default 1.2.
#' @param side the side on which the cloud is drawn, \code{"right"} (default) or
#'   \code{"left"}; the rain falls on the opposite side.
#' @param flip logical. If \code{TRUE} (default), the plot is drawn horizontally
#'   (the classic raincloud orientation, via \code{\link[ggplot2]{coord_flip}()}).
#' @param ggtheme a ggplot theme. Default is \code{\link{theme_pubr}()}.
#' @param facet.by character vector of up to two grouping variables for faceting.
#' @param ... other arguments passed to \code{\link{ggpar}()}.
#' @return a ggplot.
#' @references Allen, M., Poggiali, D., Whitaker, K., Marshall, T. R., & Kievit,
#'   R. A. (2019). Raincloud plots: a multi-platform tool for robust data
#'   visualization. Wellcome Open Research, 4, 63.
#' @seealso \code{\link{geom_violin_half}}, \code{\link{ggviolin}}.
#' @examples
#' df <- ToothGrowth
#' df$dose <- as.factor(df$dose)
#'
#' # Basic raincloud, colored by group
#' ggraincloud(df, x = "dose", y = "len")
#'
#' # A single fill color, vertical orientation
#' ggraincloud(df, x = "dose", y = "len", fill = "#00AFBB", flip = FALSE)
#'
#' # Journal palette + faceting
#' ggraincloud(df, x = "supp", y = "len", palette = "jco", facet.by = "dose")
#'
#' @export
ggraincloud <- function(data, x, y,
                        color = NULL, fill = NULL, palette = NULL,
                        alpha = 0.7, width = 0.9,
                        boxplot.width = 0.1,
                        jitter.width = 0.06, jitter.size = 1.2,
                        side = c("right", "left"), flip = TRUE,
                        title = NULL, xlab = NULL, ylab = NULL,
                        ggtheme = theme_pubr(), facet.by = NULL,
                        labeller = "label_value", ...) {
  side <- match.arg(side)
  if (!is.factor(data[[x]])) data[[x]] <- factor(data[[x]])

  if (!is.null(color) && length(color) != 1) {
    stop("`color` must be a single value.", call. = FALSE)
  }
  if (!is.null(fill) && length(fill) != 1) {
    stop("`fill` must be a single value.", call. = FALSE)
  }
  # `fill` colours the cloud and box; `color` colours the rain points. The
  # sentinel "x" (or `fill = NULL`) means "by the x group". `color = NULL`
  # (default) follows `fill`, so a single fill yields a monochrome raincloud
  # while a by-group fill keeps the rain coloured by group.
  if (is.null(fill) || identical(fill, "x")) fill <- x
  if (is.null(color)) {
    color <- fill
  } else if (identical(color, "x")) {
    color <- x
  }
  cloud.nudge <- if (side == "right") boxplot.width else -boxplot.width
  rain.offset <- if (side == "right") -0.20 else 0.20

  # The rain points are placed at the numeric group position, offset to the side
  # opposite the cloud. A discrete x scale maps the factor to 1, 2, 3, ..., so the
  # offset numeric positions sit alongside each group.
  data[[".ggpubr.rain.x."]] <- as.numeric(data[[x]]) + rain.offset

  fill.by.group <- fill %in% colnames(data)
  color.by.group <- color %in% colnames(data)

  aes.main <- create_aes(list(x = x, y = y))
  if (fill.by.group) aes.main$fill <- as.name(fill)
  p <- ggplot(data, aes.main)

  # Cloud: half violin on `side`, its flat edge nudged to the group centre + box.
  # `fill` is only passed as a fixed value when it is NOT mapped from the data
  # (passing it when mapped would blank the layer). The cloud has no outline.
  cloud.args <- list(
    side = side, position = position_nudge(x = cloud.nudge),
    trim = FALSE, width = width, alpha = alpha, colour = NA
  )
  if (!fill.by.group) cloud.args$fill <- fill
  p <- p + do.call(geom_violin_half, cloud.args)

  # Box: narrow, at the cloud's flat edge, with a black outline for contrast.
  box.args <- list(
    position = position_nudge(x = cloud.nudge),
    width = boxplot.width, alpha = alpha, outlier.shape = NA, colour = "black"
  )
  if (!fill.by.group) box.args$fill <- fill
  p <- p + do.call(geom_boxplot, box.args)

  # Rain: raw points offset to the opposite side, coloured by `color`.
  aes.rain <- create_aes(list(x = ".ggpubr.rain.x.", y = y))
  if (color.by.group) aes.rain$colour <- as.name(color)
  rain.args <- list(
    mapping = aes.rain,
    width = jitter.width, height = 0, size = jitter.size, alpha = alpha
  )
  if (!color.by.group) rain.args$colour <- color
  p <- p + do.call(geom_jitter, rain.args)

  # Apply the horizontal orientation through ggpar(orientation=) rather than a
  # raw coord_flip(): ggpar routes xlim/ylim through coord_flip for the
  # horizontal case, so a limit passed via `...` does not silently replace the
  # coordinate system and drop the flip (#646).
  p <- ggpar(
    p, palette = palette, ggtheme = ggtheme,
    orientation = if (flip) "horizontal" else "vertical",
    title = title, xlab = xlab %||% x, ylab = ylab %||% y, ...
  )
  if (fill.by.group || color.by.group) {
    p <- p + theme(legend.position = "none")
  }
  if (!is.null(facet.by)) {
    p <- facet(p, facet.by = facet.by, labeller = labeller, ...)
  }
  p
}
