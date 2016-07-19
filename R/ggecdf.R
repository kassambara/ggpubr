#' @include utilities.R ggpar.R
NULL
#' Empirical cumulative density function
#' @description Empirical Cumulative Density Function (ECDF).
#' @inheritParams ggboxplot
#' @param x variable to be drawn.
#' @param color line and point color.
#' @param linetype line type. See \code{\link{show_line_types}}.
#' @param size line and point size.
#' @param ... other arguments to be passed to
#'   \code{\link[ggplot2]{stat_ecdf}} and \code{\link{ggpar}}.
#' @details The plot can be easily customized using the function ggpar(). Read
#'   ?ggpar for changing: \itemize{ \item main title and axis labels: main,
#'   xlab, ylab \item axis limits: xlim, ylim (e.g.: ylim = c(0, 30)) \item axis
#'   scales: xscale, yscale (e.g.: yscale = "log2") \item color palettes:
#'   palette = "Dark2" or palette = c("gray", "blue", "red") \item legend title,
#'   labels and position: legend = "right" \item plot orientation : orientation
#'   = c("vertical", "horizontal", "reverse") }
#' @seealso \code{\link{ggpar}}
#'
#' @examples
#' # Create some data format
#' set.seed(1234)
#' wdata = data.frame(
#'    sex = factor(rep(c("F", "M"), each=200)),
#'    weight = c(rnorm(200, 55), rnorm(200, 58)))
#'
#' head(wdata, 4)
#'
#' # Basic ECDF plot
#' ggecdf(wdata, x = "weight")
#'
#' # Change colors and linetype by groups ("sex")
#' # Use custom palette
#' ggecdf(wdata, x = "weight",
#'    color = "sex", linetype = "sex",
#'    palette = c("#00AFBB", "#E7B800"))
#'
#' @export
ggecdf <- function(data, x,
                      color = "black",  palette = NULL,
                      size = 1, linetype = "solid",
                      ggtheme = theme_pubr(),
                      ...)
{


  p <- ggplot(data, aes_string(x))

  p <- p +
      .geom_exec(stat_ecdf, data = data,
                 color = color,  size = size,
                 linetype = linetype, ...)+
    labs(y = paste0("F(", x, ")"))
  p <- ggpar(p, palette = palette, ggtheme = ggtheme, ...)
  p
}



