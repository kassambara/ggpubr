#' ggpubr General Arguments Description
#'
#' @description A reference list of the general arguments shared across the
#'   \code{ggpubr} plotting functions. These are documented on each individual
#'   function; this page gathers them in one place. It is a reference page only
#'   (no function to call).
#'
#' @section General arguments:
#' \describe{
#'   \item{\code{data}}{a data frame}
#'   \item{\code{x}}{character string containing the name of x variable.}
#'   \item{\code{y}}{character vector containing one or more variables to plot}
#'   \item{\code{combine}}{logical value. Default is FALSE. Used only when y is a
#'     vector containing multiple variables to plot. If TRUE, create a
#'     multi-panel plot by combining the plot of y variables.}
#'   \item{\code{merge}}{logical or character value. Default is FALSE. Used only
#'     when y is a vector containing multiple variables to plot. If TRUE, merge
#'     multiple y variables in the same plotting area. Allowed values include
#'     also "asis" (TRUE) and "flip". If merge = "flip", then y variables are
#'     used as x tick labels and the x variable is used as grouping variable.}
#'   \item{\code{color}}{outline color.}
#'   \item{\code{fill}}{fill color.}
#'   \item{\code{palette}}{the color palette to be used for coloring or filling
#'     by groups. Allowed values include "grey" for grey color palettes; brewer
#'     palettes e.g. "RdBu", "Blues", ...; or custom color palette e.g.
#'     c("blue", "red"); and scientific journal palettes from ggsci R package,
#'     e.g.: "npg", "aaas", "lancet", "jco", "ucscgb", "uchicago", "simpsons"
#'     and "rickandmorty".}
#'   \item{\code{linetype}}{line types.}
#'   \item{\code{size}}{Numeric value (e.g.: size = 1). change the size of points
#'     and outlines.}
#'   \item{\code{select}}{character vector specifying which items to display.}
#'   \item{\code{remove}}{character vector specifying which items to remove from
#'     the plot.}
#'   \item{\code{order}}{character vector specifying the order of items.}
#'   \item{\code{add}}{character vector for adding another plot element (e.g.: dot
#'     plot or error bars). Allowed values are one or the combination of: "none",
#'     "dotplot", "jitter", "boxplot", "point", "mean", "mean_se", "mean_sd",
#'     "mean_ci", "mean_range", "median", "median_iqr", "median_mad",
#'     "median_range"; see ?desc_statby for more details.}
#'   \item{\code{add.params}}{parameters (color, shape, size, fill, linetype) for
#'     the argument 'add'; e.g.: add.params = list(color = "red").}
#'   \item{\code{error.plot}}{plot type used to visualize error. Allowed values
#'     are one of c("pointrange", "linerange", "crossbar", "errorbar",
#'     "upper_errorbar", "lower_errorbar", "upper_pointrange", "lower_pointrange",
#'     "upper_linerange", "lower_linerange"). Default value is "pointrange" or
#'     "errorbar". Used only when add != "none" and add contains one "mean_*" or
#'     "med_*" where "*" = sd, se, ....}
#'   \item{\code{font.label}}{a list which can contain the combination of the
#'     following elements: the size (e.g.: 14), the style (e.g.: "plain", "bold",
#'     "italic", "bold.italic") and the color (e.g.: "red") of labels. For
#'     example font.label = list(size = 14, face = "bold", color ="red"). To
#'     specify only the size and the style, use font.label = list(size = 14,
#'     face = "plain").}
#'   \item{\code{title}}{plot main title.}
#'   \item{\code{xlab}}{character vector specifying x axis labels. Use xlab =
#'     FALSE to hide xlab.}
#'   \item{\code{ylab}}{character vector specifying y axis labels. Use ylab =
#'     FALSE to hide ylab.}
#'   \item{\code{ggtheme}}{function, ggplot2 theme name. Default value is
#'     theme_pubr(). Set ggtheme = NULL to skip applying a ggpubr theme, so the
#'     plot keeps ggplot2 default theme or the theme set globally via
#'     theme_set(). Allowed values include ggplot2 official themes: theme_gray(),
#'     theme_bw(), theme_minimal(), theme_classic(), theme_void(), ....}
#' }
#' @name ggpubr_args
#' @rdname ggpubr_args
NULL
