#' @include utilities.R ggpar.R
NULL
#' QQ Plots
#' @description Quantile-Quantile plot.
#' @inheritParams ggboxplot
#' @param x variable to be drawn.
#' @param color point color.
#' @param size point size.
#' @param shape point shape.
#' @param add character vector. Allowed values are one of "none" and "qqline"
#'   (for adding qqline).
#' @param add.params parameters (color, size,  linetype) for the
#'   argument 'add'; e.g.: add.params = list(color = "red").
#' @param conf.int logical value. If TRUE, confidence interval is added.
#' @param conf.int.level the confidence level. Default value is 0.95.
#' @param ... other arguments to be passed to \code{\link{ggpar}}.
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
#' # Basic QQ plot
#' ggqqplot(wdata, x = "weight")
#'
#' # Change colors and shape by groups ("sex")
#' # Use custom palette
#' ggqqplot(wdata, x = "weight",
#'    color = "sex", palette = c("#00AFBB", "#E7B800"))
#'
#' @export
ggqqplot <- function(data, x, combine = FALSE, merge = FALSE,
                     color = "black",  palette = NULL,
                     size = NULL, shape = NULL,
                     add = c( "qqline", "none"),
                     add.params = list(linetype = "solid"),
                     conf.int = TRUE, conf.int.level = 0.95,
                     title = NULL, xlab = NULL, ylab = NULL,
                     facet.by = NULL, panel.labs = NULL, short.panel.labs = TRUE,
                     ggtheme = theme_pubr(),
                     ...)
{

  # Default options
  #:::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::
  .opts <- list(
    combine = combine, merge = merge,
    color = color, palette = palette,
    size = size, shape = shape,
    title = title, xlab = xlab, ylab = ylab,
    facet.by = facet.by, panel.labs = panel.labs, short.panel.labs = short.panel.labs,
    conf.int.level = conf.int.level,
    ggtheme = ggtheme, ...)
  if(!missing(data)) .opts$data <- data
  if(!missing(x)) .opts$x <- x

  # User options
  #:::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::
  .user.opts <- as.list(match.call(expand.dots = TRUE))
  .user.opts[[1]] <- NULL # Remove the function name
  # keep only user arguments
  for(opt.name in names(.opts)){
    if(is.null(.user.opts[[opt.name]]))
      .opts[[opt.name]] <- NULL
  }
  .opts$fun <- ggqqplot_core
  .opts$y <- "..qq.."
  if(is.null(.opts$xlab)) .opts$xlab <- "Theoretical"
  if(is.null(.opts$ylab)) .opts$ylab <- "Sample"
  if(is.null(.opts$add)) .opts$add <- "qqline"
  if(missing(ggtheme) & (!is.null(facet.by) | combine))
    .opts$ggtheme <- theme_pubr(border = TRUE)
  p <- do.call(.plotter, .opts)

  if(.is_list(p) & length(p) == 1) p <- p[[1]]
  return(p)


}

ggqqplot_core <- function(data, x, y = "..qq..",
                      color = "black",  palette = NULL,
                      size = NULL, shape = NULL, fill = "white",
                      add = c( "qqline", "none"),
                      add.params = list(linetype = "solid"),
                      conf.int = TRUE, conf.int.level = 0.95,
                      title = NULL, xlab = NULL, ylab = NULL,
                      ggtheme = theme_pubr(),
                      ...)
{

  # Check data
  .dd <- .check_data(data, x, y=NULL)
  data <- .dd$data
  x <- .dd$x
  y <- .dd$y

  group <- c(shape, color) %>% unique() %>%
    intersect(colnames(data))
  group <- ifelse(.is_empty(group), 1, group[1])

  add <- match.arg(add)
  add.params <- .check_add.params(add, add.params, error.plot = "", data, color, fill = fill, ...)
  if(is.null(add.params$size)) add.params$size <- size
  if(is.null(add.params$linetype)) add.params$linetype <- "solid"

  p <- ggplot(data, create_aes(list(sample = x)))

  p <- p +
      geom_exec(stat_qq, data = data,
                 color = color,  size = size, shape = shape)


  if ("qqline" %in% add) p <- p +
     geom_exec(.stat_qqline, data = data,
               color = add.params$color,  size = add.params$size,
               linetype = add.params$linetype, group = group)

  # Confidence interval
  if(conf.int){
    p <- p + geom_exec(.stat_qq_confint, data = data,
               fill = color, alpha = 0.2, conf.int.level = conf.int.level,
               group = group)
  }

  p <- ggpar(p, palette = palette, ggtheme = ggtheme,
             title = title, xlab = xlab, ylab = ylab,...)
  p
}



# Helper functions
########################
# from: http://stackoverflow.com/questions/4357031/qqnorm-and-qqline-in-ggplot2
# qf : qfunction (e.g.: qnorm)
.qq_line <- function(data, qf, na.rm) {
  q.sample <- stats::quantile(data, c(0.25, 0.75), na.rm = na.rm)
  q.theory <- qf(c(0.25, 0.75))
  slope <- diff(q.sample) / diff(q.theory)
  intercept <- q.sample[1] - slope * q.theory[1]
  list(slope = slope, intercept = intercept)

}


StatQQLine <- ggproto("StatQQLine", Stat,
                      # http://docs.ggplot2.org/current/vignettes/extending-ggplot2.html
                      # https://github.com/hadley/ggplot2/blob/master/R/stat-qq.r
                      required_aes = c('sample'),
                      compute_group = function(data, scales,
                                               distribution = stats::qnorm,
                                               dparams = list(),
                                               conf.int.level = 0.95,
                                               na.rm = FALSE) {
                        qf <- function(p) do.call(distribution, c(list(p = p), dparams))
                        n <- length(data$sample)
                        P <- stats::ppoints(n)
                        theoretical <- qf(P)
                        qq <- .qq_line(data$sample, qf = qf, na.rm = na.rm)
                        line <- qq$intercept + theoretical * qq$slope
                        # Confidence interval
                        zz <- stats::qnorm(1 - (1 - conf.int.level)/2)
                        SE <- (qq$slope/stats::dnorm(theoretical)) * sqrt(P * (1 - P)/n)
                        fit.value <- qq$intercept + qq$slope * theoretical
                        ymax <- fit.value + zz * SE
                        ymin <- fit.value - zz * SE
                        data.frame(sample = line, x = theoretical, y = line, ymin = ymin, ymax = ymax)
                      }
)

.stat_qqline <- function(mapping = NULL, data = NULL, geom = "line",
                        position = "identity", ...,
                        distribution = stats::qnorm,
                        dparams = list(),
                        na.rm = FALSE,
                        show.legend = NA,
                        inherit.aes = TRUE,
                        conf.int.level = 0.95) {
  layer(stat = StatQQLine, data = data, mapping = mapping, geom = geom,
        position = position, show.legend = show.legend, inherit.aes = inherit.aes,
        params = list(distribution = distribution,
                      dparams = dparams,
                      na.rm = na.rm, conf.int.level = conf.int.level, ...))
}


.stat_qq_confint <- function(mapping = NULL, data = NULL, geom = "ribbon",
                         position = "identity", ...,
                         distribution = stats::qnorm,
                         dparams = list(),
                         na.rm = FALSE,
                         show.legend = NA,
                         inherit.aes = TRUE,
                         conf.int.level = 0.95) {
  layer(stat = StatQQLine, data = data, mapping = mapping, geom = geom,
        position = position, show.legend = show.legend, inherit.aes = inherit.aes,
        params = list(distribution = distribution,
                      dparams = dparams,
                      na.rm = na.rm, conf.int.level = conf.int.level, ...))
}



