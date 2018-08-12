#' @include utilities.R ggpar.R stat_chull.R stat_conf_ellipse.R stat_stars.R stat_cor.R
NULL
#' Scatter plot
#' @description Create a scatter plot.
#' @inheritParams ggboxplot
#' @inheritParams facet
#' @inheritParams ggpar
#' @param x,y x and y variables for drawing.
#' @param color,fill point colors.
#' @param shape point shape. See \code{\link{show_point_shapes}}.
#' @param point logical value. If TRUE, show points.
#' @param rug logical value. If TRUE, add marginal rug.
#' @param add allowed values are one of "none", "reg.line" (for adding linear
#'   regression line) or "loess" (for adding local regression fitting).
#' @param add.params parameters (color, size, linetype) for the argument 'add';
#'   e.g.: add.params = list(color = "red").
#' @param conf.int logical value. If TRUE, adds confidence interval.
#' @param conf.int.level Level controlling confidence region. Default is 95\%.
#'   Used only when add != "none" and conf.int = TRUE.
#' @param fullrange should the fit span the full range of the plot, or just the
#'   data. Used only when add != "none".
#' @param ellipse logical value. If TRUE, draws ellipses around points.
#' @param ellipse.level the size of the concentration ellipse in normal
#'   probability.
#' @param ellipse.type Character specifying frame type. Possible values are
#'  \code{"convex"}, \code{"confidence"} or types supported by
#'  \code{\link[ggplot2]{stat_ellipse}()} including one of \code{c("t", "norm",
#'  "euclid")} for plotting concentration ellipses.
#'
#'  \itemize{ \item \code{"convex"}: plot convex hull of a set o points. \item
#'  \code{"confidence"}: plot confidence ellipses arround group mean points as
#'  \code{\link[FactoMineR]{coord.ellipse}()}[in FactoMineR]. \item \code{"t"}:
#'  assumes a multivariate t-distribution. \item \code{"norm"}: assumes a
#'  multivariate normal distribution. \item \code{"euclid"}: draws a circle with
#'  the radius equal to level, representing the euclidean distance from the
#'  center. This ellipse probably won't appear circular unless
#'  \code{\link[ggplot2]{coord_fixed}()} is applied.}
#' @param ellipse.alpha Alpha for ellipse specifying the transparency level of
#'   fill color. Use alpha = 0 for no fill color.
#' @param ellipse.border.remove logical value. If TRUE, remove ellipse border lines.
#' @param mean.point logical value. If TRUE, group mean points are added to the
#'   plot.
#' @param mean.point.size numeric value specifying the size of mean points.
#' @param star.plot logical value. If TRUE, a star plot is generated.
#' @param star.plot.lty,star.plot.lwd line type and line width (size) for star
#'   plot, respectively.
#' @param label the name of the column containing point labels. Can be also a
#'   character vector with length = nrow(data).
#' @param font.label a vector of length 3 indicating respectively the size
#'   (e.g.: 14), the style (e.g.: "plain", "bold", "italic", "bold.italic") and
#'   the color (e.g.: "red") of point labels. For example \emph{font.label =
#'   c(14, "bold", "red")}. To specify only the size and the style, use
#'   font.label = c(14, "plain").
#' @param font.family character vector specifying font family.
#' @param label.select character vector specifying some labels to show.
#' @param repel a logical value, whether to use ggrepel to avoid overplotting
#'   text labels or not.
#' @param label.rectangle logical value. If TRUE, add rectangle underneath the
#'   text, making it easier to read.
#' @param cor.coef logical value. If TRUE, correlation coefficient with the
#'   p-value will be added to the plot.
#' @param cor.coeff.args a list of arguments to pass to the function
#'   \code{\link{stat_cor}} for customizing the displayed correlation
#'   coefficients. For example: \code{cor.coeff.args = list(method = "pearson",
#'   label.x.npc = "right", label.y.npc = "top")}.
#' @param cor.method method for computing correlation coefficient. Allowed
#'   values are one of "pearson", "kendall", or "spearman".
#' @param cor.coef.coord numeric vector, of length 2, specifying the x and y
#'   coordinates of the correlation coefficient. Default values are NULL.
#' @param cor.coef.size correlation coefficient text font size.
#' @param ggp a ggplot. If not NULL, points are added to an existing plot.
#' @param show.legend.text logical. Should text be included in the legends? NA,
#'   the default, includes if any aesthetics are mapped. FALSE never includes,
#'   and TRUE always includes.
#' @param ... other arguments to be passed to \code{\link[ggplot2]{geom_point}}
#'   and \code{\link{ggpar}}.
#' @details The plot can be easily customized using the function ggpar(). Read
#'   ?ggpar for changing: \itemize{ \item main title and axis labels: main,
#'   xlab, ylab \item axis limits: xlim, ylim (e.g.: ylim = c(0, 30)) \item axis
#'   scales: xscale, yscale (e.g.: yscale = "log2") \item color palettes:
#'   palette = "Dark2" or palette = c("gray", "blue", "red") \item legend title,
#'   labels and position: legend = "right" \item plot orientation : orientation
#'   = c("vertical", "horizontal", "reverse") }
#' @seealso \code{\link{stat_cor}}, \code{\link{stat_stars}}, \code{\link{stat_conf_ellipse}} and \code{\link{ggpar}}.
#' @examples
#' # Load data
#' data("mtcars")
#' df <- mtcars
#' df$cyl <- as.factor(df$cyl)
#' head(df[, c("wt", "mpg", "cyl")], 3)
#'
#' # Basic plot
#' # +++++++++++++++++++++++++++
#' ggscatter(df, x = "wt", y = "mpg",
#'    color = "black", shape = 21, size = 3, # Points color, shape and size
#'    add = "reg.line",  # Add regressin line
#'    add.params = list(color = "blue", fill = "lightgray"), # Customize reg. line
#'    conf.int = TRUE, # Add confidence interval
#'    cor.coef = TRUE, # Add correlation coefficient. see ?stat_cor
#'    cor.coeff.args = list(method = "pearson", label.x = 3, label.sep = "\n")
#'    )
#'
#' # loess method: local regression fitting
#' ggscatter(df, x = "wt", y = "mpg",
#'    add = "loess", conf.int = TRUE)
#'
#'
#' # Control point size by continuous variable values ("qsec")
#' ggscatter(df, x = "wt", y = "mpg",
#'    color = "#00AFBB", size = "qsec")
#'
#'
#' # Change colors
#' # +++++++++++++++++++++++++++
#' # Use custom color palette
#' # Add marginal rug
#' ggscatter(df, x = "wt", y = "mpg", color = "cyl",
#'    palette = c("#00AFBB", "#E7B800", "#FC4E07") )
#'
#'
#'
#'
#' # Add group ellipses and mean points
#' # Add stars
#' # +++++++++++++++++++
#' ggscatter(df, x = "wt", y = "mpg",
#'    color = "cyl", shape = "cyl",
#'    palette = c("#00AFBB", "#E7B800", "#FC4E07"),
#'    ellipse = TRUE, mean.point = TRUE,
#'    star.plot = TRUE)
#'
#'
#' # Textual annotation
#' # +++++++++++++++++
#' df$name <- rownames(df)
#' ggscatter(df, x = "wt", y = "mpg",
#'    color = "cyl", palette = c("#00AFBB", "#E7B800", "#FC4E07"),
#'    label = "name", repel = TRUE)
#'
#'
#' @export
ggscatter <- function(data, x, y, combine = FALSE, merge = FALSE,
                      color = "black", fill = "lightgray", palette = NULL,
                      shape = 19, size = 2, point = TRUE,  rug = FALSE,
                      title = NULL, xlab = NULL, ylab = NULL,
                      facet.by = NULL, panel.labs = NULL, short.panel.labs = TRUE,
                      add = c("none", "reg.line", "loess"), add.params = list(),
                      conf.int = FALSE, conf.int.level = 0.95, fullrange = FALSE,
                      ellipse = FALSE, ellipse.level = 0.95,
                      ellipse.type = "norm", ellipse.alpha = 0.1,
                      ellipse.border.remove = FALSE,
                      mean.point = FALSE, mean.point.size = ifelse(is.numeric(size), 2*size, size),
                      star.plot = FALSE, star.plot.lty = 1, star.plot.lwd = NULL,
                      label = NULL,  font.label = c(12, "plain"), font.family = "",
                      label.select = NULL, repel = FALSE, label.rectangle = FALSE,
                      cor.coef = FALSE, cor.coeff.args = list(), cor.method = "pearson", cor.coef.coord = c(NULL, NULL), cor.coef.size = 4,
                      ggp = NULL, show.legend.text = NA,
                      ggtheme = theme_pubr(),
                      ...){


  # Default options
  #:::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::
  .opts <- list(
    combine = combine, merge = merge,
    color = color, fill = fill, palette = palette,
    title = title, xlab = xlab, ylab = ylab,
    facet.by = facet.by, panel.labs = panel.labs, short.panel.labs = short.panel.labs,
    shape = shape, size = size, point = point,  rug = rug,
    add = add, add.params = add.params,
    conf.int = conf.int, conf.int.level = conf.int.level, fullrange = fullrange,
    ellipse = ellipse, ellipse.level = ellipse.level,
    ellipse.type = ellipse.type, ellipse.alpha = ellipse.alpha,
    ellipse.border.remove = ellipse.border.remove,
    mean.point = mean.point, mean.point.size = mean.point.size,
    star.plot = star.plot, star.plot.lty = star.plot.lty, star.plot.lwd = star.plot.lwd,
    label = label, font.label = font.label, font.family = font.family,
    label.select = label.select, repel = repel, label.rectangle = label.rectangle,
    cor.coef = cor.coef, cor.coeff.args = cor.coeff.args, cor.method = cor.method,
    cor.coef.coord = cor.coef.coord, cor.coef.size = cor.coef.size,
    ggp = ggp, show.legend.text = show.legend.text, ggtheme = ggtheme, ...)

  if(!missing(data)) .opts$data <- data
  if(!missing(x)) .opts$x <- x
  if(!missing(y)) .opts$y <- y

  # User options
  #:::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::
  .user.opts <- as.list(match.call(expand.dots = TRUE))
  .user.opts[[1]] <- NULL # Remove the function name
  # keep only user arguments
  for(opt.name in names(.opts)){
    if(is.null(.user.opts[[opt.name]]))
      .opts[[opt.name]] <- NULL
  }

  font.label <- .parse_font(font.label) %>% .compact()
  font.label$color <- ifelse(is.null(font.label$color), color, font.label$color)
  .opts$font.label <- font.label

  .opts$fun <- ggscatter_core
  if(missing(ggtheme) & (!is.null(facet.by) | combine))
    .opts$ggtheme <- theme_pubr(border = TRUE)
  p <- do.call(.plotter, .opts)
  if(.is_list(p) & length(p) == 1) p <- p[[1]]
  return(p)

}


ggscatter_core <- function(data, x, y,
                      color = "black", fill = "lightgray", palette = NULL,
                      shape = 19, size = 2, point = TRUE,  rug = FALSE,
                      title = NULL, xlab = NULL, ylab = NULL,
                      add = c("none", "reg.line", "loess"), add.params = list(),
                      conf.int = FALSE, conf.int.level = 0.95, fullrange = FALSE,
                      ellipse = FALSE, ellipse.level = 0.95,
                      ellipse.type = "norm", ellipse.alpha = 0.1,
                      ellipse.border.remove = FALSE,
                      mean.point = FALSE, mean.point.size = ifelse(is.numeric(size), 2*size, size),
                      star.plot = FALSE, star.plot.lty = 1, star.plot.lwd = NULL,
                      label = NULL,  font.label = c(12, "plain"), font.family = "",
                      label.select = NULL, repel = FALSE, label.rectangle = FALSE,
                      cor.coef = FALSE, cor.coeff.args = list(), cor.method = "pearson", cor.coef.coord = c(NULL, NULL), cor.coef.size = 4,
                      ggp = NULL, show.legend.text = NA,
                      ggtheme = theme_classic(),
                      ...)
{
  add <- match.arg(add)
  add.params <- .check_add.params(add, add.params, error.plot = "", data, color, fill, ...)

  if(length(label) >1){
    if(length(label) != nrow(data))
      stop("The argument label should be a column name or a vector of length = nrow(data). ",
           "It seems that length(label) != nrow(data)")
    else data$label.xx <- label
    label <- "label.xx"
  }
  # label font
  font.label <- .parse_font(font.label)
  font.label$size <- ifelse(is.null(font.label$size), 12, font.label$size)
  font.label$color <- ifelse(is.null(font.label$color), color, font.label$color)
  font.label$face <- ifelse(is.null(font.label$face), "plain", font.label$face)

  if(is.null(ggp)) p <- ggplot(data, aes_string(x, y))
  else p <- ggp

  if(point) p <- p +
      .geom_exec(geom_point, data = data, x = x, y = y,
                 color = color, fill = fill, size = size,
                 shape = shape, ...)

  # Adjust shape when ngroups > 6, to avoid ggplot warnings
  if(shape %in% colnames(data)){
    ngroups <- length(levels(data[, shape]))
    if(ngroups > 6) p <- p + scale_shape_manual(values=1:ngroups, labels = levels(data[, shape]))
  }

  # Add marginal rug
  # +++++++++++
  if(rug) p <- p + .geom_exec(geom_rug, data = data,
                         color = color, size = size/2)

  # Add reg line or loess
  # ++++++++++++
  if(add %in% c("reg.line", "loess")){
    add <- ifelse(add == "reg.line", stats::lm, stats::loess)
    if(is.null(add.params$linetype)) add.params$linetype <- "solid"

    .args <- .geom_exec(NULL, data = data,
                        se = conf.int, level = conf.int.level,
                        color = add.params$color, fill = add.params$fill,
                        linetype = add.params$linetype,
                        fullrange = fullrange)

    mapping <- .args$mapping
    option <- .args$option
    option[["method"]] <- add
    option[["mapping"]] <- do.call(ggplot2::aes_string, mapping)
    p <- p + do.call(geom_smooth, option)
  }


  # Add ellipses
  # +++++++++++
  if(ellipse){
    grp <- intersect(unique(c(color, fill, shape)), colnames(data))[1]
    # NO grouping variable
    if(is.na(grp)) {
      grp <- factor(rep(1, nrow(data)))
      grp_name <- "group"
      data$group <- grp
    }
    # Case of grouping variable
    else {
      grp_name <- grp
      data[, grp_name] <- as.factor(data[, grp_name])
    }

    if (ellipse.type == 'convex')
      p <- p + .convex_ellipse(data, x, y, grp_name, color, fill, ellipse.alpha,
                               ellipse.border.remove = ellipse.border.remove)
    else if(ellipse.type == "confidence")
      p <- p + .confidence_ellipse(data, x, y, grp_name, color, fill,
                                   alpha = ellipse.alpha, level = ellipse.level,
                                   ellipse.border.remove = ellipse.border.remove)
     else if (ellipse.type %in% c('t', 'norm', 'euclid'))
       p <- p + .stat_ellipse(data, x, y, grp_name, color = color, fill = fill,
                              alpha = ellipse.alpha, type = ellipse.type, level = ellipse.level,
                              ellipse.border.remove = ellipse.border.remove)
  }
  # /ellipse

  # Add mean points
  # +++++++++
  if(mean.point) {
    p <- p + .geom_exec(stat_mean, data = data,
                        color = color, shape = shape, fill = fill,
                        size = mean.point.size)
  }

  # Star plots
  # ++++++++++++
  if(star.plot){
    p <- p + .geom_exec(stat_stars, data = data,
                        color = color, linetype = star.plot.lty, size = star.plot.lwd)
  }

  #/ star plots

  # Add textual annotation
  # ++++++
  alpha <- 1
  if(!is.null(list(...)$alpha)) alpha <- list(...)$alpha
  if(!is.null(label)) {
    lab_data <- data
    # Select some labels to show
    if(!is.null(label.select))
      lab_data  <- subset(lab_data, lab_data[, label, drop = TRUE] %in% label.select,
                          drop = FALSE)

    if(repel){
      ggfunc <- ggrepel::geom_text_repel
      if(label.rectangle) ggfunc <- ggrepel::geom_label_repel
        p <- p + .geom_exec(ggfunc, data = lab_data, x = x, y = y,
                          label = label, fontface = font.label$face,
                          size = font.label$size/3, color = font.label$color,
                          alpha = alpha, family = font.family,
                          box.padding = unit(0.35, "lines"),
                          point.padding = unit(0.3, "lines"),
                          force = 1, show.legend = show.legend.text, seed=123)
    }
    else{
      ggfunc <- geom_text
      vjust  <- -0.7
      if(label.rectangle) {
        ggfunc <- geom_label
        vjust <- -0.4
        }
      p <- p + .geom_exec(ggfunc, data = lab_data, x = x, y = y, color = color,
                          label = label, fontface = font.label$face, family = font.family,
                          size = font.label$size/3, color = font.label$color,
                          vjust = vjust, alpha = alpha, show.legend = show.legend.text)
    }
  }

  # Add correlation coefficient
  if(cor.coef){

    if(!missing(cor.method))
      cor.coeff.args$method <- cor.method
    if(!missing(cor.coef.size))
      cor.coeff.args$size <- cor.coef.size
    if(!missing(cor.coef.coord)){
      cor.coeff.args$label.x <- cor.coef.coord[1]
      cor.coeff.args$label.y <- cor.coef.coord[2]
    }
     p <- p + do.call(stat_cor, cor.coeff.args)
  }

  p <- ggpar(p, palette = palette, ggtheme = ggtheme,
             title = title, xlab = xlab, ylab = ylab,...)
  if(font.family != "")
    p <- p + theme(text = element_text(family = font.family))
  p
}



# Add convex ellipse
# data a data frame
# x,y: x and y variables
# grp_name: grp variable
.convex_ellipse <- function(data, x, y, grp_name, color = "black", fill = "lightgray", alpha = 0.1,
                            ellipse.border.remove = FALSE ){

  grp_levels <- levels(data[, grp_name])
  if(length(grp_levels) == 1) .geom_exec(geomfunc = stat_chull, data = data,
                                         color = color, fill = fill, alpha = alpha,
                                         geom = "polygon")
  else {
    if( ellipse.border.remove) color <- NULL
    else color = grp_name
    .geom_exec(geomfunc = stat_chull, data = data,
                  color = color, fill = grp_name, alpha = alpha,
                  geom = "polygon")
  }
}

# Confidence ellipse
.confidence_ellipse <- function(data, x, y, grp_name, color = "black", fill = "lightgray",
                                alpha = 0.1, level = 0.95, ellipse.border.remove = FALSE){
  grp_levels <- levels(data[, grp_name])
  if(length(grp_levels) == 1) {
    mapping <- aes_string(x = x, y = y)
    stat_conf_ellipse(mapping = mapping, data = data,
               color = color, fill = fill, alpha = alpha,
               level = level, geom = "polygon")
  }
  else {
    mapping = aes_string(x = x, y = y, colour = grp_name, fill = grp_name)
    if(ellipse.border.remove ) mapping = aes_string(x = x, y = y,  fill = grp_name)
    stat_conf_ellipse(mapping = mapping, data = data,
                          level = level, alpha = alpha,
                          geom = 'polygon')
  }
}



# Add ggplot2 stat ellipse
.stat_ellipse <- function(data, x, y, grp_name, color = "black", fill = "lightgray",
                          alpha = 0.1, type = "norm", level = 0.95, ellipse.border.remove = FALSE)
  {
  grp_levels <- levels(data[, grp_name])
  if(length(grp_levels) == 1){
    mapping <- aes_string(x = x, y = y)
    ggplot2::stat_ellipse(mapping = mapping, data = data,
                         level = level, type = type,
                         colour = color, fill = fill, alpha = alpha,
                         geom = 'polygon')
  }
  else{
  mapping = aes_string(x = x, y = y, colour = grp_name, group = grp_name, fill = grp_name)
  if(ellipse.border.remove) mapping = aes_string(x = x, y = y, colour = NULL, group = grp_name, fill = grp_name)
  ggplot2::stat_ellipse(mapping = mapping, data = data,
                       level = level, type = type, alpha = alpha,
                       geom = 'polygon')
  }
}
