#' @include utilities.R ggpar.R stat_chull.R stat_conf_ellipse.R
NULL
#' Scatter plot
#' @description Create a scatter plot.
#' @inheritParams ggboxplot
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
#'   'convex', 'confidence' or types supporeted by
#'   \code{\link[ggplot2]{stat_ellipse}} including one of c("t", "norm",
#'   "euclid").
#' @param ellipse.alpha Alpha for ellipse specifying the transparency level of
#'   fill color. Use alpha = 0 for no fill color.
#' @param mean.point logical value. If TRUE, group mean points are added to the
#'   plot.
#' @param mean.point.size numeric value specifying the size of mean points.
#' @param label the name of the column containing point labels.
#' @param label.size text size for point labels.
#' @param label.select character vector specifying some labels to show.
#' @param repel a logical value, whether to use ggrepel to avoid overplotting text
#'   labels or not.
#' @param ... other arguments to be passed to \code{\link[ggplot2]{geom_point}}
#'   and \code{\link{ggpar}}.
#' @details The plot can be easily customized using the function ggpar(). Read
#'   ?ggpar for changing: \itemize{ \item main title and axis labels: main,
#'   xlab, ylab \item axis limits: xlim, ylim (e.g.: ylim = c(0, 30)) \item axis
#'   scales: xscale, yscale (e.g.: yscale = "log2") \item color palettes:
#'   palette = "Dark2" or palette = c("gray", "blue", "red") \item legend title,
#'   labels and position: legend = "right" \item plot orientation : orientation
#'   = c("vertical", "horizontal", "reverse") }
#' @seealso \code{\link{ggpar}}
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
#'    color = "black", shape = 21, size = 3, # point color, shape and size
#'    add = "reg.line", add.params = list(color = "blue", fill = "lightgray"),
#'    conf.int = TRUE)
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
#' # Add marginal rug
#' # +++++++++++++++++++++++++++++
#' ggscatter(df, x = "wt", y = "mpg",
#'    color = "cyl", rug = TRUE)
#'
#'
#' # Change colors
#' # +++++++++++++++++++++++++++
#' # Use custom color palette
#' ggscatter(df, x = "wt", y = "mpg", color = "cyl",
#'    palette = c('#999999','#E69F00','#56B4E9') )
#'
#' # Use brewer palette
#' ggscatter(df, x = "wt", y = "mpg", color = "cyl",
#'    palette = "Dark2" )
#'
#' # Use grey palette
#' ggscatter(df, x = "wt", y = "mpg", color = "cyl",
#'    palette = "grey" )
#'
#'
#' # Add group ellipses and mean points
#' # +++++++++++++++++++
#'
#' ggscatter(df, x = "wt", y = "mpg",
#'    color = "cyl", shape = "cyl",
#'    palette = c('#999999','#E69F00','#56B4E9'),
#'    ellipse = TRUE, mean.point = TRUE)
#'
#'
#' # Textual annotation
#'
#'
#'
#' @export
ggscatter <- function(data, x, y,
                      color = "black", fill = "lightgray", palette = NULL,
                      shape = 19, size = 2, point = TRUE,  rug = FALSE,
                      add = c("none", "reg.line", "loess"), add.params = list(),
                      conf.int = FALSE, conf.int.level = 0.95, fullrange = FALSE,
                      ellipse = FALSE, ellipse.level = 0.95,
                      ellipse.type = "norm", ellipse.alpha = 0.1,
                      mean.point = FALSE, mean.point.size = 2*size,
                      label = NULL, label.size = 5, label.select = NULL, repel = FALSE,
                      ggtheme = theme_pubr(),
                      ...)
{

  add <- match.arg(add)
  add.params <- .check_add.params(add, add.params, error.plot = "", data, color, fill, ...)

  p <- ggplot(data, aes_string(x, y))

  if(point) p <- p +
      .geom_exec(geom_point, data = data,
                 color = color, fill = fill, size = size,
                 shape = shape, ...)

  # Add marginal rug
  # +++++++++++
  if(rug) p <- p + .geom_exec(geom_rug, data = data,
                         color = color, size = size/2)

  # Add reg line or loess
  # ++++++++++++
  if(add %in% c("reg.line", "loess")){
    add <- ifelse(add == "reg.line", lm, loess)
    if(is.null(add.params$linetype)) add.params$linetype <- "solid"

    .args <- .geom_exec(geom_smooth=NULL, data = data,
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
      p <- p + .convex_ellipse(data, x, y, grp_name, color, fill, ellipse.alpha)
    else if(ellipse.type == "confidence")
      p <- p + .confidence_ellipse(data, x, y, grp_name, color, fill,
                                   alpha = ellipse.alpha, level = ellipse.level)
     else if (ellipse.type %in% c('t', 'norm', 'euclid'))
       p <- p + .stat_ellipse(data, x, y, grp_name, color = color, fill = fill,
                              alpha = ellipse.alpha, type = ellipse.type, level = ellipse.level)
  }
  # /ellipse

  # Add mean points
  # +++++++++
  if(mean.point) {
    p <- p + .geom_exec(stat_mean, data = data,
                        color = color, shape = shape,
                        size = mean.point.size)
  }

  # Add textual annotation
  # ++++++
  if(!is.null(label)) {
    lab_data <- data
    # Select some labels to show
    if(!is.null(label.select))
      lab_data  <- subset(lab_data, lab_data[, label, drop = TRUE] %in% label.select,
                          drop = FALSE)

    if(repel)
      p <- p + .geom_exec(ggrepel::geom_text_repel, data = lab_data,
                          color = color, label = label, size = label.size)
    else
      p <- p + .geom_exec(geom_text, data = lab_data, color = color,
                          label = label, size = label.size, vjust = -0.7)
  }


  p <- ggpar(p, palette = palette, ggtheme = ggtheme, ...)
  p
}



# Add convex ellipse
# data a data frame
# x,y: x and y variables
# grp_name: grp variable
.convex_ellipse <- function(data, x, y, grp_name, color = "black", fill = "lightgray", alpha = 0.1){
  grp_levels <- levels(data[, grp_name])

  if(length(grp_levels) == 1) .geom_exec(geomfunc = stat_chull, data = data,
                                         color = color, fill = fill, alpha = alpha,
                                         geom = "polygon")
  else .geom_exec(geomfunc = stat_chull, data = data,
                  color = grp_name, fill = grp_name, alpha = alpha,
                  geom = "polygon")
}

# Confidence ellipse
.confidence_ellipse <- function(data, x, y, grp_name, color = "black", fill = "lightgray",
                                alpha = 0.1, level = 0.95){
  grp_levels <- levels(data[, grp_name])
  if(length(grp_levels) == 1) {
    mapping <- aes_string(x = x, y = y)
    stat_conf_ellipse(mapping = mapping, data = data,
               color = color, fill = fill, alpha = alpha,
               level = level, geom = "polygon")
  }
  else {
    mapping = aes_string(x = x, y = y, colour = grp_name, fill = grp_name)
    stat_conf_ellipse(mapping = mapping, data = data,
                          level = level, alpha = alpha,
                          geom = 'polygon')
  }
}



# Add ggplot2 stat ellipse
.stat_ellipse <- function(data, x, y, grp_name, color = "black", fill = "lightgray",
                          alpha = 0.1, type = "norm", level = 0.95)
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
  ggplot2::stat_ellipse(mapping = mapping, data = data,
                       level = level, type = type, alpha = alpha,
                       geom = 'polygon')
  }
}




# Return the coordinates of groups levels
# x : coordinate of individuals on x axis
# y : coordinate of indiviuals on y axis
# .get_coord_groups<-function(data, x, y, groups){
#
#   data %>% group_by_(groups) %>%
#     summa
#   x_val <- data[, x]
#   y_val <- data[, y]
#   data.frame(
#     x= tapply(x_val, groups, mean),
#     y = tapply(y_val, groups, mean)
#   )
# }



