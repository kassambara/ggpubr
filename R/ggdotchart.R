#' @include utilities.R ggpar.R stat_chull.R stat_conf_ellipse.R
NULL
#' Cleveland's Dot Plots
#' @description Draw a Cleveland dot plot.
#' @inheritParams ggboxplot
#' @inheritParams ggpar
#' @param data a data frame
#' @param x,y x and y variables for drawing.
#' @param color,size points color and size.
#' @param dot.size numeric value specifying the dot size.
#' @param shape point shape. See \code{\link{show_point_shapes}}.
#' @param label the name of the column containing point labels.
#' @param group an optional column name indicating how the elements of x are
#'   grouped.
#' @param sorting a character vector for sorting into ascending or descending
#'   order. Allowed values are one of "descending" and "ascending". Partial
#'   match are allowed (e.g. sorting = "desc" or "asc"). Default is
#'   "descending".
#' @param x.text.col logical. If TRUE (default), x axis texts are colored by
#'   groups.
#' @param position Position adjustment, either as a string, or the result of a
#'   call to a position adjustment function.
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
#' df$name <- rownames(df)
#' head(df[, c("wt", "mpg", "cyl")], 3)
#'
#' # Basic plot
#' ggdotchart(df, x = "name", y ="mpg",
#'   ggtheme = theme_bw())
#'
#' # Change colors by  group cyl
#' ggdotchart(df, x = "name", y = "mpg",
#'    group = "cyl", color = "cyl",
#'    palette = c('#999999','#E69F00','#56B4E9'),
#'    rotate = TRUE,
#'    sorting = "descending",
#'    ggtheme = theme_bw(),
#'    y.text.col = TRUE )
#'
#'
#' @export
ggdotchart <- function(data, x, y, group = NULL,
                       combine = FALSE,
                       color = "black",  palette = NULL,
                       shape = 19, size = NULL, dot.size = size,
                       sorting = c("ascending", "descending"),
                       add = c("none", "segment"), add.params = list(),
                       x.text.col = TRUE,
                       rotate = FALSE,
                       title = NULL, xlab = NULL, ylab = NULL,
                       facet.by = NULL, panel.labs = NULL, short.panel.labs = TRUE,
                       select = NULL, remove = NULL, order = NULL,
                       label = NULL, font.label = list(size = 11, color = "black"),
                       label.select = NULL, repel = FALSE, label.rectangle = FALSE,
                       position = "identity",
                       ggtheme = theme_pubr(),
                       ...){

  # Default options
  #:::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::
  .opts <- list( data = data, x = x, y = y,
    group = group, combine = combine,
    color = color, palette = palette, shape = shape,
    size = size, dot.size = dot.size, sorting = sorting,
    x.text.col = x.text.col,
    rotate = rotate, title = title, xlab = xlab, ylab = ylab,
    facet.by = facet.by, panel.labs = panel.labs, short.panel.labs = short.panel.labs,
    select = select , remove = remove, order = order,
    add = add, add.params = add.params,
    label = label, font.label = font.label, label.select = label.select,
    repel = repel, label.rectangle = label.rectangle,
    position = position, ggtheme = ggtheme, ...)

  # User options
  #:::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::
  .user.opts <- as.list(match.call(expand.dots = TRUE))
  .user.opts[[1]] <- NULL # Remove the function name
  # keep only user arguments
  for(opt.name in names(.opts)){
    if(is.null(.user.opts[[opt.name]]))
      .opts[[opt.name]] <- NULL
  }

  .opts$fun <- ggdotchart_core
  .opts$fun_name <- "ggdotchart"

  if(missing(ggtheme) & (!is.null(facet.by) | combine))
    .opts$ggtheme <- theme_pubr(border = TRUE)

  p <- do.call(.plotter, .opts)

  if(.is_list(p) & length(p) == 1) p <- p[[1]]
  return(p)

}


ggdotchart_core <- function(data, x, y, group = NULL,
                       color = "black",  palette = NULL,
                       shape = 19, size = NULL, dot.size = size,
                       sorting = c("ascending", "descending"),
                       add = c("none", "segments"), add.params = list(),
                       x.text.col = FALSE,
                       rotate = FALSE,
                       title = NULL, xlab = NULL, ylab = NULL,
                       ggtheme = theme_bw(),
                       position = "identity",
                       ...)
{
  add <- match.arg(add)
  if(!is.null(group)){
    if(group == 1)
      group <- NULL
  }

  if(is.null(group) & color[1] %in% names(data)){
    group <- color[1]
  }


  .dots <- list(...)
  sorting <- match.arg(sorting)
  decreasing <- ifelse(sorting == "descending", FALSE, TRUE)
  x.text.angle <- ifelse(rotate, 0, 90)
  if(!is.null(.dots$y.text.col))
    x.text.col <- .dots$y.text.col
  data <- as.data.frame(data)
  label <- .select_vec(data, x)
  . <- NULL

  if(rotate & sorting == "descending") sorting <- "ascending"
  else if(rotate & sorting == "ascending") sorting <- "descending"

  y.sort <- y
  if(sorting == "descending") y.sort <- paste0("desc(", y, ")")
  if(is.null(group))
    data <- dplyr::arrange_(data, .dots = y.sort)
  else if(group != 1)
    data <- dplyr::arrange_(data, .dots = c(group, y.sort))

  data[, x] <- factor(data[, x], levels = unique(as.vector(data[, x])))

  p <- ggplot(data, aes_string(x = x, y =y))

  if(add == "segments"){
    seg.opts <- geom_exec(data = data, color = color,
                          size = size, position = position)

    mapping <- seg.opts$mapping %>%
      .add_item(x = x, ymin = 0, ymax = y, group = group)
    option <- seg.opts$option

    # mapping <- seg.opts$mapping %>%
    #   .add_item(y = 0, x = x, yend = y, xend = x)
    # option <- seg.opts$option

    seg.col <- "lightgray"
    if(!is.null(add.params$color))
      seg.col <- add.params$color
    else if(!is.null(add.params$colour))
      seg.col <- add.params$colour
    if(seg.col %in% names(data)) mapping$color <- seg.col
    else option$color <- seg.col


    if(!is.null(add.params$size))
      option$size <- add.params$size

    # if(!is.null(add.params$color))
    #   option$color <- add.params$color
    # else if(!is.null(add.params$colour))
    #   option$color <- add.params$colour
    # if(!is.null(add.params$size))
    #   option$size <- add.params$size

    option[["mapping"]] <- do.call(aes_string, mapping)
    p <- p + do.call(geom_linerange, option)
  }


  p <- p + geom_exec(geom_point, data = data, shape = shape,
                      color = color, size = dot.size, position = position)


  p <- ggpar(p, palette = palette, ggtheme = ggtheme, x.text.angle = x.text.angle,
             title = title, xlab = xlab, ylab = ylab, ...)


  # Change x axis text colors
  if(x.text.col){
    if(!rotate)
      p <- .set_x_text_col(p, label, x.text.angle)
    else
      p <- .set_y_text_col(p, label, x.text.angle)
  }

  if(x.text.angle == 90 & !rotate){
    p <- p + theme(axis.text.x = element_text(vjust = 0.5))
  }

  if(rotate) p <- p + coord_flip()

  p
}


# Set x text color
.set_x_text_col <- function(p, label, angle){
  g <- ggplot2::ggplot_build(p)
  cols <- unlist(g$data[[1]]["colour"])
  names(cols) <- as.vector(label) # Give every color an appropriate name
  p + theme(axis.text.x = element_text(colour = cols, angle = angle, hjust = 1))
}

# Set y text color
.set_y_text_col <- function(p, label, angle){
  g <- ggplot2::ggplot_build(p)
  cols <- unlist(g$data[[1]]["colour"])
  names(cols) <- as.vector(label) # Give every color an appropriate name
  p + theme(axis.text.y = element_text(colour = cols))
}





# Helper functions
# +++++++++++++++++++++++++
#' @export
#' @rdname ggdotchart
theme_cleveland <- function(rotate = TRUE){
  if(rotate){
    theme(panel.grid.major.x = element_blank(),
          panel.grid.minor.x = element_blank(),
          panel.grid.major.y = element_line(colour = "grey70", linetype = "dashed"),
          axis.title.y = element_blank(),
          axis.ticks.y = element_blank())
  }
  else{
    theme(panel.grid.major.x = element_line(colour = "grey70", linetype = "dashed"),
          panel.grid.major.y = element_blank(),
          panel.grid.minor.y = element_blank(),
          axis.title.x = element_blank(),
          axis.ticks.x = element_blank())
  }

}




