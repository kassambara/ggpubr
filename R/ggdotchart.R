#' @include utilities.R ggpar.R stat_chull.R stat_conf_ellipse.R
NULL
#' Cleveland's Dot Plots
#' @description Draw a Cleveland dot plot.
#' @inheritParams ggpar
#' @param data a data frame
#' @param x x variable for drawing.
#' @param color,size points color and size.
#' @param shape point shape. See \code{\link{show_point_shapes}}.
#' @param label the name of the column containing point labels.
#' @param group an optional column name indicating how the elements of x are
#'   grouped.
#' @param sorting a character vector for sorting into ascending or descending
#'   order. Allowed values are one of "descending" and "ascending". Partial
#'   match are allowed (e.g. sorting = "desc" or "asc"). Default is
#'   "descending".
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
#' ggdotchart(df, x = "mpg", label = "name" )
#'
#' # Change colors by  group cyl
#' ggdotchart(df, x = "mpg", label = "name",
#'    group = "cyl", color = "cyl",
#'    palette = c('#999999','#E69F00','#56B4E9') )
#'
#' # Use brewer palette
#' ggdotchart(df, x = "mpg", label = "name",
#'    group = "cyl", color = "cyl", palette = "Dark2" )
#'
#' # Change the orientation
#' # Sort in ascending order
#' ggdotchart(df, x = "mpg", label = "name",
#'    group = "cyl", color = "cyl",
#'     palette = c("#00AFBB", "#E7B800", "#FC4E07"),
#'    orientation = "horizontal", sorting = "ascending" )
#'
#'
#' @export
ggdotchart <- function(data, x, label, group = NULL,
                      color = "black",  palette = NULL,
                      shape = 19, size = 1,
                      sorting = c("descending", "ascending"),
                      orientation = c("vertical", "horizontal"),
                      ggtheme = theme_bw(),
                      ...)
{
  sorting <- match.arg(sorting)
  orientation <- match.arg(orientation)
  decreasing <- ifelse(sorting == "descending", FALSE, TRUE)
  if(is.null(group)){
    if (sorting == "descending") data <- data[order(data[, x]), , drop = FALSE]
    else data <- data[order(-data[, x]), , drop = FALSE]
    data[, label] <- factor(data[, label], levels = as.vector(data[, label]))
  }
  else{
    decreasing <- ifelse(orientation == "vertical", TRUE, FALSE)
    if (sorting == "descending")
      data <- data[order(data[, group], -data[, x], decreasing = decreasing), , drop = FALSE]
    else
      data <- data[order(data[, group], data[, x], decreasing = decreasing), , drop = FALSE]
      data[, label] <- factor(data[, label], levels = as.vector(data[, label]))
  }

  if(orientation == "vertical") p <- ggplot(data, aes_string(x = x, y =label))
  else p <- ggplot(data, aes_string(x = label, y =x))

  p <- p + .geom_exec(geom_point, data = data, shape = shape,
                      color = color, size = size)
  p <- ggpar(p, palette = palette, ggtheme = ggtheme,  ...)

  # theme
  if(orientation == "vertical") p <- .theme_vertical(p, data[, label])
  else p <- .theme_horizontal(p, data[, label])



  p
}


# Helper functions
# +++++++++++++++++++++++++
.theme_vertical <- function(p, label){
  p <- p + theme(panel.grid.major.x = element_blank(),
                 panel.grid.major.y = element_line(colour = "grey70", linetype = "dashed"),
                 axis.title.y = element_blank(),
                 axis.ticks.y = element_blank())+labs_pubr()

  # y axis text colors
  # +++++++++++++++++++++++++++
  g <- ggplot2::ggplot_build(p)
  cols <- unlist(g$data[[1]]["colour"])
  names(cols) <- as.vector(label) # Give every color an appropriate name
  p <- p + theme(axis.text.y = ggplot2::element_text(colour = cols))

  p
}

.theme_horizontal <- function(p, label){
  p <- p + theme(panel.grid.major.x = element_line(colour = "grey70", linetype = "dashed"),
                panel.grid.major.y = element_blank(),
                axis.title.x = element_blank(),
                axis.ticks.x = element_blank())+ labs_pubr()
  # x axis text colors
  # +++++++++++++++++++++++++++
  g <- ggplot2::ggplot_build(p)
  cols <- unlist(g$data[[1]]["colour"])
  names(cols) <- as.vector(label) # Give every color an appropriate name
  p <- p + theme(axis.text.x = ggplot2::element_text(colour = cols, angle = 90, hjust = 1))

  p
}

