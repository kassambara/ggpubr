#' @include utilities.R
#' @import ggplot2
NULL
#'Publication ready theme
#'
#'@description \itemize{ \item \strong{theme_pubr()}: Create a publication ready
#'  theme \item \strong{theme_pubclean()}: a clean theme without axis lines, to
#'  direct more attention to the data.  \item \strong{labs_pubr()}: Format only
#'  plot labels to a publication ready style \item \strong{theme_classic2()}:
#'  Create a classic theme with axis lines. \item \strong{clean_theme()}: Remove
#'  axis lines, ticks, texts and titles. \item \strong{clean_table_theme()}:
#'  Clean the the theme of a table, such as those created by
#'  \code{\link{ggsummarytable}()}}.
#'@param base_size base font size
#'@param base_family base font family
#'@param border logical value. Default is FALSE. If TRUE, add panel border.
#'@param margin logical value. Default is TRUE. If FALSE, reduce plot margin.
#'@param legend character specifying legend position. Allowed values are one of
#'  c("top", "bottom", "left", "right", "none"). Default is "top" side position.
#'  to remove the legend use legend = "none". Legend position can be also
#'  specified using a numeric vector c(x, y).  In this case it is possible to
#'  position the legend inside the plotting area. x and y are the coordinates of
#'  the legend box. Their values should be between 0 and 1. c(0,0) corresponds
#'  to the "bottom left" and c(1,1) corresponds to the "top right" position. For
#'  instance use legend = c(0.8, 0.2).
#'@param x.text.angle Rotation angle of x axis tick labels. Default value is 0.
#'  Use 90 for vertical text.
#'@param flip logical. If TRUE, grid lines are added to y axis instead of x
#'  axis.
#' @examples
#' p <- ggplot(mtcars, aes(x = wt, y = mpg)) +
#'    geom_point(aes(color = gear))
#'
#' # Default plot
#' p
#'
#' # Use theme_pubr()
#' p + theme_pubr()
#'
#' # Format labels
#' p + labs_pubr()
#'
#'@name theme_pubr
#'@rdname theme_pubr
#'@export
theme_pubr <- function (base_size = 12, base_family = "",
                        border = FALSE, margin = TRUE,
                        legend = c("top", "bottom", "left", "right", "none"),
                        x.text.angle = 0)
{
  half_line <- base_size/2
  if(!is.numeric(legend)) legend <- match.arg(legend)
  if(x.text.angle > 5) xhjust <- 1 else xhjust <- NULL

  if(border){
    panel.border <- element_rect(fill = NA, colour = "black", size = 0.7)
    axis.line <- element_blank()
  }
  else{
    panel.border <- element_blank()
    axis.line = element_line(colour = "black", linewidth = 0.5)
  }


  if(margin)
    plot.margin <- margin(half_line, half_line, half_line,
                          half_line)
  else plot.margin <- unit(c(0.5,0.3,0.3,0.3),"mm")

  .theme <- theme_bw(base_size = base_size, base_family = base_family) %+replace%
    theme(panel.border = panel.border,
          panel.grid.major = element_blank(),
          panel.grid.minor = element_blank(),
          axis.line = axis.line, axis.text = element_text(color = "black"),
          legend.key = element_blank(),
          strip.background = element_rect(fill = "#F2F2F2", colour = "black", size = 0.7),
          plot.margin = plot.margin,
          legend.position = legend,
          complete = TRUE)

  if(x.text.angle!=0)
    .theme <- .theme + theme(axis.text.x = element_text(angle = x.text.angle, hjust = xhjust))

  .theme
}

#' @rdname theme_pubr
#' @export
theme_pubclean <- function (base_size = 12, base_family = "", flip = FALSE)
{
  res <- theme_grey(base_size = base_size, base_family = base_family) +
    theme(
      panel.background = element_rect(fill = "white"),
      legend.background = element_rect(fill = "white"),
      legend.position = "top"

    )
  if(flip){
    res <- res + theme(
      panel.grid.major.x = element_line(linetype = "dotted", color = "grey"),
      axis.line.y = element_line(color = "black")
    )
  }
  else{
    res <- res + theme(
      panel.grid.major.y = element_line(linetype = "dotted", color = "grey")
    )
  }
  res
}


#' @rdname theme_pubr
#' @export
labs_pubr <- function(base_size = 14, base_family = ""){
  theme(
    text = element_text(family = base_family,
                        face = "plain", colour = "black", size = base_size, lineheight = 0.9,
                        hjust = 0.5, vjust = 0.5, angle = 0, margin = margin(),
                        debug = FALSE),
    # Tick labels
    axis.text.x = element_text(size = rel(0.86), colour = "black", face = "bold"),
    axis.text.y = element_text(size = rel(0.86), colour = "black", face = "bold"),

    # Axis labels
    axis.title = element_text(size = rel(1), colour = "black", face = "bold"),

    # Main title
    plot.title = element_text(size = rel(1), colour = "black" ,
                              lineheight=1.0, face = "bold"),
    legend.title = element_text(size = rel(0.7), face = "bold", colour = "black"),
    legend.text = element_text(size = rel(0.7), face = "plain", colour = "black")
  )
}

#' @rdname theme_pubr
#' @export
theme_classic2 <-
  function (base_size = 12, base_family = "")
  {
    theme_classic(base_size = base_size, base_family = base_family) %+replace%
      theme(
        axis.line.x = element_line(),
        axis.line.y = element_line()
      )
  }


#' @export
#' @rdname theme_pubr
clean_theme <- function()
{
  theme(
    axis.line.x = element_blank(),
    axis.line.y = element_blank(),
    axis.text.x = element_blank(),
    axis.text.y = element_blank(),
    axis.ticks.x = element_blank(),
    axis.ticks.y = element_blank(),
    axis.title.x = element_blank(),
    axis.title.y = element_blank()
  )
}

#' @export
#' @rdname theme_pubr
clean_table_theme <- function ()
{
  theme(axis.line.x = element_blank(), axis.line.y = element_blank(),
        axis.text.x = element_blank(), axis.ticks.x = element_blank(),
        axis.ticks.y = element_blank(), axis.title.x = element_blank(),
        axis.title.y = element_blank())
}

