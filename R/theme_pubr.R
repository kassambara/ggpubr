#' @include utilities.R
#' @import ggplot2
NULL
#' Publication ready theme
#'
#' @description
#' \itemize{
#'  \item \strong{theme_pubr()}: Create a publication ready theme
#'  \item \strong{labs_pubr()}: Format only plot labels to a publication ready style
#'  \item \strong{theme_classic()}: Create a classic theme with axis lines
#' }
#' @param base_size base font size
#' @param base_family base font family
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
#' @name theme_pubr
#' @rdname theme_pubr
#' @export
theme_pubr <-
  function (base_size = 14, base_family = "")
  {
    theme_bw(base_size = base_size, base_family = base_family) %+replace%
      theme(
        panel.border = element_blank(), axis.line = element_line(colour = "black"),
        panel.grid.major = element_blank(), panel.grid.minor = element_blank(),
        strip.background = element_rect(colour = "black",
                                        size = 0.5),
        legend.key = element_blank(),
        # Tick labels
        axis.text.x = element_text(size = rel(0.86), colour = "black",face = "bold"),
        axis.text.y = element_text(size = rel(0.86), colour = "black",face = "bold"),

        # Axis
        axis.title = element_text(size = rel(1), colour = "black", face = "bold"),
        axis.line.x = element_line(colour = "black", size = 1),
        axis.line.y = element_line(colour = "black", size = 1),
        axis.ticks = element_line(colour = "black", size = 1),

        # Main title
        plot.title = element_text(size = rel(1), colour = "black" ,
                                  lineheight=1.0, face = "bold"),

        legend.position = "bottom",
        legend.title = element_text(size = rel(0.7), face = "bold", colour = "black"),
        legend.text = element_text(size = rel(0.7), face = "plain", colour = "black")
      )
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
