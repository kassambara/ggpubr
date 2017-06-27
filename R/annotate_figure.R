#' @include utilities.R
NULL
#'Annotate Arranged Figure
#'
#'@description Annotate figures including: i) ggplots, ii) arranged ggplots from
#'  \code{\link{ggarrange}()}, \code{\link[gridExtra]{grid.arrange}()} and
#'  \code{\link[cowplot]{plot_grid}()}.
#'@param p (arranged) ggplots.
#'@param top,bottom,left,right optional string, or grob.
#'@param fig.lab figure label (e.g.: "Figure 1").
#'@param fig.lab.pos position of the figure label, can be one of "top.left",
#'  "top", "top.right", "bottom.left", "bottom", "bottom.right". Default is
#'  "top.left".
#'@param fig.lab.size optional size of the figure label.
#'@param fig.lab.face optional font face of the figure label. Allowed values
#'  include: "plain", "bold", "italic", "bold.italic".
#'@author Alboukadel Kassambara \email{alboukadel.kassambara@@gmail.com}
#'@seealso \code{\link{ggarrange}()}
#' @examples
#' data("ToothGrowth")
#' df <- ToothGrowth
#' df$dose <- as.factor(df$dose)
#'
#' # Create some plots
#' # ::::::::::::::::::::::::::::::::::::::::::::::::::
#' # Box plot
#' bxp <- ggboxplot(df, x = "dose", y = "len",
#'     color = "dose", palette = "jco")
#' # Dot plot
#' dp <- ggdotplot(df, x = "dose", y = "len",
#'     color = "dose", palette = "jco")
#' # Density plot
#' dens <- ggdensity(df, x = "len", fill = "dose", palette = "jco")
#'
#' # Arrange and annotate
#' # ::::::::::::::::::::::::::::::::::::::::::::::::::
#' figure <- ggarrange(bxp, dp, dens, ncol = 2, nrow = 2)
#'annotate_figure(figure,
#'                top = text_grob("Visualizing Tooth Growth", color = "red", face = "bold", size = 14),
#'                bottom = text_grob("Data source: \n ToothGrowth data set", color = "blue",
#'                                   hjust = 1, x = 1, face = "italic", size = 10),
#'                left = text_grob("Figure arranged using ggpubr", color = "green", rot = 90),
#'                right = "I'm done, thanks :-)!",
#'                fig.lab = "Figure 1", fig.lab.face = "bold"
#')
#'
#'
#'@export
annotate_figure <- function(p,
                            top = NULL, bottom = NULL, left = NULL, right = NULL,
                            fig.lab = NULL,
                            fig.lab.pos = c("top.left", "top", "top.right",
                                            "bottom.left", "bottom", "bottom.right"),
                            fig.lab.size, fig.lab.face
                            )
{

  fig.lab.pos <- match.arg(fig.lab.pos)
  annot.args <- list(top = top, bottom = bottom, left = left,right = right) %>%
    .compact()

  lab.args <- list(label = fig.lab, position = fig.lab.pos) %>%
    .compact()
  if(!missing(fig.lab.size)) lab.args$size <- fig.lab.size
  if(!missing(fig.lab.face)) lab.args$fontface <- fig.lab.face



  if(!.is_empty(annot.args)){
    p <- gridExtra::arrangeGrob(p, top = top, bottom = bottom, left = left,right = right) %>%
      as_ggplot()
  }

  if(!is.null(fig.lab)){
    p <- cowplot::ggdraw(p) + do.call(cowplot::draw_figure_label, lab.args)

  }

  p
}
