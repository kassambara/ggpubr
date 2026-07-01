#' @include utilities.R
NULL
#' Annotate Arranged Figure
#'
#' @description Annotate figures including: i) ggplots, ii) arranged ggplots from
#'  \code{\link{ggarrange}()}, \code{\link[gridExtra:arrangeGrob]{grid.arrange}()} and
#'  \code{\link[cowplot]{plot_grid}()}.
#' @param p (arranged) ggplots.
#' @param top,bottom,left,right optional string, or grob.
#' @param fig.lab figure label (e.g.: "Figure 1").
#' @param fig.lab.pos position of the figure label, can be one of "top.left",
#'  "top", "top.right", "bottom.left", "bottom", "bottom.right". Default is
#'  "top.left".
#' @param fig.lab.size optional size of the figure label.
#' @param fig.lab.face optional font face of the figure label. Allowed values
#'  include: "plain", "bold", "italic", "bold.italic".
#' @author Laszlo Erdey \email{erdey.laszlo@@econ.unideb.hu}
#' @seealso \code{\link{ggarrange}()}
#' @examples
#' data("ToothGrowth")
#' df <- ToothGrowth
#' df$dose <- as.factor(df$dose)
#'
#' # Create some plots
#' # ::::::::::::::::::::::::::::::::::::::::::::::::::
#' # Box plot
#' bxp <- ggboxplot(df,
#'   x = "dose", y = "len",
#'   color = "dose", palette = "jco"
#' )
#' # Dot plot
#' dp <- ggdotplot(df,
#'   x = "dose", y = "len",
#'   color = "dose", palette = "jco"
#' )
#' # Density plot
#' dens <- ggdensity(df, x = "len", fill = "dose", palette = "jco")
#'
#' # Arrange and annotate
#' # ::::::::::::::::::::::::::::::::::::::::::::::::::
#' figure <- ggarrange(bxp, dp, dens, ncol = 2, nrow = 2)
#' annotate_figure(figure,
#'   top = text_grob("Visualizing Tooth Growth", color = "red", face = "bold", size = 14),
#'   bottom = text_grob("Data source: \n ToothGrowth data set",
#'     color = "blue",
#'     hjust = 1, x = 1, face = "italic", size = 10
#'   ),
#'   left = text_grob("Figure arranged using ggpubr", color = "green", rot = 90),
#'   right = text_grob(bquote("Superscript: (" * kg ~ NH[3] ~ ha^-1 ~ yr^-1 * ")"), rot = 90),
#'   fig.lab = "Figure 1", fig.lab.face = "bold"
#' )
#'
#' @export
annotate_figure <- function(p,
                            top = NULL, bottom = NULL, left = NULL, right = NULL,
                            fig.lab = NULL,
                            fig.lab.pos = c(
                              "top.left", "top", "top.right",
                              "bottom.left", "bottom", "bottom.right"
                            ),
                            fig.lab.size, fig.lab.face) {
  fig.lab.pos <- match.arg(fig.lab.pos)
  annot.args <- list(top = top, bottom = bottom, left = left, right = right) %>%
    .compact()

  # Anchor the figure label with a length-independent horizontal justification.
  # cowplot::draw_figure_label() uses hjust = -0.1 / 1.1 for the left/right
  # positions, a width-proportional offset, so a longer label is pushed further
  # from the corner and captions no longer align across figures (#185). Use a
  # fixed hjust (0 for left, 1 for right; the centre positions already use 0, as
  # cowplot did) with a fixed x anchor; vertical placement matches cowplot.
  fig.lab.args <- switch(fig.lab.pos,
    "top.left"     = list(x = 0.01, y = 1, hjust = 0, vjust = 1.1),
    "top"          = list(x = 0.50, y = 1, hjust = 0, vjust = 1.1),
    "top.right"    = list(x = 0.99, y = 1, hjust = 1, vjust = 1.1),
    "bottom.left"  = list(x = 0.01, y = 0, hjust = 0, vjust = -0.1),
    "bottom"       = list(x = 0.50, y = 0, hjust = 0, vjust = -0.1),
    "bottom.right" = list(x = 0.99, y = 0, hjust = 1, vjust = -0.1)
  )
  lab.args <- c(list(label = fig.lab), fig.lab.args)
  # Preserve the previous default label size/face, which cowplot::draw_figure_label()
  # took from the current theme text (not draw_plot_label's bold/16 default) (#185).
  lab.args$size <- if (!missing(fig.lab.size)) fig.lab.size else ggplot2::theme_get()$text$size
  lab.args$fontface <- if (!missing(fig.lab.face)) fig.lab.face else ggplot2::theme_get()$text$face


  if (!.is_empty(annot.args)) {
    p <- gridExtra::arrangeGrob(p, top = top, bottom = bottom, left = left, right = right) %>%
      as_ggplot()
  }

  if (!is.null(fig.lab)) {
    p <- cowplot::ggdraw(p) + do.call(cowplot::draw_plot_label, lab.args)
  }

  p
}
