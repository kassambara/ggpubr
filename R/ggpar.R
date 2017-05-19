#' @include utilities.R
NULL
#' Graphical parameters
#'
#' @param p an object of class ggplot or a list of ggplots
#'
#' @param palette the color palette to be used for coloring or filling by
#'   groups. Allowed values include "grey" for grey color palettes; brewer
#'   palettes e.g. "RdBu", "Blues", ...; or custom color palette e.g. c("blue",
#'   "red"); and scientific journal palettes from ggsci R package, e.g.: "npg",
#'   "aaas", "lancet", "jco", "ucscgb", "uchicago", "simpsons" and
#'   "rickandmorty". Can be also a numeric vector of length(groups); in this
#'   case a basic color palette is created using the function \link[grDevices]{palette}.
#' @param gradient.cols vector of colors to use for n-colour gradient. Allowed
#'   values include brewer and ggsci color palettes.
#' @param main,title plot main title.
#' @param submain,subtitle plot subtitle.
#' @param caption plot caption.
#' @param xlab character vector specifying x axis labels. Use xlab
#'   = FALSE to hide xlab.
#' @param ylab character vector specifying y axis labels. Use ylab = FALSE to
#'   hide ylab.
#' @param font.main,font.submain,font.caption,font.x,font.y a vector of length 3
#'   indicating respectively the size (e.g.: 14), the style (e.g.: "plain",
#'   "bold", "italic", "bold.italic") and the color (e.g.: "red") of main title,
#'   subtitle, caption, xlab and ylab, respectively. For example \emph{font.x =
#'   c(14, "bold", "red")}. Use font.x = 14, to change only font size; or use
#'   font.x = "bold", to change only font face.
#' @param font.title,font.subtitle alias of font.submain and font.submain,
#'   respectively.
#' @param xlim,ylim a numeric vector of length 2, specifying  x and y axis
#'   limits (minimum and maximum), respectively. e.g.: ylim = c(0, 50).
#' @param xscale,yscale x and y axis scale, respectively. Allowed values are one
#'   of c("none", "log2", "log10", "sqrt"); e.g.: yscale="log2".
#' @param format.scale logical value. If TRUE, axis tick mark labels will be
#'   formatted when xscale or yscale = "log2" or "log10".
#' @param legend character specifying legend position. Allowed values are one of
#'   c("top", "bottom", "left", "right", "none"). To remove the legend use
#'   legend = "none". Legend position can be also specified using a numeric
#'   vector c(x, y); see details section.
#' @param legend.title legend title.
#' @param font.legend legend text font style; e.g.: font.legend = c(10, "plain",
#'   "black").
#' @param ticks logical value. Default is TRUE. If FALSE, hide axis tick marks.
#' @param tickslab logical value. Default is TRUE. If FALSE, hide axis tick
#'   labels.
#' @param font.tickslab Font style (size, face, color) for tick labels, e.g.:
#'   c(14, "bold", "red").
#' @param xtickslab.rt,ytickslab.rt Rotation angle of x and y axis tick labels,
#'   respectively. Default value is NULL. For vertical x axis texts use xtickslab.rt = 90.
#' @param xticks.by,yticks.by numeric value controlling x and y axis breaks,
#'   respectively. For example, if yticks.by = 5, a tick mark is shown on every
#'   5. Default value is NULL.
#' @param orientation change the orientation of the plot. Allowed values are one
#'   of c( "vertical", "horizontal", "reverse"). Partial match is allowed.
#' @param ggtheme function, ggplot2 theme name. Default value is theme_pubr().
#'   Allowed values include ggplot2 official themes: theme_gray(), theme_bw(),
#'   theme_minimal(), theme_classic(), theme_void(), ....
#' @param ... not used
#' @examples
#' # Load data
#' data("ToothGrowth")
#' df <- ToothGrowth
#'
#' # Basic box plot
#' # +++++++++++++++++++++++++++
#'
#' p <- ggboxplot(df, x = "dose", y = "len")
#'
#' # Change the plot orientation: horizontal
#' ggpar(p, orientation = "horiz")
#'
#'
#'  # Change main title and axis labels
#'  # ++++++++++++++++++++++++++++
#'
#'  ggpar(p,
#'    main = "Plot of length \n by dose",
#'    xlab = "Dose (mg)", ylab = "Length")
#'
#'  # Title font styles: 'plain', 'italic', 'bold', 'bold.italic'
#'  ggpar(p,
#'    main = "Length by dose",
#'    font.main = c(14,"bold.italic", "red"),
#'    font.x = c(14, "bold", "#2E9FDF"),
#'    font.y = c(14, "bold", "#E7B800"))
#'
#'  # Hide axis labels
#'  ggpar(p, xlab = FALSE, ylab = FALSE)
#'
#'
#' # Change colors
#' # ++++++++++++++++++++++
#'
#' # Change outline colors by groups: dose
#'  p2 <- ggboxplot(df, "dose", "len", color = "dose")
#'  p2
#'
#' # Use custom color palette
#' ggpar(p2, palette = c("#00AFBB", "#E7B800", "#FC4E07"))
#'
#' # Use brewer palette
#' ggpar(p2, palette = "Dark2" )
#'
#' # Use grey palette
#' ggpar(p2, palette = "grey")
#'
#' # Use scientific journal palette from ggsci package
#' ggpar(p2, palette = "npg") # nature
#'
#' # Axis ticks, limits, scales
#' # +++++++++++++++++++++++++
#'
#' # Axis ticks labels and rotation
#' ggpar(p,
#'  font.tickslab = c(14,"bold", "#993333"),
#'  xtickslab.rt = 45, ytickslab.rt = 45)
#' # Hide axis ticks and tick labels
#' ggpar(p, ticks = FALSE, tickslab = FALSE)
#'
#' # Axis limits
#' ggpar(p, ylim = c(0, 50))
#'
#' # Axis scale
#' ggpar(p, yscale = "log2")
#'
#' # Format axis scale
#' ggpar(p, yscale = "log2", format.scale = TRUE)
#'
#' # Legends
#' # ++++++++++++++++++
#' # Change legend position and title
#' ggpar(p2,
#'  legend = "right", legend.title = "Dose (mg)",
#'  font.legend = c(10, "bold", "red"))
#'
#' @export
ggpar <- function(p, palette = NULL, gradient.cols = NULL,
                  main = NULL, submain = NULL, caption = NULL, xlab = NULL, ylab = NULL,
                  title = NULL, subtitle = NULL,
                  font.main = NULL, font.submain = NULL, font.x = NULL, font.y = NULL, font.caption = NULL,
                  font.title = NULL, font.subtitle = NULL,
                  xlim = NULL, ylim = NULL,
                  xscale = c("none", "log2", "log10", "sqrt"),
                  yscale = c("none", "log2", "log10", "sqrt"),
                  format.scale = FALSE,
                  legend = NULL,
                  legend.title = NULL, font.legend = NULL,
                  ticks = TRUE, tickslab = TRUE, font.tickslab = NULL,
                  xtickslab.rt = NULL, ytickslab.rt = NULL,
                  xticks.by = NULL, yticks.by = NULL,
                  orientation = c("vertical", "horizontal", "reverse"),
                  ggtheme = NULL,
                  ...)
  {

  original.p <- p

  if(is.ggplot(original.p)) list.plots <- list(original.p)
  else if(is.list(original.p)) list.plots <- original.p
  else stop("Can't handle an object of class ", class (original.p))
  if(!is.null(title)) main <- title
  if(!is.null(subtitle)) submain <- subtitle
  if(!is.null(font.title)) font.main <- font.title
  if(!is.null(font.subtitle)) font.submain <- font.subtitle
  if(is.numeric(palette)) palette <- grDevices::palette()[palette]


  for(i in 1:length(list.plots)){
    p <- list.plots[[i]]
    if(is.ggplot(p)){
        p <- p + .ggcolor(palette)+
          .ggfill(palette)
        if(!is.null(ggtheme)) p <- p + ggtheme # labs_pubr() +
        if(!is.null(gradient.cols)) p <- p + .gradient_col(gradient.cols)

        p <- p +.set_ticks(ticks, tickslab, font.tickslab,
                     xtickslab.rt, ytickslab.rt)
        p <- .set_ticksby(p, xticks.by, yticks.by)
        p <- p + .set_axis_limits(xlim, ylim)
        p <-.set_legend(p, legend, legend.title, font.legend)
        p <- .set_scale(p, xscale = xscale, yscale = yscale, format.scale = format.scale)
        p <- .labs(p, main, xlab, ylab,
                     font.main, font.x, font.y,
                   submain = submain, caption = caption, font.submain = font.submain, font.caption = font.caption)
        p <- .set_orientation(p, orientation)
        list.plots[[i]] <- p
    }

  }

  if(is.ggplot(original.p)) list.plots[[1]]
  else list.plots
}

