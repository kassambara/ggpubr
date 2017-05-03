#' @include utilities.R
NULL
#' Facet a ggplot into Multiple Panels
#' @description Create multi-panel plots of a data set grouped by one or two
#'   grouping variables. Wrapper around \code{\link[ggplot2]{facet_wrap}}
#'@param p a ggplot
#'@param facet.by character vector, of length 1 or 2, specifying grouping
#'  variables for faceting the plot. Should be in the data.
#'@param nrow,ncol Number of rows and columns in the pannel. Used only when the
#'  data is faceted by one grouping variable.
#'@param scales should axis scales of panels be fixed ("fixed", the default),
#'  free ("free"), or free in one dimension ("free_x", "free_y").
#'@param short.panel.labs logical value. Default is FALSE. If TRUE, create short
#'  labels for panels by omitting variable names; in other words panels will be
#'  labelled only by variable grouping levels.
#'@param panel.labs a list of one or two character vectors to modify facet label
#'  text. For example, panel.labs = list(sex = c("Male", "Female")) specifies
#'  the labels for the "sex" variable. For two grouping variables, you can use
#'  for example panel.labs = list(sex = c("Male", "Female"), rx = c("Obs",
#'  "Lev", "Lev2") ).
#'@param panel.labs.background a list to customize the background of panel
#'  labels. Should contain the combination of the following elements: \itemize{
#'  \item \code{color, linetype, size}: background line color, type and size
#'  \item \code{fill}: background fill color. } For example,
#'  panel.labs.background = list(color = "blue", fill = "pink").
#'@param panel.labs.font a list of aestheics indicating the size (e.g.: 14), the
#'  face/style (e.g.: "plain", "bold", "italic", "bold.italic") and the color
#'  (e.g.: "red") and the orientation angle (e.g.: 45) of panel labels.
#'@param panel.labs.font.x,panel.labs.font.y same as panel.labs.font but for x
#'  and y direction, respectively.
#' @examples
#' p <- ggboxplot(ToothGrowth, x = "dose", y = "len",
#'       color = "supp")
#' print(p)
#'
#' facet(p, facet.by = "supp")
#' @rdname facet
#' @export
facet <- function(p,  facet.by, nrow = NULL, ncol = NULL,
                   scales = "fixed", short.panel.labs = FALSE,
                   panel.labs.background = list(color = NULL, fill = NULL),
                   panel.labs.font = list(face = NULL, color = NULL, size = NULL, angle = NULL),
                   panel.labs.font.x = panel.labs.font,
                   panel.labs.font.y = panel.labs.font
)
{

  panel.labs.background <- .compact(panel.labs.background)
  panel.labs.font.x <- .compact(panel.labs.font.x)
  panel.labs.font.y <- .compact(panel.labs.font.y)

  .labeller <- "label_value"
  if(!short.panel.labs) .labeller <- label_both

  if(length(facet.by) == 1){
    facet.formula <- paste0("~", facet.by) %>% stats::as.formula()
    p <- p + facet_wrap(facet.formula, nrow = nrow, ncol = ncol, scales = scales, labeller = .labeller)
  }
  else if(length(facet.by) == 2){
    facet.formula <- paste(facet.by, collapse = " ~ ") %>% stats::as.formula()
    p <- p + facet_grid(facet.formula, scales = scales, labeller = .labeller)
  }

  if(!.is_empty(panel.labs.background))
    p <- p + theme(strip.background = do.call(element_rect, panel.labs.background))
  if(!.is_empty(panel.labs.font.x))
    p <- p + theme(strip.text.x = do.call(element_text, panel.labs.font.x))
  if(!.is_empty(panel.labs.font.y))
    p <- p + theme(strip.text.y = do.call(element_text, panel.labs.font.y))

  p
}
