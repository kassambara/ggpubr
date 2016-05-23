#' @include utilities.R ggpar.R
NULL
#' Line types available in R
#' @description Show line types available in R.
#' @return a ggplot.
#'
#' @seealso \code{\link{ggpar}} and \code{\link{ggline}}.
#' @examples
#' show_line_types()+
#'  theme_minimal()
#' @export
show_line_types <- function()
{
  lt <- c("blank", "solid", "dashed",  "dotted",
          "dotdash", "longdash", "twodash")

  d <- data.frame(lt = factor(lt, levels = lt))

  ggplot() +
    scale_x_continuous(name="", limits=c(0,1), breaks=NULL) +
    scale_linetype_identity() +
    geom_segment(data=d, mapping=aes(x=0, xend=1, y=lt,
                                     yend=lt, linetype=lt))+
    labs(title = "Line types available in R", y = "")+
    theme(axis.text.y = element_text(face="bold", color="black"))
}


