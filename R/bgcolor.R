#'Change ggplot Panel Background Color
#'
#' @description Change ggplot panel background color.
#' @param color background color.
#'
#' @seealso \link{border}().
#'
#'@examples
#'# Load data
#'data("ToothGrowth")
#'df <- ToothGrowth
#'
#'# Basic plot
#'ggboxplot(df, x = "dose", y = "len")
#'p
#'
#'# Change panel background color
#' p +
#'   bgcolor("#BFD5E3")+
#'   border("#BFD5E3")
#'@export
bgcolor <- function(color){
   theme(panel.background = element_rect(fill = color))
}
