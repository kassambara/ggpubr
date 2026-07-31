#' Change ggplot Panel Background Color
#'
#' @description Change ggplot panel background color.
#'  Read more: \href{https://www.datanovia.com/learn/data-visualization/ggpubr/customize}{Customize ggpubr Plots in R with ggpar()}.
#' @param color background color.
#'
#' @seealso \link{border}().
#'  Read more: \href{https://www.datanovia.com/learn/data-visualization/ggpubr/customize}{Customize ggpubr Plots in R with ggpar()}.
#'
#' @examples
#' # Load data
#' data("ToothGrowth")
#' df <- ToothGrowth
#'
#' # Basic plot
#' p <- ggboxplot(df, x = "dose", y = "len")
#' p
#'
#' # Change panel background color
#' p +
#'   bgcolor("#BFD5E3") +
#'   border("#BFD5E3")
#' @export
bgcolor <- function(color) {
  theme(panel.background = element_rect(fill = color))
}
