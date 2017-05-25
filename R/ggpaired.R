#'@include utilities.R ggpar.R
NULL
#'Plot Paired Data
#'@description Plot paired data.
#'@inheritParams ggboxplot
#'@param cond1 variable name corresponding to the first condition.
#'@param cond2 variable name corresponding to the second condition.
#'@param color points and boxs plot colors. To color by conditions, use color = "condition".
#'@param fill box plot fill color. To change fill color by conditions, use fill = "condition".
#'@param line.color line color.
#'@param point.size,line.size point and line size, respectively.
#'@param width box plot width.
#'@param ... other arguments to be passed to be passed to \link{ggpar}().
#' @examples
#'
#' before <-c(200.1, 190.9, 192.7, 213, 241.4, 196.9, 172.2, 185.5, 205.2, 193.7)
#' after <-c(392.9, 393.2, 345.1, 393, 434, 427.9, 422, 383.9, 392.3, 352.2)
#'
#' d <- data.frame(before = before, after = after)
#' ggpaired(d, cond1 = "before", cond2 = "after",
#'     fill = "condition", palette = "jco")
#'
#'
#' @export
ggpaired <- function(data, cond1, cond2,
                        color = "black", fill = "white", palette = NULL,
                        width = 0.5, point.size = 1.2, line.size = 0.5, line.color = "black",
                        title = NULL, xlab = "Condition", ylab = "Value",
                        ggtheme = theme_pubr(),
                        ...)
{

  data <- data %>%
    tidyr::gather_(key_col = "condition", value_col = "val",
                   gather_cols = c(cond1, cond2)) %>%
    dplyr::mutate(id = rep(1:nrow(data), 2))

  data$condition<- factor(data$condition, levels = c(cond1, cond2))

  condition <- val <- id <- NULL
  p <- ggplot(data, aes(x = condition, y = val)) +
    geom_exec(geom_boxplot, data = data, color = color, fill = fill, width = width)+
    geom_line(aes(group= id),  color = line.color, size = line.size)+
    geom_exec(geom_point, data = data, color = color, size = point.size)

  p <- ggpar(p, palette = palette, ggtheme = ggtheme, xlab = xlab, ylab = ylab, title = title, ...)

  p
}

