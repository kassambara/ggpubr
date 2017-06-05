#'@include utilities.R ggpar.R
NULL
#'Plot Paired Data
#'@description Plot paired data.
#'@inheritParams ggboxplot
#'@param cond1 variable name corresponding to the first condition.
#'@param cond2 variable name corresponding to the second condition.
#'@param x,y x and y variables, where x is a grouping variable and y contains
#'  values for each group. Considered only when \code{cond1} and \code{cond2}
#'  are missing.
#'@param id variable name corresponding to paired samples' id. Used to connect
#'  paired points with lines.
#'@param color points and box plot colors. To color by conditions, use color =
#'  "condition".
#'@param fill box plot fill color. To change fill color by conditions, use fill
#'  = "condition".
#'@param line.color line color.
#'@param point.size,line.size point and line size, respectively.
#'@param width box plot width.
#'@param ... other arguments to be passed to be passed to \link{ggpar}().
#' @examples
#'
#'# Example 1
#'#::::::::::::::::::::::::::::::::::::::::::
#' before <-c(200.1, 190.9, 192.7, 213, 241.4, 196.9, 172.2, 185.5, 205.2, 193.7)
#' after <-c(392.9, 393.2, 345.1, 393, 434, 427.9, 422, 383.9, 392.3, 352.2)
#'
#' d <- data.frame(before = before, after = after)
#' ggpaired(d, cond1 = "before", cond2 = "after",
#'     fill = "condition", palette = "jco")
#'
#'# Example 2
#'#::::::::::::::::::::::::::::::::::::::::::
#'ggpaired(ToothGrowth, x = "supp", y = "len",
#'  color = "supp", line.color = "gray", line.size = 0.4,
#'  palette = "npg")
#'
#'@export
ggpaired <- function(data, cond1, cond2, x = NULL, y = NULL, id = NULL,
                     color = "black", fill = "white", palette = NULL,
                     width = 0.5, point.size = 1.2, line.size = 0.5, line.color = "black",
                     title = NULL, xlab = "Condition", ylab = "Value",
                     facet.by = NULL, panel.labs = NULL, short.panel.labs = TRUE,
                     label = NULL, font.label = list(size = 11, color = "black"),
                     label.select = NULL, repel = FALSE, label.rectangle = FALSE,
                     ggtheme = theme_pubr(),
                     ...){


  grouping.vars <- c(x, color, fill) %>%
    unique() %>%
    intersect(colnames(data))

  if(!missing(cond1) & !missing(cond2)){
    data <- data %>%
      tidyr::gather_(key_col = "condition", value_col = "val",
                     gather_cols = c(cond1, cond2))
    data$condition<- factor(data$condition, levels = c(cond1, cond2))
    x <- "condition"
    y <- "val"
  }
  else if(!is.null(x) & !is.null(y)){
    if(missing(xlab)) xlab <- x
    if(missing(ylab)) ylab <- y
  }


  # Default options
  #:::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::
  .opts <- list(
    id = id,
    color = color, fill = fill, palette = palette,
    width = width, point.size = point.size, line.size = line.size, line.color = line.color,
    title = title, xlab = xlab, ylab = ylab,
    facet.by = facet.by, panel.labs = panel.labs, short.panel.labs = short.panel.labs,
    label = label, font.label = font.label, label.select = label.select,
    repel = repel, label.rectangle = label.rectangle, ggtheme = ggtheme, ...)

  # User options
  #:::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::
  .user.opts <- as.list(match.call(expand.dots = TRUE))
  .user.opts[[1]] <- NULL # Remove the function name
  # keep only user arguments
  for(opt.name in names(.opts)){
    if(is.null(.user.opts[[opt.name]]))
      .opts[[opt.name]] <- NULL
  }
  .opts$data <- data
  .opts$x <- x
  .opts$y <- y


  .opts$fun <- ggpaired_core
  if(missing(ggtheme) & (!is.null(facet.by)))
    .opts$ggtheme <- theme_pubr(border = TRUE)
  p <- do.call(.plotter, .opts)

  if(.is_list(p) & length(p) == 1) p <- p[[1]]
  return(p)

}

ggpaired_core <- function(data, x = NULL, y = NULL, id = NULL,
                      color = "black", fill = "white", palette = NULL,
                      width = 0.5, point.size = 1.2, line.size = 0.5, line.color = "black",
                      title = NULL, xlab = "Condition", ylab = "Value",
                      ggtheme = theme_pubr(),
                        ...)
{

  if(!is.factor(data[, x])) data[, x] <- as.factor(data[, x])

  grouping.vars <- c(x, color, fill) %>%
    unique() %>%
    intersect(colnames(data))


  # Add paired sample ids
  if(!is.null(id)) id <- .select_vec(data, id)
  else id <-  rep(1:(nrow(data)/2), 2)
  data$id <-  id


  position <- "identity"
  # if(length(grouping.vars) > 1)
  #   position <- position_dodge(0.8)

  condition <- val <- id <- NULL
  p <- ggplot(data, aes_string(x = x, y = y)) +
    geom_exec(geom_boxplot, data = data, color = color, fill = fill, width = width,
              position = position)+
    geom_line(aes(group= id),  color = line.color, size = line.size, position = position )+
    geom_exec(geom_point, data = data, color = color, size = point.size,
              position = position)

  p <- ggpar(p, palette = palette, ggtheme = ggtheme, xlab = xlab, ylab = ylab, title = title, ...)

  p
}
