#' @include utilities.R ggpar.R
NULL
#' Density plot
#' @description Create a density plot.
#' @inheritParams ggboxplot
#' @param x variable to be drawn.
#' @param y one of "density" or "count".
#' @param color,fill density line color and fill color.
#' @param linetype line type. See \code{\link{show_line_types}}.
#' @param alpha numeric value specifying fill color transparency. Value should
#'   be in [0, 1], where 0 is full transparency and 1 is no transparency.
#' @param add allowed values are one of "mean" or "median" (for adding mean or
#'   median line, respectively).
#' @param add.params parameters (color, size, linetype) for the argument 'add';
#'   e.g.: add.params = list(color = "red").
#' @param rug logical value. If TRUE, add marginal rug.
#' @param ... other arguments to be passed to
#'   \code{\link[ggplot2]{geom_density}} and \code{\link{ggpar}}.
#' @details The plot can be easily customized using the function ggpar(). Read
#'   ?ggpar for changing: \itemize{ \item main title and axis labels: main,
#'   xlab, ylab \item axis limits: xlim, ylim (e.g.: ylim = c(0, 30)) \item axis
#'   scales: xscale, yscale (e.g.: yscale = "log2") \item color palettes:
#'   palette = "Dark2" or palette = c("gray", "blue", "red") \item legend title,
#'   labels and position: legend = "right" \item plot orientation : orientation
#'   = c("vertical", "horizontal", "reverse") }
#' @seealso \code{\link{gghistogram}} and \code{\link{ggpar}}.
#'
#' @examples
#' # Create some data format
#' set.seed(1234)
#' wdata = data.frame(
#'    sex = factor(rep(c("F", "M"), each=200)),
#'    weight = c(rnorm(200, 55), rnorm(200, 58)))
#'
#' head(wdata, 4)
#'
#' # Basic density plot
#'  # Add mean line and marginal rug
#' ggdensity(wdata, x = "weight", fill = "lightgray",
#'    add = "mean", rug = TRUE)
#'
#' # Change outline colors by groups ("sex")
#' # Use custom palette
#' ggdensity(wdata, x = "weight",
#'    add = "mean", rug = TRUE,
#'    color = "sex", palette = c("#00AFBB", "#E7B800"))
#'
#'
#' # Change outline and fill colors by groups ("sex")
#' # Use custom palette
#' ggdensity(wdata, x = "weight",
#'    add = "mean", rug = TRUE,
#'    color = "sex", fill = "sex",
#'    palette = c("#00AFBB", "#E7B800"))
#'
#'
#' @export
ggdensity <- function(data, x, y = "density", combine = FALSE, merge = FALSE,
                      color = "black", fill = NA, palette = NULL,
                      size = NULL, linetype = "solid", alpha = 0.5,
                      title = NULL, xlab = NULL, ylab = NULL,
                      facet.by = NULL, panel.labs = NULL, short.panel.labs = TRUE,
                      add = c("none", "mean", "median"),
                      add.params = list(linetype = "dashed"),
                      rug = FALSE,
                      label = NULL, font.label = list(size = 11, color = "black"),
                      label.select = NULL, repel = FALSE, label.rectangle = FALSE,
                      ggtheme = theme_pubr(),
                      ...){

  # Default options
  #:::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::
  .opts <- list(
    combine = combine, merge = merge,
    color = color, fill = fill, palette = palette,
    linetype = linetype, size = size, alpha = alpha,
    title = title, xlab = xlab, ylab = ylab,
    facet.by = facet.by, panel.labs = panel.labs, short.panel.labs = short.panel.labs,
    add = add, add.params = add.params, rug = rug,
    label = label, font.label = font.label, label.select = label.select,
    repel = repel, label.rectangle = label.rectangle, ggtheme = ggtheme, ...)
  if(!missing(data)) .opts$data <- data
  if(!missing(x)) .opts$x <- x
  if(!missing(y)) .opts$y <- y

  # User options
  #:::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::
  .user.opts <- as.list(match.call(expand.dots = TRUE))
  .user.opts[[1]] <- NULL # Remove the function name
  # keep only user arguments
  for(opt.name in names(.opts)){
    if(is.null(.user.opts[[opt.name]]))
      .opts[[opt.name]] <- NULL
  }

  .opts$fun <- ggdensity_core
  if(missing(ggtheme) & (!is.null(facet.by) | combine))
    .opts$ggtheme <- theme_pubr(border = TRUE)
  if(missing(y)) .opts$y <- y
  if(missing(add.params)) .opts$add.params <- add.params
  p <- do.call(.plotter, .opts)

  if(.is_list(p) & length(p) == 1) p <- p[[1]]
  return(p)
}

ggdensity_core <- function(data, x, y = "density",
                      color = "black", fill = NA, palette = NULL,
                      size = NULL, linetype = "solid", alpha = 0.5,
                      title = NULL, xlab = NULL, ylab = NULL,
                      add = c("none", "mean", "median"),
                      add.params = list(linetype = "dashed"),
                      rug = FALSE,
                      facet.by = NULL,
                      ggtheme = theme_classic(),
                      ...)
{

  grouping.vars <- grp <- c(color, fill, linetype, size, alpha, facet.by) %>%
    unique() %>%
    intersect(colnames(data))

  add <- match.arg(add)
  add.params <- .check_add.params(add, add.params, error.plot = "", data, color, fill, ...)
  if(is.null(add.params$size)) add.params$size <- size
  if(is.null(add.params$linetype)) add.params$linetype <- linetype
  if (y %in% c("..density..", "density")) y <- "after_stat(density)"
  else if (y %in% c("..count..", "count")) y <- "after_stat(count)"

  p <- ggplot(data, create_aes(list(x = x, y = y)))

  p <- p +
       geom_exec(geom_density, data = data,
                 color = color, fill = fill, size = size,
                 linetype = linetype, alpha = alpha, ...)

  # Add mean/median
  if(add %in% c("mean", "median")){
    p <- p %>% .add_center_line(add = add, grouping.vars = grouping.vars, color = add.params$color,
                                linetype = add.params$linetype, size = add.params$size)
  }

  # Add marginal rug
  if(rug) {

    grps <- c(color, fill, linetype, size, alpha) %>%
      unique() %>% intersect(colnames(data))
    alpha <- ifelse(.is_empty(grps), 1, alpha)
    .args <- geom_exec(NULL, data = data,
                              color = color, sides = "b", alpha = alpha)
    mapping <- .args$mapping
    mapping[["y"]] <- 0
    option <- .args$option
    option[["mapping"]] <- create_aes(mapping)
    p <- p + do.call(geom_rug, option)
  }


  p <- ggpar(p, palette = palette, ggtheme = ggtheme,
             title = title, xlab = xlab, ylab = ylab,...)
  p
}



