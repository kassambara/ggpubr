#' @include utilities.R
NULL
#'Scatter Plot with Marginal Histograms
#'@description Create a scatter plot with marginal histograms, density plots or
#'  box plots.
#'@inheritParams ggscatter
#'@param main.plot.size the width of the main plot. Default is 2.
#'@param margin.plot.size the width of the marginal plot. Default is 1.
#'@param group a grouping variable. Change points color and shape by groups if
#'  the options \code{color} and \code{shape} are missing. Should be also
#'  specified when you want to create a marginal box plot that is grouped.
#'@param margin.plot the type of the marginal plot. Default is "hist".
#'@param margin.params parameters to be applied to the marginal plots.
#'@param margin.ggtheme the theme of the marginal plot. Default is
#'  \code{\link[ggplot2:ggtheme]{theme_void}()}.
#'@param margin.space logical value. If TRUE, adds space between the main plot
#'  and the marginal plot.
#'@param bins Number of histogram bins. Defaults to 30. Pick a better value that
#'  fit to your data.
#'@param linetype line type ("solid", "dashed", ...)
#'@param legend specify the legend position. Allowed values include: "top",
#'  "bottom", "left", "right".
#'@param ggtheme the theme to be used for the scatter plot. Default is
#'  \code{\link{theme_pubr}()}.
#'@param print logical value. If \code{TRUE} (default), print the plot.
#'@param ... other arguments passed to the function \code{\link{ggscatter}()}.
#'@return an object of class \code{ggscatterhist}, which is list of ggplots,
#'  including the following elements: \itemize{\item sp: main scatter plot;
#'  \item xplot: marginal x-axis plot; \item yplot: marginal y-axis plot. }.
#'
#'  User can modify each of plot before printing.
#'
#' @examples
#' # Basic scatter plot with marginal density plot
#' ggscatterhist(iris, x = "Sepal.Length", y = "Sepal.Width",
#'               color = "#00AFBB",
#'               margin.params = list(fill = "lightgray"))
#'
#'
#' # Grouped data
#'ggscatterhist(
#'  iris, x = "Sepal.Length", y = "Sepal.Width",
#'  color = "Species", size = 3, alpha = 0.6,
#'  palette = c("#00AFBB", "#E7B800", "#FC4E07"),
#'  margin.params = list(fill = "Species", color = "black", size = 0.2)
#')
#'
#'# Use boxplot as marginal
#'ggscatterhist(
#'  iris, x = "Sepal.Length", y = "Sepal.Width",
#'  color = "Species", size = 3, alpha = 0.6,
#'  palette = c("#00AFBB", "#E7B800", "#FC4E07"),
#'  margin.plot = "boxplot",
#'  ggtheme = theme_bw()
#')
#'
#'# Add vertical and horizontal line to a ggscatterhist
#'plots <- ggscatterhist(iris, x = "Sepal.Length", y = "Sepal.Width", print = FALSE)
#'plots$sp <- plots$sp +
#'  geom_hline(yintercept = 3, linetype = "dashed", color = "blue") +
#'  geom_vline(xintercept = 6, linetype = "dashed", color = "red")
#'plots
#'
#'@export
ggscatterhist <- function(
  data, x, y, group = NULL,
  color = "black", fill = NA, palette = NULL,
  shape = 19, size = 2,
  linetype = "solid", bins = 30,
  margin.plot = c("density", "histogram",  "boxplot"),
  margin.params = list(),
  margin.ggtheme = theme_void(),
  margin.space = FALSE,
  main.plot.size = 2,
  margin.plot.size = 1,
  title = NULL, xlab = NULL, ylab = NULL,
  legend = "top", ggtheme = theme_pubr(),
  print = TRUE, ...
)
{

  if(!has_cowplot_v0.9()){
    warning("Install the latest developmental version of cowplot on github ",
            "to fully use all the feature of ggscatterhist",
            .call = FALSE)
    # margin.space = TRUE
  }

  margin.plot <- match.arg(margin.plot)
  margin.params <- .check_margin_params(
    margin.params, data, color, fill, linetype
  )
  if(margin.plot == "histogram") {
    if(is.null(margin.params$position)){
      margin.params$position <-  "identity"
    }
    margin.params <- margin.params %>%
      .add_item(bins = bins)
  }

  if(!is.null(group)){
    if(missing(color)) color = group
    if(missing(shape)) shape = group
  }

  . <- NULL

  sp <- ggscatter(
    data, x, y,
    color = color, fill = fill,
    palette = palette,
    shape = shape, size = size,
    xlab = xlab, ylab = ylab,
    ggtheme = ggtheme, title = title,
    legend = legend,
    ...
  )

  # Type of graphics to be added in the margins
  geomfunc <- switch (margin.plot,
                      histogram = geom_histogram,
                      density = geom_density,
                      boxplot = geom_boxplot,
                      geom_histogram
  )

  # Define the x and the y variables depending on
  # the geometry used in margins
  if(margin.plot %in% c("density", "histogram")){
    xplot.x <- x
    xplot.y <- NULL
    yplot.x <- y
    yplot.y <- NULL
  }
  else if(margin.plot %in% c("boxplot")){
    if(is.null(group)) {
      data <- data %>%
        mutate(.xgroupx. = factor(1))
      group = ".xgroupx."
    }
    xplot.x <- group
    xplot.y <- x
    yplot.x <- group
    yplot.y <- y
  }

  # Create the different marginal plot
  xplot <- ggplot() + margin.params %>%
    .add_item(geomfunc = geomfunc, data = data,
              x = xplot.x, y = xplot.y, alpha = 0.7) %>%
    do.call(geom_exec, .)
  xplot <- set_palette(xplot, palette)

  yplot <- ggplot() + margin.params %>%
    .add_item(geomfunc = geomfunc, data = data,
              x = yplot.x, y = yplot.y, alpha = 0.7) %>%
    do.call(geom_exec, .)
  yplot <- set_palette(yplot, palette)

  # Flip the marginal plots
  if(margin.plot %in% c("density", "histogram"))
    yplot <- yplot +
    coord_flip()
  else if(margin.plot %in% c("boxplot"))
    xplot <- xplot + coord_flip()

  # Cleaning the plot
  .legend <- get_legend(sp)
  sp <- sp + theme(plot.margin = grid::unit(c(0,0,0.25,0.25), "cm"))
  xplot <- xplot + margin.ggtheme + clean_theme() + rremove("legend") +
    theme( plot.margin = grid::unit(c(0,0,0,0), "cm"))
  yplot <- yplot + margin.ggtheme + clean_theme() + rremove("legend") +
    theme(plot.margin = grid::unit(c(0,0,0,0), "cm"))

  plots <- list(sp = sp, xplot = xplot, yplot = yplot)
  class(plots) <- c("ggscatterhist",  "list")
  if(print){
    res <- print(
      plots, margin.space = margin.space,
      main.plot.size = main.plot.size, margin.plot.size = margin.plot.size,
      title = title, legend = legend
      )
  }
  invisible(plots)
}

#' @method print ggscatterhist
#' @param x an object of class \code{ggscatterhist}.
#' @rdname ggscatterhist
#' @export
print.ggscatterhist <- function(x, margin.space = FALSE, main.plot.size = 2,
                                margin.plot.size = 1, title = NULL, legend = "top", ...){

  sp <- x$sp
  xplot <- x$xplot
  yplot <- x$yplot
  .legend <- get_legend(sp)

  if(margin.space){
    common.legend <- FALSE
    if(!is.null(.legend)) common.legend = TRUE
    sp <- sp + theme(
      plot.title = element_blank(),
      plot.subtitle = element_blank()
    )

    fig <- ggarrange(
      xplot, NULL, sp, yplot, ncol = 2, nrow = 2,
      align = "hv", widths = c(main.plot.size, margin.plot.size),
      heights = c(margin.plot.size, main.plot.size),
      common.legend = common.legend, legend = legend
    )
    if(!is.null(title)){
      fig <- annotate_figure(
        fig,
        top = text_grob(title, color = "black", size = 13, face = "bold")
      )
    }
  }
  else{
    if(!is.null(title)) sp <- sp + ggtitle(title)
    fig <- .insert_xaxis_grob(sp, xplot, grid::unit(margin.plot.size/5, "null"), position = "top")
    fig <- .insert_yaxis_grob(fig, yplot, grid::unit(margin.plot.size/5, "null"), position = "right")
    fig <- cowplot::ggdraw(fig)
  }
  print(fig)
  invisible(fig)
}


has_cowplot_v0.9 <- function(){

  vv <- as.character(utils::packageVersion("cowplot"))
  cc <- utils::compareVersion(vv, "0.8.0.8") > 0
  cc
}

.check_margin_params <- function(params, data, color = "black", fill = NA, linetype = "slid"){

  if(is.null(params$color)) {

    if(color %in% colnames(data)){
      col.val <- dplyr::pull(data, color)
      if(!is.factor(col.val)) params$color <- "black"
      else params$color <- color
    }
  }

  if(is.null(params$fill)) {

    if(fill %in% colnames(data)){
      fill.val <- .select_vec(data, fill)
      if(!is.factor(fill.val)) params$fill <- "black"
      else params$fill <- fill
    }

  }
  if(is.null(params$linetype)) params$linetype <- linetype
  params
}

# Helper functions to insert marginal plots
#::::::::::::::::::::::::::::::::::::::::::::::::::::
# Use cowplot::insert_xaxis_grob and cowplot::insert_yaxis_grob,
# when 0.9 stable version released

.insert_xaxis_grob <- function (
  plot, grob, height = grid::unit(0.2, "null"),
  position = c("top",  "bottom")
)
  {

  if(inherits(grob, "ggplot"))
    grob <- .get_panel(grob)

    gt <- .plot_to_gtable(plot)
    pp <- gt$layout[gt$layout$name == "panel", ]

    if (position[1] == "top") {
    g <- gtable::gtable_add_rows(gt, height, pp$t - 1)
    g <- gtable::gtable_add_grob(
      g, grob, pp$t, pp$l, pp$t,
      pp$r, clip = "inherit", name = "xaxis-grob-t"
      )
    }
  else {
    g <- gtable::gtable_add_rows(gt, height, pp$b)
    g <- gtable::gtable_add_grob(
      g, grob, pp$b + 1, pp$l,
      pp$b + 1, pp$r, clip = "inherit", name = "xaxis-grob-b"
      )
  }
}

.insert_yaxis_grob <- function (
  plot, grob, width = grid::unit(0.2, "null"),
  position = c("right", "left"))
{

  if(inherits(grob, "ggplot"))
    grob <- .get_panel(grob)

  gt <- .plot_to_gtable(plot)
  pp <- gt$layout[gt$layout$name == "panel", ]

  if (position[1] == "right") {
    g <- gtable::gtable_add_cols(gt, width, pp$r)
    g <- gtable::gtable_add_grob(
      g, grob, pp$t, pp$r + 1,
      pp$b, pp$r + 1, clip = "inherit", name = "yaxis-grob-r"
      )
  }
  else {
    g <- gtable::gtable_add_cols(gt, width, pp$l - 1)
    g <- gtable::gtable_add_grob(
      g, grob, pp$t, pp$l, pp$b,
      pp$l, clip = "inherit", name = "yaxis-grob-l"
      )
  }
}


.plot_to_gtable <- function (plot) {
  if(inherits(plot, "gtable"))
    return(plot)
  else
    ggplot2::ggplotGrob(plot)
}

.get_panel <- function (plot) {
  gt <- .plot_to_gtable(plot)
  panelIndex <- which(gt$layout$name == "panel")
  panel <- gt$grobs[[panelIndex]]
}
