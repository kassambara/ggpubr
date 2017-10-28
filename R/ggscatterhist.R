#' @include utilities.R
NULL
#' Scatter Plot with Marginal Histograms
#' @description Create a scatter plot with marginal histograms, density plots or box plots.
#' @inheritParams ggscatter
#' @param main.plot.size the width of the main plot. Default is 2.
#' @param margin.plot.size the width of the marginal plot. Default is 1.
#' @param group a grouping variable. Change points color and shape by groups if
#'   the options \code{color} and \code{shape} are missing. Should be also
#'   specified when you want to create a marginal box plot that is grouped.
#' @param margin.plot the type of the marginal plot. Default is "hist".
#' @param margin.params parameters to be applied to the marginal plots.
#' @param margin.ggtheme the theme of the marginal plot. Default is \code{\link[ggplot2]{theme_void}()}.
#' @param margin.space logical value. If TRUE, adds space between the main plot and the marginal plot.
#' @param bins Number of histogram bins. Defaults to 30. Pick a better value
#'   that fit to your data.
#' @param linetype line type ("solid", "dashed", ...)
#' @param legend specify the legend position. Allowed values include: "top", "bottom", "left", "right".
#' @param ggtheme the theme to be used for the scatter plot. Default is \code{\link{theme_pubr}()}.
#' @param ... other arguments passed to the function \code{\link{ggscatter}()}.
#' @export
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
  legend = "top", ggtheme = theme_pubr(), ...
)
{

  if(!has_cowplot_v0.9()){
    warning("Install the latest developmental version of cowplot on github ",
            "to fully use all the feature of ggscatterhist",
            .call = FALSE)
    margin.space = TRUE
  }

  margin.plot <- match.arg(margin.plot)
  margin.params <- .check_margin_params(
    margin.params, data, color, fill, linetype
  )
  if(margin.plot == "histogram") {
    margin.params <- margin.params %>%
      .add_item(bins = bins, position = "identity")
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

  geomfunc <- switch (margin.plot,
                      histogram = geom_histogram,
                      density = geom_density,
                      boxplot = geom_boxplot,
                      geom_histogram
  )

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


  xplot <- ggplot() + margin.params %>%
    .add_item(geomfunc = geomfunc, data = data,
              x = xplot.x, y = xplot.y, alpha = 0.7) %>%
    do.call(geom_exec, .)
  xplot <- set_palette(xplot, palette)

  yplot <- ggplot() + margin.params %>%
    .add_item(geomfunc = geomfunc, data = data,
              x = xplot.x, y = xplot.y, alpha = 0.7) %>%
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

    if(!is.null(title))
      fig <- annotate_figure(
        fig,
        top = text_grob(title, color = "black", size = 13, face = "bold")
      )
  }
  else{

    fig <- cowplot::insert_xaxis_grob(sp, xplot, grid::unit(margin.plot.size/5, "null"), position = "top")
    fig <- cowplot::insert_yaxis_grob(fig, yplot, grid::unit(margin.plot.size/5, "null"), position = "right")
    fig <- cowplot::ggdraw(fig)
  }

  fig

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
