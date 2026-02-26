#' Execute ggplot2 functions
#' @description A helper function used by ggpubr functions to execute any geom_*
#'   functions in ggplot2. Useful only when you want to call a geom_* function
#'   without carrying about the arguments to put in aes(). Basic users of ggpubr
#'   don't need this function.
#' @param geomfunc a ggplot2 function (e.g.: geom_point)
#' @param data a data frame to be used for mapping
#' @param position Position adjustment, either as a string, or the result of a
#'   call to a position adjustment function.
#' @param ... arguments accepted by the function
#' @return return a plot if geomfunc!=Null or a list(option, mapping) if
#'   geomfunc = NULL.
#' @examples
#' \dontrun{
#' ggplot() +
#'   geom_exec(geom_point,
#'     data = mtcars,
#'     x = "mpg", y = "wt", size = "cyl", color = "cyl"
#'   )
#' }
#' @export
geom_exec <- function(geomfunc = NULL, data = NULL,
                      position = NULL, ...) {
  params <- list(...)

  mapping <-
    list() # option to pass to mapping aes()
  option <- list() # option to the geom_*()

  allowed_options <- c(
    # general
    "x", "y", "color", "colour", "linetype", "fill", "size", "linewidth", "shape", "width",
    "alpha", "na.rm", "lwd", "pch", "cex", "position", "stat", "geom",
    "show.legend", "inherit.aes", "fun.args", "fontface",
    # point
    "stroke",
    # boxplot
    "outlier.colour", "outlier.shape", "outlier.size", "outliers",
    "outlier.stroke", "notch", "notchwidth", "varwidth",
    # dot plot
    "binwidth", "binaxis", "method", "binpositions",
    "stackdir", "stackratio", "dotsize",
    # Violin and density
    "trim", "draw_quantiles", "quantiles", "quantile.linetype", "quantile.type",
    "quantile.alpha", "quantile.colour", "quantile.color", "quantile.linewidth",
    "quantile.size", "scale", "adjust", "bw",
    # error
    "ymin", "ymax", "xmin", "xmax",
    # text
    "label", "hjust", "vjust", "fontface", "angle", "family", "parse",
    # text.repel
    "segment.size", "force", "max.overlaps", "seed",
    # smooth
    "se", "level", "fullrange",
    "conf.int.level",
    # straightline
    "xintercept", "yintercept",
    # histograms
    "bins", "weight",
    # rug
    "sides",
    # segment
    "arrow", "xend", "yend",
    # stat_summary,
    "fun.data", "fun", "fun.min", "fun.max",
    # bracket
    "y.position", "tip.length", "label.size", "step.increase",
    "bracket.nudge.y", "bracket.shorten", "coord.flip"
  )

  columns <- colnames(data)

  # Helper to check for geoms/stats that use linewidth for strokes
  is_line_geom <- function(f) {
    if (is.null(f)) {
      return(FALSE)
    }

    # List of geoms/stats that use linewidth in modern ggplot2
    line_geoms <- list(
      ggplot2::geom_line, ggplot2::geom_path, ggplot2::geom_segment,
      ggplot2::geom_step, ggplot2::geom_density, ggplot2::geom_freqpoly,
      ggplot2::geom_histogram, ggplot2::geom_bar, ggplot2::geom_col,
      ggplot2::geom_smooth, ggplot2::geom_errorbar,
      ggplot2::geom_linerange, ggplot2::geom_vline, ggplot2::geom_hline,
      ggplot2::geom_abline, ggplot2::geom_rug, ggplot2::geom_rect,
      ggplot2::geom_tile, ggplot2::geom_polygon, ggplot2::geom_ribbon,
      ggplot2::geom_area, ggplot2::geom_crossbar, ggplot2::geom_boxplot,
      ggplot2::geom_violin,
      ggplot2::stat_ecdf, ggplot2::stat_qq_line
    )

    for (geom in line_geoms) {
      if (identical(f, geom)) {
        return(TRUE)
      }
    }
    return(FALSE)
  }

  # Auto-convert size to linewidth for line-based geoms
  if (is_line_geom(geomfunc) && "size" %in% names(params) && !"linewidth" %in% names(params)) {
    names(params)[names(params) == "size"] <- "linewidth"
  }

  for (key in names(params)) {
    value <- params[[key]]
    if (is.null(value)) {} else if (unlist(value)[1] %in% columns & key %in% allowed_options) {
      mapping[[key]] <- value
    } else if (key %in% allowed_options) {
      option[[key]] <- value
    } else if (key == "group") {
      mapping[[key]] <- value # for line plot
    } else if (key == "step.group.by") {
      # for geom_bracket, value are variable name.
      # but this parameter is an option not an aes
      option[[key]] <- value
    }
    # else warnings("Don't know '", key, "'")
  }
  if (!is.null(position)) {
    option[["position"]] <- position
  }
  option[["data"]] <- data
  if (is.null(geomfunc)) {
    res <- list(option = option, mapping = mapping)
  } else {
    option[["mapping"]] <- create_aes(mapping)
    res <- do.call(geomfunc, option)
  }
  res
}
