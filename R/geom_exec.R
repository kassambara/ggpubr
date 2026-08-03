#' Execute ggplot2 functions
#' @description A helper function used by ggpubr functions to execute any geom_*
#'   functions in ggplot2. Useful only when you want to call a geom_* function
#'   without worrying about the arguments to put in aes(). Basic users of ggpubr
#'   don't need this function.
#' @param geomfunc a ggplot2 function (e.g.: geom_point)
#' @param data a data frame to be used for mapping
#' @param position Position adjustment, either as a string, or the result of a
#'   call to a position adjustment function.
#' @param ... arguments accepted by the function
#' @return a plot if geomfunc != NULL or a list(option, mapping) if
#'   geomfunc is NULL.
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
    "outlier.stroke", "notch", "notchwidth", "varwidth", "coef",
    # dot plot
    "binwidth", "binaxis", "method", "binpositions",
    "stackdir", "stackratio", "dotsize",
    # Violin and density
    "trim", "drop", "draw_quantiles", "quantiles", "quantile.linetype", "quantile.type",
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
    "y.position", "tip.length", "tip.length.ref", "label.size", "step.increase",
    "bracket.nudge.y", "bracket.shorten", "coord.flip"
  )
  target.aesthetics <- NULL
  # A geom's public contract is wider than this historical fallback list. In
  # particular, parameters accepted through the geom's stat (for example
  # histogram boundary/closed/pad and density kernel/n) are not necessarily
  # formals of the geom_* wrapper itself. Ask the layer for both geom and stat
  # parameters so valid documented arguments reach the target instead of being
  # silently discarded. If a custom target cannot be instantiated without
  # arguments, retain the established fallback list.
  if (!is.null(geomfunc)) {
    # A stat wrapper can select a different concrete geom (and a geom wrapper a
    # different stat). Include those selectors in the probe so target-specific
    # aesthetics such as stat_summary(geom = "errorbar") width are discoverable.
    probe.args <- params[intersect(names(params), c("geom", "stat"))]
    target.layer <- tryCatch(
      suppressWarnings(do.call(geomfunc, probe.args)),
      error = function(e) NULL
    )
    if (!is.null(target.layer) &&
        !is.null(target.layer$geom) && !is.null(target.layer$stat)) {
      target.aesthetics <- unique(c(
        target.layer$geom$aesthetics(),
        target.layer$stat$aesthetics()
      ))
      target.options <- unique(c(
        names(formals(geomfunc)),
        target.layer$geom$aesthetics(),
        target.layer$geom$parameters(extra = TRUE),
        target.layer$stat$parameters(extra = TRUE),
        names(target.layer$geom_params),
        names(target.layer$stat_params)
      ))
      target.options <- setdiff(target.options, "...")
      allowed_options <- unique(c(allowed_options, target.options))
    }
  }

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

  # Auto-convert size to linewidth for line-based geoms. A `linewidth` that was
  # passed but is NULL must not block the conversion: callers forward
  # `linewidth = <maybe-NULL>` alongside `size`, and a named NULL is dropped
  # later anyway (see the NULL check in the loop below).
  if (is_line_geom(geomfunc) && "size" %in% names(params) &&
      (!"linewidth" %in% names(params) || is.null(params[["linewidth"]]))) {
    params[["linewidth"]] <- NULL
    names(params)[names(params) == "size"] <- "linewidth"
  }

  for (key in names(params)) {
    value <- params[[key]]
    aesthetic.key <- if (key == "color") "colour" else key
    value.names.column <- is.character(value) && length(value) > 0 &&
      value[[1]] %in% columns
    can.map <- if (is.null(target.aesthetics)) {
      key %in% allowed_options
    } else {
      aesthetic.key %in% target.aesthetics
    }
    if (is.null(value)) {} else if (value.names.column && can.map) {
      mapping[[key]] <- value
    } else if (key == "group") {
      # Layer discovery includes `group` among a geom's aesthetics. Keep a
      # constant such as add_summary(group = 1) in aes(), just as a group
      # column is kept there above; passing the constant as a layer parameter
      # would let other inherited discrete aesthetics split the statistic.
      mapping[[key]] <- value
    } else if (key %in% allowed_options) {
      option[[key]] <- value
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
