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
#' ggplot() + geom_exec(geom_point, data = mtcars,
#'     x = "mpg", y = "wt", size = "cyl", color = "cyl")
#' }
#' @export
geom_exec <- function (geomfunc = NULL, data = NULL,
                        position = NULL, ...) {
  params <- list(...)

  mapping <-
    list() # option to pass to mapping aes() or aes_string()
  option <- list() # option to the geom_*()

  allowed_options <- c(
    # general
    "x", "y", "color", "colour", "linetype", "fill", "size", "shape", "width",
    "alpha", "na.rm", "lwd", "pch", "cex", "position", "stat", "geom",
    "show.legend", "inherit.aes", "fun.args", "fontface",
    # point
    "stroke",
    # boxplot
    "outlier.colour", "outlier.shape", "outlier.size",
    "outlier.stroke", "notch", "notchwidth", "varwidth",
    # dot plot
    "binwidth", "binaxis", "method", "binpositions",
    "stackdir", "stackratio", "dotsize",
    # Violin
    "trim", "draw_quantiles", "scale",
    # error
    "ymin", "ymax", "xmin", "xmax",
    # text
    "label", "hjust", "vjust", "fontface", "angle", "family",
    # text.repel
    "segment.size", "force",
    # smooth
    "se", "level", "fullrange",
    "conf.int.level",
    # straightline
    "xintercept", "yintercept",
    # histograms
    "bins",
    # rug
    "sides",
    # segment
    "arrow", "xend", "yend",
    # stat_summary,
    "fun.data", "fun.y", "fun.ymin", "fun.ymax",
    # bracket
    "y.position", "tip.length", "label.size", "step.increase",
    "bracket.nudge.y", "bracket.shorten", "coord.flip"


  )

  columns <- colnames(data)
  for (key in names(params)) {
    value <- params[[key]]
    if (is.null(value)) {

    }
    else if (unlist(value)[1] %in% columns & key %in% allowed_options) {
      mapping[[key]] <- value

    }
    else if (key %in% allowed_options) {
      option[[key]] <- value
    }
    else if (key =="group") {
      mapping[[key]] <- value # for line plot
    }
    else if(key == "step.group.by"){
      # for geom_bracket, value are variable name.
      # but this parameter is an option not an aes
      option[[key]] <- value
    }
    # else warnings("Don't know '", key, "'")
  }
  if (!is.null(position))
    option[["position"]] <- position
  option[["data"]] <- data
  if(is.null(geomfunc)){
   res <- list(option = option, mapping = mapping)
  }
  else{
    option[["mapping"]] <- do.call(ggplot2::aes_string, mapping)
    res <- do.call(geomfunc, option)
  }
  res
}
