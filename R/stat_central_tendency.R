#' @include utilities.R
NULL
#' Add Central Tendency Measures to a GGPLot
#'
#' @description Add central tendency measures (mean, median, mode) to density
#'   and histogram plots created using ggplots.
#'
#'   Note that, normally, the mode is used for categorical data where we wish to
#'   know which is the most common category. Therefore, we can have have two or
#'   more values that share the highest frequency. This might be problematic for
#'   continuous variable.
#'
#'   For continuous variable, we can consider using mean or median as the
#'   measures of the central tendency.
#' @inheritParams ggplot2::layer
#' @param ... other arguments to pass to \code{\link[ggplot2]{geom_line}}.
#' @param na.rm If FALSE (the default), removes missing values with a warning.
#'   If TRUE silently removes missing values.
#' @param type the type of central tendency measure to be used. Possible values
#'   include: \code{"mean", "median", "mode"}.
#' @seealso \code{\link{ggdensity}}
#' @examples
#' # Simple density plot
#' data("mtcars")
#' ggdensity(mtcars, x = "mpg", fill = "red") +
#'   scale_x_continuous(limits = c(-1, 50)) +
#'   stat_central_tendency(type = "mean", linetype = "dashed")
#'
#' # Color by groups
#' data(iris)
#' ggdensity(iris, "Sepal.Length", color = "Species") +
#'   stat_central_tendency(aes(color = Species), type = "median", linetype = 2)
#'
#' # Use geom = "point" for central tendency
#' data(iris)
#' ggdensity(iris, "Sepal.Length", color = "Species") +
#'   stat_central_tendency(
#'      aes(color = Species), type = "median",
#'      geom = "point", size = 4
#'      )
#'
#' # Facet
#' ggdensity(iris, "Sepal.Length", facet.by = "Species") +
#'   stat_central_tendency(type = "mean", color = "red", linetype = 2) +
#'   stat_central_tendency(type = "median", color = "blue", linetype = 2)
#'
#' @export
stat_central_tendency <- function(mapping = NULL, data = NULL, geom = c("line", "point"),
                    position = "identity", na.rm = FALSE, show.legend = NA,
                    inherit.aes = TRUE,
                    type = c("mean", "median", "mode"),
                    ...) {
  type <- match.arg(type)
  geom <- match.arg(geom)
  if(is.null(mapping)){
    mapping <- ggplot2::aes(y = NULL)
  }else{
    mapping["y"] <- list(NULL)
  }
  layer(
    stat = StatCentralTendency, data = data, mapping = mapping, geom = geom,
    position = position, show.legend = show.legend, inherit.aes = inherit.aes,
    params = list(na.rm = na.rm,  type = type, geom = geom, ...)
  )
}

StatCentralTendency <- ggproto("StatCentralTendency", Stat,
                  required_aes = c("x"),
                  compute_group = function(data, scales, type, geom) {
                    center.func <- switch (type,
                      mean = mean, median = stats::median, mode = .get_mode
                    )
                    .center <- center.func(data$x, na.rm = TRUE)
                    n <- length(.center)
                    if(geom == "line"){
                      results <- data.frame(x = rep(.center, each = 2),y = c(-Inf, Inf))
                      if(n >= 2) results$group <- rep(1:n, each = 2)
                    }
                    else if(geom == "point") {
                      results <- data.frame(x = .center, y = -Inf)
                    }
                    results
                  }
)


# from rstatix
.get_mode <- function (x, na.rm = TRUE)
{
  if(na.rm) x <- stats::na.omit(x)
  .x <- factor(x)
  .table <- table(.x)
  .max <- max(.table)
  if (all(.table == .max)) {
    .mode <- NA
  }
  else {
    .mode <- names(.table)[.table == .max]
  }
  if (is.numeric(x)) {
    .mode <- as.numeric(.mode)
  }
  .mode
}
