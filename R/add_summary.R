#' @include utilities.R
NULL
#'Add Summary Statistics onto a ggplot.
#'@description add summary statistics onto a ggplot.
#'@param p a ggplot on which you want to add summary statistics.
#'@param fun a function that is given the complete data and should return a data
#'  frame with variables ymin, y, and ymax. Allowed values are one of: "mean",
#'  "mean_se", "mean_sd", "mean_ci", "mean_range", "median", "median_iqr",
#'  "median_mad", "median_range".
#'@param error.plot plot type used to visualize error. Allowed values are one of
#'  \code{c("pointrange", "linerange", "crossbar", "errorbar", "upper_errorbar",
#'  "lower_errorbar", "upper_pointrange", "lower_pointrange", "upper_linerange",
#'  "lower_linerange")}. Default value is "pointrange".
#'@param color point or outline color.
#'@param fill fill color. Used only whne \code{error.plot = "crossbar"}.
#'@param group grouping variable. Allowed values are 1 (for one group) or a
#'  character vector specifying the name of the grouping variable. Used only for
#'  adding statistical summary per group.
#'@param width numeric value between 0 and 1 specifying bar or box width.
#'  Example width = 0.8. Used only when \code{error.plot} is one of
#'  c("crossbar", "errorbar").
#'@param shape point shape. Allowed values can be displayed using the function
#'  \code{\link{show_point_shapes}()}.
#'@param size numeric value in [0-1] specifying point and line size.
#'@param linetype line type.
#'@param show.legend logical. Should this layer be included in the legends? NA,
#'  the default, includes if any aesthetics are mapped. \code{FALSE} never includes,
#'  and TRUE always includes. It can also be a named logical vector to finely
#'  select the aesthetics to display.
#'@param data a \code{data.frame} to be displayed. If \code{NULL}, the default,
#'  the data is inherited from the plot data as specified in the call to
#'  \link[ggplot2]{ggplot}.
#'@param position position adjustment, either as a string, or the result of a
#'  call to a position adjustment function. Used to adjust position for multiple
#'  groups.
#'@param x a numeric vector.
#'@param ci the percent range of the confidence interval (default is 0.95).
#'@param error.limit allowed values are one of ("both", "lower", "upper",
#'  "none") specifying whether to plot the lower and/or the upper limits of
#'  error interval.
#'@examples
#'
#'# Basic violin plot
#'p <- ggviolin(ToothGrowth, x = "dose", y = "len", add = "none")
#'p
#'
#'# Add median_iqr
#'add_summary(p, "mean_sd")
#'
#'
#'@describeIn add_summary add summary statistics onto a ggplot.
#'@export
add_summary <- function(p, fun = "mean_se", error.plot = "pointrange",
                        color = "black", fill = "white", group = 1,
                        width = NULL, shape = 19, size = 1, linetype = 1,
                        show.legend = NA, ci = 0.95,
                        data = NULL, position = position_dodge(0.8))
  {

  if(is.null(data)) data <- p$data
  if(fun == "mean_se")
    fun <- "mean_se_"

  allowed.fun <- c("mean", "median", "mean_se", "mean_se_", "mean_sd", "mean_ci", "mean_range",
                   "median_iqr", "median_mad", "median_range")
  if(!(fun %in% allowed.fun))
    stop("Don't support ", fun, ". Possibilities for the argument fun are: ",
         .collapse(allowed.fun, sep = ", "))

  allowed.error.plot = c("pointrange", "linerange", "crossbar", "errorbar",
                 "upper_errorbar", "lower_errorbar", "upper_pointrange", "lower_pointrange",
                 "upper_linerange", "lower_linerange")

  if(!(error.plot %in% allowed.error.plot))
    stop("Don't support ", error.plot, ". Possibilities for the argument error.plot are: ",
         .collapse(allowed.error.plot, sep = ", "))


  if(missing(width)) width <- 0.8

  .map <- .mapping(p)
  if(missing(color) & !is.null(.map$colour))
    color <- .map$colour
  if(missing(fill) & !is.null(.map$fill))
    fill <- .map$fill

  # Error limits
  #::::::::::::::::::::::::::::::::::::::::::::::::::
  . <- NULL
  error.limit <- strsplit(error.plot, "_") %>%
    unlist() %>%
    .[1]
  if(!(error.limit %in% c("upper", "lower")))
    error.limit <- "both"
  if(fun %in% c("mean", "median")) error.limit <- "none"

   # Defining plot geometry
   #::::::::::::::::::::::::::::::::::::::::::::::::::
  geom <- error.plot
  if(error.plot %in% c("pointrange", "lower_pointrange", "upper_pointrange"))
    geom <- "pointrange"
  else if(error.plot %in% c("linerange", "lower_linerange", "upper_linerange"))
    geom <- "linerange"
  else if(error.plot %in% c("errorbar", "lower_errorbar", "upper_errorbar"))
    geom <- "errorbar"

  fun.data <- fun.y <- fun.ymin <- fun.ymax <- NULL
  if(fun %in% c("mean", "median")){
    fun.y <- fun.ymin <- fun.ymax <- fun
  }
  else fun.data <- fun



  # General option
  #::::::::::::::::::::::::::::::::::::::::::::::::::
  opts <- list(geomfunc = "stat_summary", fun.data = fun.data, fun.y = fun.y,
            fun.ymin = fun.ymin, fun.ymax = fun.ymax,
            color = color,  geom = geom, size = size, linetype = linetype,
            show.legend = show.legend, data = data, position = position,
            fun.args = list(error.limit = error.limit), group = group)
  if(fun == "mean_ci") opts$fun.args$ci <- ci

  # Specific option
  #::::::::::::::::::::::::::::::::::::::::::::::::::
  if(geom == "crossbar") opts <- opts %>%
    .add_item(fill = fill, width = width)

  else if(geom == "errorbar"){
    if(missing(width)) opts$width = 0.1
    else opts$width = width
    opts$width <- 0.15
  }

  opts %>% .update_plot(p)
}


#' @describeIn add_summary returns the \code{mean} and the error limits defined by the
#'   \code{standard error}. We used the name \code{mean_se_}() to avoid masking \code{\link[ggplot2]{mean_se}}().
#' @export
mean_se_ <- function(x, error.limit = "both")
  {
  length <- base::sum(!is.na(x))
  sd = stats::sd(x, na.rm=TRUE)
  se <- sd / sqrt(length)
  .mean <- base::mean(x, na.rm = TRUE)
  data.frame(
    y =  .mean,
    ymin = .mean - se,
    ymax = .mean + se
  ) %>% .format_error(error.limit)
}

#' @describeIn add_summary returns the \code{mean} and the error limits defined by the
#'   \code{standard deviation}.
#' @export
mean_sd <- function(x, error.limit = "both"){
  sd = stats::sd(x, na.rm=TRUE)
  .mean <- base::mean(x, na.rm = TRUE)
  data.frame(
    y =  .mean,
    ymin = .mean - sd,
    ymax = .mean + sd
  ) %>% .format_error(error.limit)
}


#' @describeIn add_summary returns the \code{mean} and the error limits defined by the
#'   \code{confidence interval}.
#' @export
mean_ci <- function(x, ci = 0.95, error.limit = "both"){
  length <- base::sum(!is.na(x))
  sd = stats::sd(x, na.rm=TRUE)
  se <- sd / sqrt(length)
  .mean <- base::mean(x, na.rm = TRUE)
  ci <- stats::qt(ci/2 + .5, length-1)*se
  data.frame(
    y =  .mean,
    ymin = .mean - ci,
    ymax = .mean + ci
  ) %>% .format_error(error.limit)
}



#' @describeIn add_summary returns the \code{mean} and the error limits defined by the
#'   \code{range = max - min}.
#' @export
mean_range <- function(x, error.limit = "both"){
  .mean <- base::mean(x, na.rm = TRUE)
  .min <- base::min(x, na.rm=TRUE)
  .max <- base::max(x, na.rm=TRUE)
  .range <- .max - .min
  data.frame(
    y =  .mean,
    ymin = .mean - .range,
    ymax = .mean + .range
  ) %>% .format_error(error.limit)
}


#' @describeIn add_summary returns the \code{median} and the error limits
#'   defined by the \code{interquartile range}.
#' @export
median_iqr <- function(x, error.limit = "both"){
  .median = stats::median(x, na.rm=TRUE)
  .iqr <- stats::IQR(x, na.rm=TRUE)
  data.frame(
    y =  .median,
    ymin = .median - .iqr,
    ymax = .median + .iqr
  ) %>% .format_error(error.limit)
}

#' @describeIn add_summary returns the \code{median} and the error limits
#'   defined by the \code{median absolute deviation}.
#' @export
median_mad <- function(x, error.limit = "both"){
  .median = stats::median(x, na.rm=TRUE)
  .mad = stats::mad(x, na.rm=TRUE)
  data.frame(
    y =  .median,
    ymin = .median - .mad,
    ymax = .median + .mad
  ) %>% .format_error(error.limit)
}

#' @describeIn add_summary returns the \code{median} and the error limits
#'   defined by the \code{range = max - min}.
#' @export
median_range <- function(x, error.limit = "both"){
  .median = stats::median(x, na.rm=TRUE)
  .min <- base::min(x, na.rm=TRUE)
  .max <- base::max(x, na.rm=TRUE)
  .range <- .max - .min
  data.frame(
    y =  .median,
    ymin = .median - .range,
    ymax = .median + .range
  ) %>% .format_error(error.limit)
}


# Format error
.format_error <- function(d, error.limit = "both"){

  if(error.limit == "upper") d$ymin <- d$y
  else if(error.limit == "lower") d$ymax <- d$y
  else if(error.limit == "none") d$ymin <- d$ymax <- d$y
  d
}




