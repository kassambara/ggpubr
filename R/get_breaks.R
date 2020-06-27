#' Easy Break Creation for Numeric Axes
#'
#' @description Creates breaks for numeric axes to be used in the functions
#'   \code{\link[ggplot2:scale_continuous]{scale_x_continuous}()} and
#'   \code{\link[ggplot2:scale_continuous]{scale_y_continuous}()}. Can be used to increase the
#'   number of x and y ticks by specifying the option \code{n}. It's also
#'   possible to control axis breaks by specifying a step between ticks.  For
#'   example, if \code{by = 5}, a tick mark is shown on every 5.
#' @param n number of breaks.
#' @param by number: the step between breaks.
#' @param from the starting value of breaks. By default, 0 is used for positive
#'   variables
#' @param to the end values of breaks. This corresponds generally to the maximum
#'   limit of the axis.
#' @return a break function
#'
#' @examples
#'
#' # Generate 5 breaks for a variable x
#' get_breaks(n = 5)(x = 1:100)
#'
#' # Generate breaks using an increasing step
#' get_breaks(by = 10)(x = 1:100)
#'
#' # Combine with ggplot scale_xx functions
#' library(ggplot2)
#'
#' # Create a basic plot
#' p <- ggscatter(mtcars, x = "wt", y = "mpg")
#' p
#'
#' # Increase the number of ticks
#' p +
#'  scale_x_continuous(breaks = get_breaks(n = 10)) +
#'  scale_y_continuous(breaks = get_breaks(n = 10))
#'
#' # Set ticks according to a specific step, starting from 0
#' p + scale_x_continuous(
#'   breaks = get_breaks(by = 1.5, from = 0),
#'   limits =  c(0, 6)
#' ) +
#'  scale_y_continuous(
#'   breaks = get_breaks(by = 10, from = 0),
#'   limits = c(0, 40)
#'   )
#'
#' @export
get_breaks <- function(n = NULL, by = NULL, from = NULL, to = NULL){
  breaks <- ggplot2::waiver()
  if(!is.null(n)){
    breaks  <- get_breaks_number(n = n)
  }
  else if(is.numeric(by)){
    breaks <- get_breaks_position(by = by, from = from, to = to)
  }
  breaks
}


# Set the number of breaks
get_breaks_number <- function(n){
 scales::breaks_extended(n = n)
}

# Set breaks using increasing step
# Adapted from scales::breaks_extended
get_breaks_position <- function(by, from = NULL, to = NULL){
  by_default <- by
  from_default <- from
  to_default <- to
  function(x,  by = by_default, from = from_default, to = to_default) {
    x <- x[is.finite(x)]
    if (length(x) == 0) {
      return(numeric())
    }
    rng <- range(x)
    if( rng[1] > 0 & is.null(from)) from <- 0
    xmin <- ifelse(is.null(from), floor(rng[1]), from)
    xmax <- ifelse(is.null(to), rng[2], to)
    seq(from = xmin, to = xmax, by = by)
  }
}
