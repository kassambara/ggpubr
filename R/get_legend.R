#' @include utilities.R
NULL
#' Extract Legends from a ggplot object
#' @description Extract the legend labels from a ggplot object.
#' @param p an object of class ggplot or a list of ggplots. If p is a list, only the first legend is returned.
#' @param position character specifying legend position. Allowed values are one of
#'   c("top", "bottom", "left", "right", "none"). To remove the legend use
#'   legend = "none".
#' @return an object of class gtable.
#' @examples
#' # Create a scatter plot
#' p <- ggscatter(iris, x = "Sepal.Length", y = "Sepal.Width",
#'         color = "Species", palette = "jco",
#'         ggtheme = theme_minimal())
#' p
#'
#' # Extract the legend. Returns a gtable
#' leg <- get_legend(p)
#'
#' # Convert to a ggplot and print
#' as_ggplot(leg)
#'
#' @export
get_legend <- function(p, position = NULL){

  if(.is_list(p)){
    continue <- TRUE
    i <- 1
    while(i <= length(p) & continue){
      leg <- .get_legend(p[[i]], position = position)
      if(!is.null(leg)) continue <- FALSE
      i <- i+1
    }
  }
  else{
    leg <- .get_legend(p, position = position)
  }
  leg
}



# Return legend for one plot
.get_legend <- function(p, position = NULL){

  if(is.null(p)) return(NULL)
  if(!is.null(position)){
    p <- p + theme(legend.position = position)
  }
  tmp <- ggplot_gtable(ggplot_build(p))
  leg <- which(sapply(tmp$grobs, function(x) x$name) == "guide-box")
  if(length(leg) > 0) leg <- tmp$grobs[[leg]]
  else leg <- NULL
  leg
}

