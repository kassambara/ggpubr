#' Checks and Returns Data Coordinates from Multiple Input Options
#'
#' @description Checks and returns selected coordinates from multiple input
#'   options, which can be either data (x-y) coordinates or npc (normalized
#'   parent coordinates).
#'
#'   Helper function internally used in \code{ggpubr} function to guess the type
#'   of coordinates specified by the user. For example, in the function
#'   \code{stat_cor()}, users can specify either the option \code{label.x} (data
#'   coordinates) or \code{label.x.npc} (npc coordinates); those coordinates are
#'   passed to \code{get_coord()}, which will make some checking and then return
#'   a unique coordinates for the label position.
#' @inheritParams as_npc
#' @param data.ranges a numeric vector of length 2 containing the data ranges
#'   (minimum and the maximum). Should be specified only when \code{coord =
#'   NULL} and \code{npc} is specified. Used to convert \code{npc} to data
#'   coordinates. Considered only when the argument \code{npc} is specified.
#' @param coord data coordinates (i.e., either x or y coordinates).
#' @param npc numeric (in [0-1]) or character vector of coordinates. If
#'   character, should be one of c('right', 'left', 'bottom', 'top', 'center',
#'   'centre', 'middle'). Note that, the \code{data.ranges}, \code{step} and
#'   \code{margin.npc}, arguments are considered only when \code{npc} is
#'   specified. The option \code{npc} is ignored when the argument \code{coord} is specified.
#' @return a numeric vector representing data coordinates.
#' @seealso \code{\link{as_npc}}, \code{\link{npc_to_data_coord}}.
#' @examples
#' # If npc is specified, it is converted into data coordinates
#' get_coord(data.ranges = c(2, 20), npc = "left")
#' get_coord(data.ranges = c(2, 20), npc = 0.1)
#'
#' # When coord is specified, no transformation is performed
#' # because this is assumed to be a data coordinate
#' get_coord(coord = 5)
#'
#' # For grouped plots
#' res_top <- get_coord(
#'   data.ranges = c(4.2, 36.4), group = c(1, 2, 3),
#'   npc = "top", step = -0.1, margin.npc = 0
#' )
#' res_top
#'
#' @rdname get_coord
#' @export
get_coord <- function(group = 1L, data.ranges = NULL, coord = NULL, npc = "left",
                      step = 0.1, margin.npc = 0.05){
  if(!is.null(coord)){
    if(!is.numeric(group)){
      stop("get_coord: 'group' should be numeric. ",
           "Current class is: ", class(group))
    }
    # If coords are too short, they are recycled.
    coord <- ifelse(length(coord) >= group, coord[group], coord[1])
    return(coord)
  }
  else if(!is.null(npc)){
    if(is.null(data.ranges)){
      stop("Specify the option data.ranges", call. = FALSE)
    }
    npc <- ifelse(length(npc) >= group, npc[group], npc[1])
    coord <- as_npc(
      npc, group = group, step = step,
      margin.npc = margin.npc
    ) %>%
      npc_to_data_coord(data.ranges)
  }
  coord
}


