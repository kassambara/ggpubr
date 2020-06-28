#' Convert NPC to Data Coordinates
#' @description Convert NPC (Normalized Parent Coordinates) into data coordinates.
#' @param npc a numeric vector. Each value should be in [0-1]
#' @param data.ranges a numeric vector of length 2 containing the data ranges (minimum and the maximum)
#' @return a numeric vector representing data coordinates.
#' @seealso \code{\link{as_npc}}, \code{\link{get_coord}}.
#' @examples
#' npc_to_data_coord(npc = c(0.2, 0.95), data.ranges = c(1, 20))
#' as_npc(c("top", "right")) %>%
#'    npc_to_data_coord(data.ranges = c(1, 20))
#'
#' @rdname npc_to_data_coord
#' @export
npc_to_data_coord <- function(npc, data.ranges){
  data.ranges[1] + npc * (data.ranges[2] - data.ranges[1])
}
