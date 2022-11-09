#' Convert Character Coordinates into Normalized Parent Coordinates (NPC)
#'
#' Convert character coordinates to npc units and shift postions to avoid
#' overlaps when grouping is active. If numeric validate npc values.
#'
#' @param value numeric (in [0-1]) or character vector of coordinates. If
#'   character, should be one of \code{c('right', 'left', 'bottom', 'top',
#'   'center', 'centre', 'middle')}.
#' @param group integer ggplot's group id. Used to shift coordinates to avoid
#'   overlaps.
#' @param step numeric value in [0-1]. The step size for shifting coordinates
#'   in npc units. Considered as horizontal step for x-axis and vertical step
#'   for y-axis. For y-axis, the step value can be negative to reverse the order of groups.
#' @param margin.npc numeric [0-1] The margin added towards the nearest
#'   plotting area edge when converting character coordinates into npc.
#' @param axis the concerned axis . Should be one of \code{c("xy", "x", "y")}.
#'
#' @return A numeric vector with values in the range [0-1] representing npc
#'   coordinates.
#' @seealso \code{\link{npc_to_data_coord}}, \code{\link{get_coord}}.
#' @details the \code{as_npc()} function is an adaptation from
#'   \code{ggpmisc::compute_npc()}.
#'
#' @examples
#' as_npc(c("left", "right"))
#' as_npc(c("top", "right"))
#'
#' @describeIn as_npc converts x or y coordinate values into npc. Input values
#'   should be numeric or one of the following values \code{c('right', 'left',
#'   'bottom', 'top', 'center', 'centre', 'middle')}.
#' @export
as_npc <- function(value, group = 1L, step = 0.1, margin.npc = 0.05, axis = c("xy", "x", "y")) {
  group <- abs(group)
  axis <- match.arg(axis)
  if (is.factor(value)) {
    value <- as.character(value)
  }
  assertthat_coord_is_valid(value, axis = axis)
  if (is.character(value)) {
    map <- c(right = 1 - margin.npc,
             left = 0 + margin.npc,
             top = 1 - margin.npc,
             bottom = 0 + margin.npc,
             centre = 0.5,
             center = 0.5,
             middle = 0.5,
             NA_real_)
    value <- unname(map[value])
  }
  if (any(group > 1L) & step != 0) {
    value <- value + (group - 1L) * step * ifelse(value < 0.5, 1, -1)
  }
  #value <- ifelse(value > 1, 1, value)
  #value <- ifelse(value < 0, 0, value)
  value
}

#' @describeIn as_npc converts x coordinate values into npc. Input values should
#'   be numeric or one of the following values \code{c('right', 'left',
#'   'center', 'centre', 'middle')}. Wrapper around \code{as_npc(axis = "x")}.
#' @export
as_npcx <- function(value, group = 1L, step = 0.1, margin.npc = 0.05) {
  as_npc(value, group = group, step = step, margin.npc = margin.npc, axis = "x" )
}

#' @describeIn as_npc converts y coordinate values into npc. Input values should
#'   be numeric or one of the following values \code{c( 'bottom', 'top',
#'   'center', 'centre', 'middle')}. Wrapper around \code{as_npc(axis = "y")}.
#' @export
as_npcy <- function(value, group = 1L, step = 0.1, margin.npc = 0.05) {
  as_npc(value, group = group, step = step, margin.npc = margin.npc, axis = "y" )
}




# Check NPC coord-----------------------------------------
# coord: should be between 0 and 1 or a character
# axis: should be "x" or "y"
assertthat_coord_is_valid <- function(.coord, axis = c("x", "y", "xy")){
  axis <- match.arg(axis)
  allowed.values <- switch (axis,
    x = c('right', 'left', 'center', 'centre', 'middle'),
    y = c( 'bottom', 'top', 'center', 'centre', 'middle'),
    xy = c('right', 'left', 'bottom', 'top', 'center', 'centre', 'middle')
  )
  .message <- paste0(
    "'*.npc coord for ", axis, " axis should be either a numeric value in [0-1] ",
    "or a character strings including one of: ",
    paste(allowed.values, collapse = ", ")
  )
  if(is.numeric(.coord)){
    if (any(.coord < 0 | .coord > 1)) {
      stop(.message, call. = FALSE)
    }
  }
  else if(is.character(.coord)){
    if(!all(.coord %in% allowed.values))
      stop(.message, call. = FALSE)
  }
}


