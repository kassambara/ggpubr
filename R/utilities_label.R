# ::::::::::::::::::::::::::::::::::::::::::::::::::::::::
# Helper function for adding annotation to a ggplot
# ::::::::::::::::::::::::::::::::::::::::::::::::::::::::


# Get label parameters for each group
# :::::::::::::::::::::::::::::::::::::::::::::::::::::::::
# Returns a data frame with x, y, hjust, vjust
# group.id is the index position of the group in a boxplot for example
# Pick the axis position for a label from its npc fraction. With
# label.anchor = "panel" the fraction is returned as an AsIs (`I()`) value so
# ggplot2 (>= 3.5.0) places it at the true panel-relative npc AT DRAW TIME -
# this keeps labels aligned across panels/facets whose axis ranges differ (e.g.
# scales = "free_y" or geom_smooth extending each panel), fixing #248. With
# label.anchor = "data" (default) the caller-supplied `data.value` - the exact
# historical data-coordinate expression for that anchor - is returned verbatim,
# so default output stays bit-identical.
.npc_to_position <- function(npc, data.value, label.anchor = "data") {
  if (identical(label.anchor, "panel")) return(I(npc))
  data.value
}

.label_params <- function(data, scales, label.x.npc = "left", label.y.npc = "right",
                          label.x = NULL, label.y = NULL, label.y.step = 1.4,
                          label.anchor = c("data", "panel"),
                          .by = c("group", "panel"),
                          group.id = NULL, ...) {
  .by <- match.arg(.by)
  label.anchor <- match.arg(label.anchor)
  # Check label coordinates for each group
  # :::::::::::::::::::::::::::::::::::::::::::::::::::::::::::
  if (is.null(group.id)) {
    group.id <- group.id <- abs(data$group[1])
  }
  label.x.npc <- .group_coord(label.x.npc, group.id)
  label.y.npc <- .group_coord(label.y.npc, group.id)
  label.x <- .group_coord(label.x, group.id)
  label.y <- .group_coord(label.y, group.id)

  .check_npc_coord(label.x.npc, axis = "x")
  .check_npc_coord(label.y.npc, axis = "y")


  if (length(label.x) > 0) {
    x <- label.x
    hjust <- 0.5
  } else if (length(label.x.npc) > 0) {
    # For each anchor, resolve the npc fraction (for panel mode) alongside the
    # EXACT historical data expression (for the default data mode), so
    # label.anchor = "data" is bit-identical to previous versions.
    if (is.numeric(label.x.npc)) {
      x <- .npc_to_position(
        label.x.npc,
        scales$x$dimension()[1] + label.x.npc * diff(scales$x$dimension()),
        label.anchor
      )
      hjust <- 0.5
    } else if (is.character(label.x.npc)) {
      if (label.x.npc == "right") {
        x <- .npc_to_position(1, scales$x$dimension()[2], label.anchor)
        hjust <- 1
      } else if (label.x.npc %in% c("center", "centre", "middle")) {
        x <- .npc_to_position(0.5, mean(scales$x$dimension()), label.anchor)
        hjust <- 0.5
      } else if (label.x.npc == "left") {
        x <- .npc_to_position(0, scales$x$dimension()[1], label.anchor)
        hjust <- 0
      }
    }
  }

  if (length(label.y) > 0) {
    y <- label.y
    vjust <- 0.5
  } else if (length(label.y.npc) > 0) {
    if (is.numeric(label.y.npc)) {
      y <- .npc_to_position(
        label.y.npc,
        scales$y$dimension()[1] + label.y.npc * diff(scales$y$dimension()),
        label.anchor
      )
      vjust <- label.y.step * group.id - (label.y.step / 2 * length(group.id))
    } else if (is.character(label.y.npc)) {
      if (label.y.npc == "bottom") {
        y <- .npc_to_position(0, scales$y$dimension()[1], label.anchor)
        vjust <- -label.y.step * group.id
      } else if (label.y.npc %in% c("center", "centre", "middle")) {
        y <- .npc_to_position(0.5, mean(scales$y$dimension()), label.anchor)
        vjust <- label.y.step * group.id - (label.y.step / 2 * length(group.id))
      } else if (label.y.npc == "top") {
        y <- .npc_to_position(1, scales$y$dimension()[2], label.anchor)
        vjust <- label.y.step * group.id
      }
    }
  }
  if (.by == "panel") {
    hjust <- 0.5
    vjust <- 0.5
  }

  data.frame(x = x, y = y, hjust = hjust, vjust = vjust)
}


# Get label parameters by group
# Useful in boxplot, where group.ids is the index of the group: 1, 2, 3, etc
# Useful only when computation is done by panel
.label_params_by_group <- function(..., group.ids) {
  purrr::map(
    group.ids,
    function(group.id, ...) {
      .label_params(..., group.id = group.id)
    },
    ...
  ) %>%
    dplyr::bind_rows() # %>%
  # dplyr::mutate(x = group.ids)
}


# Check label coordinates for each group
# :::::::::::::::::::::::::::::::::::::::::
# coord.values: label coordinate for each group. If too short, they are recycled.
# group.id the id of groups as returned by ggplot_build()
.group_coord <- function(coord.values, group.id) {
  if (!.is_empty(coord.values)) {
    # group.id must be a valid 1-based index; a non-positive id (e.g. when the
    # group value is derived from an x position of 0) would make
    # coord.values[group.id] empty and produce NA. Fall back to the first value
    # (recycling) in that case (#594).
    coord.values <- ifelse(length(coord.values) >= group.id & group.id > 0,
      coord.values[group.id], coord.values[1]
    )
  }
  coord.values
}


# Check NPC coord
# :::::::::::::::::::::::::::::::::::::::::
# npc: Normalised Parent Coordinates.
#   The origin of the viewport is (0, 0) and the viewport has a width and height of 1 unit.
#   For example, (0.5, 0.5) is the centre of the viewport.
# coord: should be between 0 and 1
# axis: should be "x" or "y"
.check_npc_coord <- function(.coord, axis = c("x", "y")) {
  axis <- match.arg(axis)
  if (axis == "x") {
    allowed.values <- c("right", "left", "center", "centre", "middle")
  } else if (axis == "y") {
    allowed.values <- c("bottom", "top", "center", "centre", "middle")
  }

  .message <- paste0(
    "'*.npc coord for ", axis, " axis should be either a numeric value in [0-1] ",
    "or a character strings including one of ",
    .collapse(allowed.values, sep = ", ")
  )

  if (!is.null(.coord)) {
    if (is.numeric(.coord)) {
      if (any(.coord < 0 | .coord > 1)) {
        stop(.message)
      }
    } else if (is.character(.coord)) {
      if (!(.coord %in% allowed.values)) {
        stop(.message)
      }
    } else {
      stop(.message)
    }
  }
}
