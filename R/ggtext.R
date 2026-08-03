#' @include utilities.R ggpar.R stat_chull.R stat_conf_ellipse.R
NULL
#' Text
#' @description Add text to a plot.
#' @inheritParams ggscatter
#' @param data a data frame
#' @param x,y x and y variables for drawing.
#' @param label the name of the column containing point labels. Can be also a
#'   character vector with length = nrow(data).
#' @param color text font color.
#' @param size text font size.
#' @param face text font style. Allowed values are one of c("plain", "bold",
#'   "italic", "bold.italic").
#' @param parse If \code{TRUE}, the labels will be parsed into expressions and
#'   displayed as described in \code{?plotmath}.
#' @param family character vector specifying font family.
#' @param show.legend logical. Should text be included in the legends? NA, the
#'   default, includes if any aesthetics are mapped. FALSE never includes, and
#'   TRUE always includes.
#' @param label.select can be of two formats: \itemize{ \item a character vector
#'   specifying some labels to show. \item a list containing one or the
#'   combination of the following components: \itemize{ \item \code{top.up} and
#'   \code{top.down}: to display the labels  of the top up/down points. For
#'   example, \code{label.select = list(top.up = 10, top.down = 4)}. \item
#'   \code{criteria}: to filter, for example, by x and y variables values, use
#'   this: \code{label.select = list(criteria = "`y` > 2 & `y` < 5 & `x` \%in\%
#'   c('A', 'B')")}. } }
#' @param repel a logical value, whether to use ggrepel to avoid overplotting
#'   text labels or not.
#' @param label.rectangle logical value. If TRUE, add rectangle underneath the
#'   text, making it easier to read.
#' @param grouping.vars grouping variables to sort the data by, when the user
#'   wants to display the top n up/down labels.
#' @param position Position adjustment, either as a string, or the result of a
#'   call to a position adjustment function.
#' @param ggp a ggplot. If not NULL, points are added to an existing plot.
#' @param ... other arguments to be passed to \code{\link{ggpar}}.
#' @details The plot can be easily customized using the function ggpar(). Read
#'   ?ggpar for changing: \itemize{ \item main title and axis labels: main,
#'   xlab, ylab \item axis limits: xlim, ylim (e.g.: ylim = c(0, 30)) \item axis
#'   scales: xscale, yscale (e.g.: yscale = "log2") \item color palettes:
#'   palette = "Dark2" or palette = c("gray", "blue", "red") \item legend title,
#'   labels and position: legend = "right"  }
#' @seealso \code{\link{ggpar}}
#' @examples
#' # Load data
#' data("mtcars")
#' df <- mtcars
#' df$cyl <- as.factor(df$cyl)
#' df$name <- rownames(df)
#' head(df[, c("wt", "mpg", "cyl")], 3)
#'
#' # Textual annotation
#' # +++++++++++++++++
#' ggtext(df,
#'   x = "wt", y = "mpg",
#'   color = "cyl", palette = c("#00AFBB", "#E7B800", "#FC4E07"),
#'   label = "name", repel = TRUE
#' )
#'
#' # Add rectangle around label
#' ggtext(df,
#'   x = "wt", y = "mpg",
#'   color = "cyl", palette = c("#00AFBB", "#E7B800", "#FC4E07"),
#'   label = "name", repel = TRUE, label.rectangle = TRUE
#' )
#'
#' @export
ggtext <- function(data, x = NULL, y = NULL, label = NULL,
                   color = "black", palette = NULL,
                   size = 11, face = "plain", family = "", show.legend = NA,
                   label.select = NULL, repel = FALSE, label.rectangle = FALSE,
                   parse = FALSE,
                   grouping.vars = NULL,
                   position = "identity",
                   ggp = NULL, ggtheme = theme_pubr(),
                   ...) {
  . <- NULL
  .dots <- list(...)
  data <- as.data.frame(data)
  if (length(label) > 1) {
    if (length(label) != nrow(data)) {
      stop(
        "The argument label should be a column name or a vector of length = nrow(data). ",
        "It seems that length(label) != nrow(data)"
      )
    } else {
      label.col <- .new_col_name("label.xx", names(data))
      data[[label.col]] <- label
    }
    label <- label.col
  }

  if (is.null(ggp)) {
    p <- ggplot(data, create_aes(list(x = x, y = y)))
  } else {
    p <- ggp
  }

  # Add textual annotation
  # ++++++
  alpha <- 1
  if (!is.null(list(...)$alpha)) alpha <- list(...)$alpha

  if (is.null(label)) {
    return(p)
  }

  lab_data <- data

  # Decide whether y names a computed distribution height rather than a column
  # in the data, since the label must then be placed on the drawn bar or curve.
  #
  # Written as a statistic -- dot-dot or any `after_stat()` call -- it is one, and
  # that needs nothing else. The bare names `count` and `density` cannot be
  # resolved here at all: `gghistogram(y = "count")` means the bar height while
  # `ggtext(y = "count", ggp = <that histogram>)` means the caller's own column,
  # and both arrive with the same data, the same y and the same plot. The calling
  # function therefore says which, through `.computed.y`.
  .y_arg <- gsub("[[:space:]]", "", as.character(y[1])[1])

  # `after_stat()` may be namespaced or nested inside a larger expression, so
  # look for the call anywhere in the parsed value rather than matching text.
  # Anchoring on "after_stat(" missed `ggplot2::after_stat(count)` and
  # `sqrt(after_stat(count))`, both of which the drawing side computes.
  .calls_after_stat <- function(txt) {
    if (!nzchar(txt)) {
      return(FALSE)
    }
    e <- tryCatch(str2lang(txt), error = function(err) NULL)
    if (is.null(e)) {
      return(FALSE)
    }
    walk <- function(node) {
      if (!is.call(node)) {
        return(FALSE)
      }
      head <- node[[1]]
      nm <- if (is.name(head)) {
        as.character(head)
      } else if (is.call(head) && length(head) == 3L &&
        as.character(head[[1]]) %in% c("::", ":::")) {
        as.character(head[[3]])
      } else {
        ""
      }
      if (nm %in% c("after_stat", "stat")) {
        return(TRUE)
      }
      any(vapply(as.list(node)[-1], walk, logical(1)))
    }
    walk(e)
  }

  # A value the caller wrote as a computed statistic is one. Beyond that, the
  # calling function says so explicitly: `gghistogram()` and `ggdensity()` set
  # `.computed.y` when their `y` names a distribution height.
  #
  # It has to be told rather than worked out. `gghistogram(y = "count")` means
  # the bar height and `ggtext(y = "count", ggp = <that histogram>)` means the
  # caller's own column, and those two calls arrive here with the same data, the
  # same y and the same plot. Two earlier attempts to infer it -- from the data's
  # column names, then from the built plot's mapping -- each got one of the pair
  # right and the other silently wrong.
  # Written as a statistic, it is one -- that needs no signal and holds for a
  # direct call too. The signal only resolves the bare names, which are the
  # ambiguous case.
  .arg_is_computed <- .y_arg %in% c("..count..", "..density..") ||
    .calls_after_stat(.y_arg)
  .is_density_plot <- .arg_is_computed || isTRUE(.dots$.computed.y)
  if (.is_density_plot) {
    lab_data <- .hist_label_data(p, grouping.vars = list(...)$facet.by, x = x, data = data)
    y <- attr(lab_data, "ggpubr.label.y")

    # hist.data <- ggplot_build(p)$data[[1]][, c("x", "y", "count", "density")]
    # hist.x <- hist.data$x
    # hist.y <- hist.data$y
    # break.x <- c(0, hist.x) %>% unique()
    # label.break <- seq_len(length(break.x) - 1)
    # lab.y <- .select_vec(data, x) %>%
    #   cut(breaks = break.x, labels = label.break) %>%
    #   hist.y[.]
    # lab_data$lab.y <- lab.y
    # y <- "lab.y"
    # lab_data <- lab_data %>% dplyr::filter(!is.na(lab.y))
  }

  # Select some labels to show
  if (!is.null(label.select)) {
    lab_data <- .get_label_data(lab_data, x, y,
      label = label,
      label.select = label.select,
      grouping.vars = grouping.vars
    )
  }

  if (repel) {
    max.overlaps <- getOption("ggrepel.max.overlaps", default = Inf)
    ggfunc <- ggrepel::geom_text_repel
    if (label.rectangle) ggfunc <- ggrepel::geom_label_repel
    p <- p + geom_exec(ggfunc,
      data = lab_data, x = x, y = y,
      label = label, fontface = face,
      family = family, show.legend = show.legend,
      size = size / 3, color = color,
      alpha = alpha, parse = parse,
      box.padding = unit(0.35, "lines"),
      point.padding = unit(0.3, "lines"),
      force = 1, segment.size = 0.2, seed = 123,
      max.overlaps = max.overlaps
    )
  } else {
    ggfunc <- geom_text
    vjust <- -0.7
    hjust <- NULL
    if (label.rectangle) {
      ggfunc <- geom_label
      vjust <- -0.4
    }
    vjust <- ifelse(is.null(.dots$vjust), vjust, .dots$vjust)
    if (!is.null(.dots$hjust)) hjust <- .dots$hjust
    p <- p + geom_exec(ggfunc,
      data = lab_data, x = x, y = y, color = color,
      label = label, fontface = face, family = family, show.legend = show.legend,
      size = size / 3, parse = parse,
      vjust = vjust, hjust = hjust, alpha = alpha, position = position
    )
  }

  # p <- ggpar(p, palette = palette, ggtheme = ggtheme, ...)
  if (family != "") {
    p <- p + theme(text = element_text(family = family))
  }

  p
}


# data: data frame
# x, y: x and y variables
# label: label columns
# label.select: select some labels. Can be a character vector, or a list
#   with the following components (top.up, top.down)
# grouping.vars grouping variables
.get_label_data <- function(data, x, y, label = NULL,
                            label.select = NULL, grouping.vars = NULL) {
  if (.is_list(label.select)) {
    expected.components <- c("top.up", "top.down", "criteria")
    if (!any(expected.components %in% names(label.select))) {
      stop(
        "If label.select is a list, it should contain one or the combination ",
        "of the following element: ", .collapse(expected.components, sep = ", ")
      )
    }
  }

  data <- as.data.frame(data)

  if (is.null(label)) {
    lab_data <- NULL
  } else if (is.null(label.select)) {
    lab_data <- data
  } else if (.is_list(label.select)) {
    lab_data <- data
    top_up <- top_down <- . <- NULL

    if (!is.null(label.select$top.up)) {
      top_up <- .top_up(data, x, y,
        n = label.select$top.up,
        grouping.vars = grouping.vars
      )
    }

    if (!is.null(label.select$top.down)) {
      top_down <- .top_down(data, x, y,
        n = label.select$top.down,
        grouping.vars = grouping.vars
      )
    }

    if (!is.null(top_up) | !is.null(top_down)) {
      lab_data <- rbind(top_up, top_down)
    }

    if (!is.null(label.select$criteria)) {
      criteria <- gsub("`y`", y, label.select$criteria) %>%
        gsub("`x`", x, .)
      lab_data <- dplyr::filter(lab_data, !!rlang::parse_expr(criteria))
    }
  } else {
    # Evaluate the selector outside the data mask. A user column named `label`
    # must not replace the local column-name variable used here.
    lab_data <- data[data[[label]] %in% label.select, , drop = FALSE]
  }

  return(lab_data)
}


# Get histogram/density label y coord from ggplot output
# grouping.vars : facet variables
# x: x variable name
.hist_label_data <- function(p, grouping.vars = NULL, x = NULL, data = NULL) {
  . <- NULL
  # x <- .mapping(p) %>%.$x
  built <- ggplot_build(p)
  hist.data <- built$data[[1]]
  # Take the caller's frame when it supplies one. A vector `label` is stored as
  # an extra column on ggtext()'s local copy, but the plot arrives already built
  # from the caller's own data, so reading p$data here dropped that column and
  # every annotation rendered as the literal column name.
  if (is.null(data)) data <- p$data
  label_y_col <- .new_col_name("lab.y", names(data))
  layout <- built$layout$layout
  closed <- p$layers[[1]]$stat_params$closed %||% "right"
  pad <- isTRUE(p$layers[[1]]$stat_params$pad)

  # Distribution coordinates have already passed through the panel scale. Apply
  # the same panel-specific transformation to each observation before lookup.
  panel_x_transform <- function(panel) {
    layout_row <- match(as.character(panel), as.character(layout$PANEL))
    if (is.na(layout_row) || !"SCALE_X" %in% names(layout)) return(base::identity)
    scale_id <- layout$SCALE_X[[layout_row]]
    scale <- built$layout$panel_scales_x[[scale_id]]
    if (is.null(scale)) base::identity else scale$transform
  }

  if (is.null(grouping.vars)) {
    panel <- if ("PANEL" %in% names(hist.data)) hist.data$PANEL[[1]] else layout$PANEL[[1]]
    data <- .hist_label_y(
      hist.data, data, x, label_y_col,
      x.transform = panel_x_transform(panel), closed = closed, pad = pad
    )
    attr(data, "ggpubr.label.y") <- label_y_col
    return(data)
  }

  nested_col <- .new_col_name(".ggpubr.nested.", names(data))
  data <- data %>%
    dplyr::group_by(dplyr::across(dplyr::all_of(grouping.vars))) %>%
    dplyr::group_nest(.key = nested_col) %>%
    dplyr::ungroup()

  hist.nested <- hist.data %>%
    df_nest_by(vars = "PANEL")

  # Pair each nested annotation group with the panel that shares its facet
  # values. The two sides were matched by position, and they do not always agree:
  # the annotations nest in the order the values first appear in the data, while
  # panels follow the factor's level order. A facet whose levels are not in
  # appearance order therefore gave every label another panel's heights -- on a
  # two-level factor with a tight cluster in one panel and a spread in the other,
  # all ten labels were drawn at the wrong distribution.
  #
  # Walk out from the nested distributions rather than from the layout. A sparse
  # two-way grid has layout rows for combinations that carry no data, so a mask
  # built over the layout does not line up with this list.
  panel.facets <- layout[match(
    as.character(hist.nested$PANEL), as.character(layout$PANEL)
  ), , drop = FALSE]

  # Compare the facet values column by column instead of pasting them into one
  # string. Any separator or missing-value marker is itself a legal facet value,
  # so a serialised key can collide with real data -- the same trap as naming a
  # temporary column and hoping no user picks that name. A column-wise
  # comparison has nothing to collide with, and treats two absent values as
  # equal without conflating them with the literal text "NA".
  matched <- vapply(seq_len(nrow(panel.facets)), function(i) {
    hit <- rep(TRUE, nrow(data))
    for (v in grouping.vars) {
      a <- panel.facets[[v]][i]
      b <- data[[v]]
      hit <- hit & ifelse(
        is.na(a) | is.na(b),
        is.na(a) & is.na(b),
        as.character(a) == as.character(b)
      )
    }
    w <- which(hit)
    if (length(w)) w[[1]] else NA_integer_
  }, integer(1))
  keep <- !is.na(matched)
  data <- data[matched[keep], , drop = FALSE]
  hist.data <- hist.nested$data[keep]
  panels <- hist.nested$PANEL[keep]

  # Derive names that are provably absent from this frame rather than picking
  # ones that seem unlikely. `facet.by` is user-controlled, so any FIXED name is
  # a name some user may already have: plain `hist.data`/`lab.data` collided, and
  # renaming them to `.hist.data.`/`.lab.data.` only moved the collision - a
  # facet column called `.hist.data.` still broke the label layer.
  hist_col <- .new_col_name(".hist.data.", names(data))
  lab_col  <- .new_col_name(".lab.data.",  c(names(data), hist_col))

  data[[hist_col]] <- hist.data
  lab.data <- Map(
    function(hist, labels, transform) {
      .hist_label_y(
        hist, labels, x, label_y_col,
        x.transform = transform, closed = closed, pad = pad
      )
    },
    data[[hist_col]], data[[nested_col]], lapply(panels, panel_x_transform)
  )

  data[[lab_col]] <- lab.data
  data <- data %>%
    df_select(vars = c(lab_col, grouping.vars)) %>%
    tidyr::unnest(cols = dplyr::all_of(lab_col))

  attr(data, "ggpubr.label.y") <- label_y_col
  data
}

# Get histogram/density label y coord from ggplot output
# hist.data: histogram data. ggplot_build(p)$data[[1]]
# data: data frame
# x: x variable name
.hist_label_y <- function(hist.data, data, x, label_y_col = "lab.y",
                          x.transform = base::identity, closed = "right",
                          pad = FALSE) {
  . <- NULL
  xv <- x.transform(.select_vec(data, x))

  if (all(c("xmin", "xmax") %in% names(hist.data))) {
    # stat_bin(pad = TRUE) adds one display-only empty bin to each end of every
    # panel/group. Those bins must not receive labels at a shared boundary: the
    # adjacent real bin is the one whose count includes the observation.
    pad.row <- rep(FALSE, nrow(hist.data))
    if (isTRUE(pad) && nrow(hist.data)) {
      group.cols <- intersect(c("PANEL", "group"), names(hist.data))
      bin.group <- if (length(group.cols)) {
        interaction(hist.data[group.cols], drop = TRUE, lex.order = TRUE)
      } else {
        rep.int(1L, nrow(hist.data))
      }
      for (rows in split(seq_len(nrow(hist.data)), bin.group)) {
        rows <- rows[order(hist.data$xmin[rows], hist.data$xmax[rows])]
        if (length(rows) >= 3L) pad.row[c(rows[[1]], rows[[length(rows)]])] <- TRUE
      }
    }
    eligible <- !pad.row
    endpoint.equal <- function(a, b) isTRUE(all.equal(a, b))

    # Match stat_bin()'s half-open interval convention. Its outermost endpoint is
    # included so an observation at the scale boundary is never dropped. This
    # previously cut on the bar CENTRES, which
    # puts every value between two centres into the wrong bar: with bins
    # [-0.85, 1.65] and [1.65, 4.15], an observation at 1.5 was given the second
    # bar's height. Measured on twelve observations spread across four bins,
    # five were annotated at another bar's height.
    idx <- vapply(xv, function(v) {
      if (is.na(v)) {
        return(NA_integer_)
      }
      if (identical(closed, "left")) {
        in_bin <- eligible & v >= hist.data$xmin & v < hist.data$xmax
        endpoint <- max(hist.data$xmax[eligible], na.rm = TRUE)
        if (!any(in_bin) && endpoint.equal(v, endpoint)) {
          in_bin <- eligible & hist.data$xmax == endpoint
        }
      } else {
        in_bin <- eligible & v > hist.data$xmin & v <= hist.data$xmax
        endpoint <- min(hist.data$xmin[eligible], na.rm = TRUE)
        if (!any(in_bin) && endpoint.equal(v, endpoint)) {
          in_bin <- eligible & hist.data$xmin == endpoint
        }
      }
      j <- which(in_bin)
      if (length(j)) j[[1]] else NA_integer_
    }, integer(1))
    lab.y <- hist.data$y[idx]
  } else {
    # A density layer is a curve sampled on a grid, with no interval to fall
    # inside, so read the height off the curve at the observation's x.
    #
    # `rule = 1` leaves observations outside the curve's range as NA, and they
    # are dropped below. Clamping them to the nearest end instead would place
    # them at a height the curve never has.
    lab.y <- stats::approx(hist.data$x, hist.data$y, xout = xv, rule = 1)$y
  }

  data[[label_y_col]] <- lab.y
  data <- data[!is.na(data[[label_y_col]]), , drop = FALSE]
  data
}
