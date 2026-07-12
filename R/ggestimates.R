#' @include utilities.R
NULL
#' Forest Plot of Estimates and Confidence Intervals
#' @description Draws a publication-ready forest plot: one row per group showing
#'   a point estimate and its confidence interval, a reference (null) line and,
#'   optionally, a right-hand column with the numeric estimate and interval. It
#'   is a single, generic point-and-interval builder that covers both forest
#'   plots (odds/hazard/risk ratios, regression coefficients, meta-analysis) and
#'   estimation plots of effect sizes with their confidence intervals.
#' @details \code{ggestimates()} draws \strong{pre-computed} estimates and
#'   intervals - it does not fit a model or resample. Supply a data frame with an
#'   estimate column and its lower/upper interval columns (for example the output
#'   of \code{broom::tidy(model, conf.int = TRUE)}), one row per term. For ratio
#'   estimates (odds, hazard or risk ratios) set \code{ref.line = 1} and
#'   \code{log.scale = TRUE}; for differences and effect sizes keep the defaults
#'   \code{ref.line = 0}, \code{log.scale = FALSE}. The design follows the forest
#'   plot popularized by \code{survminer::ggforest()} and the estimation plots of
#'   the \code{dabestr} package.
#' @param data a data frame with one row per estimate.
#' @param estimate,conf.low,conf.high names of the columns holding the point
#'   estimate and the lower/upper confidence limits. Defaults match
#'   \code{broom::tidy()} output.
#' @param label name of the column used to label the rows (the y axis). If
#'   \code{NULL} (default), the row names of \code{data} are used, or the row
#'   number when there are none.
#' @param color the point/interval color. A single color name, or the name of a
#'   grouping column to color the rows by group.
#' @param palette the color palette for a grouping \code{color}. e.g. "jco",
#'   "npg"; see \code{\link{ggpar}()}.
#' @param ref.line x-intercept of the reference (null) line. Default 0, or 1 when
#'   \code{log.scale = TRUE} (the null for ratio estimates). Set to \code{NA} to
#'   omit the line.
#' @param log.scale logical. If \code{TRUE}, the x axis is log10-scaled (for
#'   ratio estimates). All estimates and limits must then be positive.
#' @param sort ordering of the rows. One of \code{"none"} (default, data order,
#'   first row on top), \code{"estimate"} (by the point estimate) or
#'   \code{"label"} (alphabetical).
#' @param descending logical. If \code{TRUE}, reverse the \code{sort} order.
#' @param point.size the point size. Ignored when \code{size} is given.
#' @param size optional name of a column mapped to the point size (e.g. a
#'   study weight or precision in meta-analysis).
#' @param shape the point shape. Default 15 (filled square).
#' @param ci.text logical. If \code{TRUE} (default), a right-hand column shows
#'   the numeric estimate and interval for each row.
#' @param ci.text.title the header of the \code{ci.text} column.
#' @param ci.text.width the fraction of the data x-range reserved on the right
#'   for the \code{ci.text} column. Increase it for long labels.
#' @param digits number of decimal places for the \code{ci.text} column.
#' @param label.hjust horizontal justification of the row (y-axis) labels:
#'   \code{1} (default) right-aligns them against the axis, \code{0} left-aligns,
#'   \code{0.5} centers.
#' @param banding logical. If \code{TRUE}, alternate rows are shaded with a light
#'   band (as in \code{survminer::ggforest()}) to help read across to the
#'   \code{ci.text} column. Default \code{FALSE}.
#' @param xlab,ylab,title axis labels and title.
#' @param ggtheme a ggplot theme. Default \code{\link{theme_pubr}()}.
#' @param ... other arguments passed to \code{\link{ggpar}()}.
#' @return a ggplot.
#' @seealso \code{\link{ggpar}}, \code{\link{ggdotchart}}.
#' @examples
#' # Odds ratios on a log scale (reference line at 1)
#' or <- data.frame(
#'   term = c("Age", "Sex: male", "BMI", "Smoker", "Treatment"),
#'   estimate = c(1.03, 1.45, 0.98, 2.10, 0.62),
#'   conf.low = c(0.99, 1.05, 0.94, 1.40, 0.45),
#'   conf.high = c(1.07, 2.00, 1.02, 3.15, 0.85)
#' )
#' ggestimates(or, label = "term", ref.line = 1, log.scale = TRUE,
#'   xlab = "Odds ratio (95% CI)")
#'
#' # Mean differences vs a control (reference line at 0), colored by group
#' md <- data.frame(
#'   group = c("Low dose", "Medium dose", "High dose"),
#'   estimate = c(1.8, 3.6, 5.9),
#'   conf.low = c(0.4, 2.1, 4.0),
#'   conf.high = c(3.2, 5.1, 7.8)
#' )
#' ggestimates(md, label = "group", color = "group", palette = "jco",
#'   xlab = "Mean difference (95% CI)")
#'
#' @export
ggestimates <- function(data, estimate = "estimate",
                        conf.low = "conf.low", conf.high = "conf.high",
                        label = NULL, color = "black", palette = NULL,
                        ref.line = 0, log.scale = FALSE,
                        sort = c("none", "estimate", "label"),
                        descending = FALSE,
                        point.size = 3, size = NULL, shape = 15,
                        ci.text = TRUE, ci.text.title = "Estimate (95% CI)",
                        ci.text.width = 0.5, digits = 2, label.hjust = 1,
                        banding = FALSE,
                        xlab = "Estimate (95% CI)", ylab = NULL, title = NULL,
                        ggtheme = theme_pubr(), ...) {
  sort <- match.arg(sort)
  ref.line.missing <- missing(ref.line)
  data <- as.data.frame(data)

  # Resolve and validate the required columns.
  need <- c(estimate, conf.low, conf.high)
  miss <- setdiff(need, colnames(data))
  if (length(miss) > 0) {
    stop("Column(s) not found in `data`: ", paste(miss, collapse = ", "),
      ". Set `estimate`/`conf.low`/`conf.high` to the correct column names.",
      call. = FALSE)
  }

  # Row labels: an explicit column, else the row names, else the row number.
  if (!is.null(label)) {
    if (!label %in% colnames(data)) {
      stop("`label` column ('", label, "') not found in `data`.", call. = FALSE)
    }
    labs.vec <- as.character(data[[label]])
  } else if (!is.null(rownames(data)) && !identical(rownames(data), as.character(seq_len(nrow(data))))) {
    labs.vec <- rownames(data)
  } else {
    labs.vec <- as.character(seq_len(nrow(data)))
  }

  df <- data.frame(
    .estimate = as.numeric(data[[estimate]]),
    .conf.low = as.numeric(data[[conf.low]]),
    .conf.high = as.numeric(data[[conf.high]]),
    .label = labs.vec,
    stringsAsFactors = FALSE
  )
  # Grouping color: a column name colors by group; anything else is a fixed color.
  group.color <- is.character(color) && length(color) == 1L && color %in% colnames(data)
  if (group.color) df$.color <- data[[color]]
  size.col <- !is.null(size)
  if (size.col) {
    if (!size %in% colnames(data)) {
      stop("`size` column ('", size, "') not found in `data`.", call. = FALSE)
    }
    df$.size <- as.numeric(data[[size]])
  }

  # Drop rows that cannot be plotted (missing estimate or limits), keeping the
  # count honest and avoiding a draw-time failure.
  keep <- stats::complete.cases(df[, c(".estimate", ".conf.low", ".conf.high")])
  if (!all(keep)) {
    warning(sum(!keep), " row(s) with missing estimate/interval were dropped.",
      call. = FALSE)
    df <- df[keep, , drop = FALSE]
  }
  if (nrow(df) == 0L) {
    stop("No rows with a complete estimate and interval to plot.", call. = FALSE)
  }

  if (log.scale && any(c(df$.estimate, df$.conf.low, df$.conf.high) <= 0, na.rm = TRUE)) {
    stop("`log.scale = TRUE` requires all estimates and limits to be positive.",
      call. = FALSE)
  }
  # On a log (ratio) scale the null is 1, not 0. Adopt 1 when the caller left
  # ref.line at its default, and reject an explicit non-positive reference line
  # that cannot be placed on a log axis (0 would give log10(0) = -Inf and a
  # blank panel).
  if (log.scale) {
    if (ref.line.missing) {
      ref.line <- 1
    } else if (!is.na(ref.line) && ref.line <= 0) {
      stop("`ref.line` must be positive when `log.scale = TRUE` (use ",
        "`ref.line = 1` for ratio estimates, or `ref.line = NA` to omit it).",
        call. = FALSE)
    }
  }

  # Row order. The y axis is a factor; reversing the levels puts the first row
  # on top (the reading order of a forest plot).
  ord <- switch(sort,
    none = seq_len(nrow(df)),
    estimate = order(df$.estimate),
    label = order(df$.label)
  )
  if (descending) ord <- rev(ord)
  df <- df[ord, , drop = FALSE]
  df$.label <- factor(df$.label, levels = rev(unique(df$.label)))
  n <- nlevels(df$.label)

  # Numeric estimate + interval text for the right-hand column.
  fmt <- function(v) formatC(v, format = "f", digits = digits)
  df$.ci <- paste0(fmt(df$.estimate), " (", fmt(df$.conf.low), ", ",
    fmt(df$.conf.high), ")")

  # Reserve room on the right for the ci.text column, computed in the axis's
  # own space (log or linear) so the text sits just past the widest interval.
  to.axis <- if (log.scale) log10 else identity
  from.axis <- if (log.scale) function(z) 10^z else identity
  ref.vals <- if (is.na(ref.line)) numeric(0) else ref.line
  axis.vals <- to.axis(c(df$.conf.low, df$.conf.high, ref.vals))
  a.lo <- min(axis.vals)
  a.hi <- max(axis.vals)
  a.span <- a.hi - a.lo
  if (a.span == 0) a.span <- abs(a.hi) + 1 # single-point degenerate case
  text.x <- from.axis(a.hi + ci.text.width * a.span)
  left.x <- from.axis(a.lo - 0.04 * a.span)

  # Build the plot.
  point.aes <- if (group.color) {
    ggplot2::aes(x = .data$.estimate, y = .data$.label, color = .data$.color)
  } else {
    ggplot2::aes(x = .data$.estimate, y = .data$.label)
  }
  p <- ggplot2::ggplot(df, point.aes)
  if (isTRUE(banding) && n > 1L) {
    # Shade alternate rows (2nd, 4th, ... from the top) behind everything. The
    # band spans the whole plotted width - including the ci.text gutter - so the
    # numeric column sits on the same stripe as its row. Positions are integer
    # row indices (deterministic); x-extent reuses the reserved region.
    pos <- seq_len(n)
    shade <- pos[(n - pos) %% 2 == 1]
    band.xmax <- if (ci.text) text.x else from.axis(a.hi + 0.04 * a.span)
    band.df <- data.frame(ymin = shade - 0.5, ymax = shade + 0.5)
    p <- p + ggplot2::geom_rect(
      data = band.df, inherit.aes = FALSE,
      mapping = ggplot2::aes(
        xmin = left.x, xmax = band.xmax,
        ymin = .data$ymin, ymax = .data$ymax
      ),
      fill = "grey93"
    )
  }
  if (!is.na(ref.line)) {
    p <- p + ggplot2::geom_vline(
      xintercept = ref.line, linetype = "dashed", color = "grey50"
    )
  }
  point.color <- if (group.color) NULL else color
  err <- ggplot2::geom_errorbar(
    ggplot2::aes(xmin = .data$.conf.low, xmax = .data$.conf.high),
    orientation = "y", width = 0.18, linewidth = 0.6
  )
  if (!group.color) err$aes_params$colour <- point.color
  p <- p + err
  if (size.col) {
    pt <- ggplot2::geom_point(ggplot2::aes(size = .data$.size), shape = shape)
  } else {
    pt <- ggplot2::geom_point(shape = shape, size = point.size)
  }
  if (!group.color) pt$aes_params$colour <- point.color
  p <- p + pt

  if (ci.text) {
    p <- p +
      ggplot2::geom_text(
        ggplot2::aes(x = text.x, label = .data$.ci),
        hjust = 1, size = 3.3, color = "black"
      ) +
      ggplot2::annotate("text", x = text.x, y = n + 0.75,
        label = ci.text.title, hjust = 1, fontface = "bold", size = 3.3)
  }

  p <- p +
    ggplot2::labs(x = xlab, y = ylab, title = title) +
    ggplot2::coord_cartesian(
      xlim = c(left.x, if (ci.text) text.x else from.axis(a.hi + 0.04 * a.span)),
      ylim = c(1, n + if (ci.text) 0.9 else 0.4),
      clip = "off"
    )
  if (log.scale) {
    p <- p + ggplot2::scale_x_log10(
      breaks = .nice_log_breaks(from.axis(a.lo), from.axis(a.hi))
    )
  }

  p <- ggpar(p, palette = palette, ggtheme = ggtheme, ...)
  # Forest-plot convention: no vertical (categorical) axis line or ticks - the
  # rows are labelled, and the reference line is the only meaningful vertical.
  p <- p + ggplot2::theme(
    legend.position = "none",
    axis.text.y = ggplot2::element_text(hjust = label.hjust, face = "bold"),
    axis.line.y = ggplot2::element_blank(),
    axis.ticks.y = ggplot2::element_blank(),
    plot.margin = ggplot2::margin(6, 12, 6, 6)
  )
  p
}

# Clean 1-2-5 breaks (and their powers of ten) within [lo, hi], for a log x
# axis. Falls back to the default breaks when the range is unusable.
.nice_log_breaks <- function(lo, hi) {
  if (!is.finite(lo) || !is.finite(hi) || lo <= 0 || hi <= 0) {
    return(ggplot2::waiver())
  }
  decades <- seq(floor(log10(lo)), ceiling(log10(hi)))
  cand <- sort(unique(as.vector(outer(c(1, 2, 5), 10^decades))))
  brks <- cand[cand >= lo & cand <= hi]
  if (length(brks) < 2) brks <- cand[cand >= lo / 2 & cand <= hi * 2]
  if (length(brks) < 2) return(ggplot2::waiver())
  brks
}
