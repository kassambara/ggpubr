#' @include utilities.R ggpar.R
NULL
#' Plot Paired Data
#' @description Plot paired data.
#' @inheritParams ggboxplot
#' @param cond1 variable name corresponding to the first condition.
#' @param cond2 variable name corresponding to the second condition.
#' @param x,y x and y variables, where x is a grouping variable and y contains
#'  values for each group. Considered only when \code{cond1} and \code{cond2}
#'  are missing.
#' @param id variable name corresponding to paired samples' id. Used to connect
#'  paired points with lines.
#' @param color points and box plot colors. To color by conditions, use color =
#'  "condition".
#' @param fill box plot fill color. To change fill color by conditions, use fill
#'  = "condition".
#' @param line.color line color.
#' @param linetype line type.
#' @param point.size,line.size point and line size, respectively.
#' @param width box plot width.
#' @param jitter numeric value (default 0, no jitter) giving the amount of
#'   horizontal jitter added to the paired points to reduce overlap. Points are
#'   nudged sideways within \code{[-jitter, jitter]}; each subject (\code{id})
#'   gets a single offset so its two points move together and the connecting
#'   line stays intact. Only the horizontal positions change (the values are
#'   never moved). Typical values are small relative to the box \code{width}
#'   (e.g. \code{jitter = 0.05} to \code{0.1}). \code{jitter = 0} leaves the plot
#'   unchanged.
#' @param ... other arguments to be passed to be passed to \link{ggpar}().
#' @examples
#'
#' # Example 1
#' # ::::::::::::::::::::::::::::::::::::::::::
#' before <- c(200.1, 190.9, 192.7, 213, 241.4, 196.9, 172.2, 185.5, 205.2, 193.7)
#' after <- c(392.9, 393.2, 345.1, 393, 434, 427.9, 422, 383.9, 392.3, 352.2)
#'
#' d <- data.frame(before = before, after = after)
#' ggpaired(d,
#'   cond1 = "before", cond2 = "after",
#'   fill = "condition", palette = "jco"
#' )
#'
#' # Example 2
#' # ::::::::::::::::::::::::::::::::::::::::::
#' ggpaired(ToothGrowth,
#'   x = "supp", y = "len",
#'   color = "supp", line.color = "gray", line.size = 0.4,
#'   palette = "npg"
#' )
#'
#' @export
ggpaired <- function(data, cond1, cond2, x = NULL, y = NULL, id = NULL,
                     color = "black", fill = "white", palette = NULL,
                     width = 0.5, point.size = 1.2, line.size = 0.5, line.color = "black",
                     linetype = "solid",
                     title = NULL, xlab = "Condition", ylab = "Value",
                     facet.by = NULL, panel.labs = NULL, short.panel.labs = TRUE,
                     label = NULL, font.label = list(size = 11, color = "black"),
                     label.select = NULL, repel = FALSE, label.rectangle = FALSE,
                     ggtheme = theme_pubr(),
                     jitter = 0,
                     ...) {
  grouping.vars <- c(x, color, fill) %>%
    unique() %>%
    intersect(colnames(data))

  if (!missing(cond1) & !missing(cond2)) {
    data <- data %>%
      df_gather(
        cols = c(cond1, cond2),
        names_to = "condition", values_to = "val"
      )
    data$condition <- factor(data$condition, levels = c(cond1, cond2))
    x <- "condition"
    y <- "val"
  } else if (!is.null(x) & !is.null(y)) {
    if (missing(xlab)) xlab <- x
    if (missing(ylab)) ylab <- y
  }


  # Default options
  # :::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::
  .opts <- list(
    id = id,
    color = color, fill = fill, palette = palette,
    width = width, point.size = point.size,
    line.size = line.size, line.color = line.color, linetype = linetype,
    title = title, xlab = xlab, ylab = ylab,
    facet.by = facet.by, panel.labs = panel.labs, short.panel.labs = short.panel.labs,
    label = label, font.label = font.label, label.select = label.select,
    repel = repel, label.rectangle = label.rectangle, ggtheme = ggtheme,
    jitter = jitter, ...
  )

  # User options
  # :::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::
  .user.opts <- as.list(match.call(expand.dots = TRUE))
  .user.opts[[1]] <- NULL # Remove the function name
  # keep only user arguments
  for (opt.name in names(.opts)) {
    if (is.null(.user.opts[[opt.name]])) {
      .opts[[opt.name]] <- NULL
    }
  }
  .opts$data <- data
  .opts$x <- x
  .opts$y <- y


  .opts$fun <- ggpaired_core
  if (missing(ggtheme) & (!is.null(facet.by))) {
    .opts$ggtheme <- theme_pubr(border = TRUE)
  }
  # Honor an explicit `ggtheme = NULL` (skip theming). The NULL-filter loop above
  # drops it like an unset argument, so restore any explicitly passed value here,
  # keeping an explicit NULL intact via single-bracket list assignment (#561).
  if (!missing(ggtheme)) .opts["ggtheme"] <- list(ggtheme)
  p <- do.call(.plotter, .opts)

  if (.is_list(p) & length(p) == 1) p <- p[[1]]
  return(p)
}

ggpaired_core <- function(data, x = NULL, y = NULL, id = NULL,
                          color = "black", fill = "white", palette = NULL,
                          width = 0.5, point.size = 1.2, line.size = 0.5, line.color = "black",
                          linetype = "solid", title = NULL, xlab = "Condition", ylab = "Value",
                          ggtheme = theme_pubr(),
                          jitter = 0,
                          ...) {
  if (!is.factor(data[[x]])) data[[x]] <- as.factor(data[[x]])

  grouping.vars <- c(x, color, fill) %>%
    unique() %>%
    intersect(colnames(data))


  # Add paired sample ids
  if (!is.null(id)) {
    id <- .select_vec(data, id)
  } else {
    id <- rep(seq_len(nrow(data) / 2), 2)
  }
  data$id <- id

  # Optional horizontal jitter of the paired points (#407). Spreads the points
  # to reduce overlap while keeping each pair's connecting line intact: one
  # offset per subject id (so the connecting lines stay parallel and readable),
  # applied to BOTH the points and the lines, and horizontal only (the values
  # `y` are never moved). A fixed seed keeps a given plot reproducible; the
  # caller's RNG stream is left untouched. Default jitter = 0 leaves the output
  # exactly unchanged (no jittered-x column, no RNG use).
  jitter.x <- NULL
  if (!is.null(jitter) && length(jitter) == 1 && is.numeric(jitter) &&
      !is.na(jitter) && jitter > 0) {
    ids <- unique(data$id)
    if (exists(".Random.seed", envir = .GlobalEnv, inherits = FALSE)) {
      old.seed <- get(".Random.seed", envir = .GlobalEnv, inherits = FALSE)
      on.exit(assign(".Random.seed", old.seed, envir = .GlobalEnv), add = TRUE)
    } else {
      # no RNG used yet: remove the seed we are about to create, so ggpaired()
      # leaves the caller's random stream exactly as it found it
      on.exit(suppressWarnings(rm(".Random.seed", envir = .GlobalEnv)), add = TRUE)
    }
    set.seed(123)
    offsets <- stats::runif(length(ids), -jitter, jitter)
    names(offsets) <- as.character(ids)
    data$.ggpubr.x.jitter. <- as.integer(data[[x]]) + offsets[as.character(data$id)]
    jitter.x <- ".ggpubr.x.jitter."
  }

  position <- "identity"
  # if(length(grouping.vars) > 1)
  #   position <- position_dodge(0.8)

  condition <- val <- id <- NULL
  p <- ggplot(data, create_aes(list(x = x, y = y))) +
    geom_exec(geom_boxplot,
      data = data, color = color, fill = fill, width = width,
      position = position
    ) +
    geom_exec(geom_line,
      data = data, x = jitter.x, group = "id",
      color = line.color, linewidth = line.size, linetype = linetype,
      position = position
    ) +
    geom_exec(geom_point,
      data = data, x = jitter.x, color = color, size = point.size,
      position = position
    )

  p <- ggpar(p, palette = palette, ggtheme = ggtheme, xlab = xlab, ylab = ylab, title = title, ...)

  p
}
