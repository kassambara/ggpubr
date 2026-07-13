#' @include utilities.R ggpar.R
#' @importFrom rstatix df_split_by get_summary_stats
NULL
#' GGPlot with Summary Stats Table Under the Plot
#'
#' @description Create a ggplot with summary stats (n, median, mean, iqr) table
#'   under the plot. Read more: \href{https://www.datanovia.com/en/blog/how-to-create-a-beautiful-plots-in-r-with-summary-statistics-labels/}{How to Create a Beautiful Plots in R with Summary Statistics Labels}.
#' @details \code{ggsummarystats()} returns a compound object (a list of ggplots
#'   of class \code{"ggsummarystats"}), not a single ggplot, so \code{p +
#'   theme()} does \strong{not} restyle it. Theme it in one of three ways:
#'   \itemize{
#'   \item \strong{At build time} - pass \code{ggtheme =} to the builder (applied
#'   to both the main plot and the summary table).
#'   \item \strong{Per sub-plot} - the elements \code{$main.plot} (the plot) and
#'   \code{$summary.plot} (the summary table) are ordinary, editable ggplots:
#'   \code{p$main.plot <- p$main.plot + theme_bw(); p}.
#'   \item \strong{In one call} - \code{\link{style_summarystats}(p, main = ,
#'   table = )} adds ggplot components to the main plot and/or the summary table
#'   and returns the updated object.
#'   }
#' @inheritParams ggboxplot
#' @param digits integer indicating the number of decimal places (round) to be
#'   used.
#' @param table.font.size the summary table font size.
#' @param position Position adjustment, either as a string, or the result of a
#'   call to a position adjustment function.
#' @param angle numeric value specifying the rotation angle (in degrees) of the
#'   summary table text. Default is 0.
#' @param summaries summary stats to display in the table. Possible values are
#'   those returned by the function \code{\link[rstatix]{get_summary_stats}()},
#'   including: \code{"n", "min", "max",  "median",  "q1", "q2", "q3", "mad",
#'   "mean", "sd", "se", "ci"}.
#' @param ggfunc a ggpubr function, including: ggboxplot, ggviolin, ggdotplot,
#'   ggbarplot, ggline, etc. Can be any other ggplot function that accepts the
#'   following arguments \code{data, x, color, fill, palette, ggtheme,
#'   facet.by}.
#' @param free.panels logical. If TRUE, create free plot panels when the
#'   argument \code{facet.by} is specified.
#' @param labeller Character vector. An alternative to the argument
#'  \code{short.panel.labs}. Possible values are one of "label_both" (panel
#'  labelled by both grouping variable names and levels) and "label_value"
#'  (panel labelled with only grouping levels).
#' @param ... other arguments passed to the function \code{\link{ggpar}()},
#'   \code{\link{facet}()} or \code{\link{ggarrange}()} when printing the plot.
#' @examples
#' # Data preparation
#' # ::::::::::::::::::::::::::::::::::::::::::::::::
#' data("ToothGrowth")
#' df <- ToothGrowth
#' df$dose <- as.factor(df$dose)
#' # Add random QC column
#' set.seed(123)
#' qc <- rep(c("pass", "fail"), 30)
#' df$qc <- as.factor(sample(qc, 60))
#' # Inspect the data
#' head(df)
#'
#'
#' # Basic summary stats
#' # ::::::::::::::::::::::::::::::::::::::::::::::::
#' # Compute summary statistics
#' summary.stats <- df %>%
#'   group_by(dose) %>%
#'   get_summary_stats(type = "common")
#' summary.stats
#'
#' # Visualize summary table
#' ggsummarytable(
#'   summary.stats,
#'   x = "dose", y = c("n", "median", "iqr"),
#'   ggtheme = theme_bw()
#' )
#'
#'
#' # Create plots with summary table under the plot
#' # ::::::::::::::::::::::::::::::::::::::::::::::::
#' # Basic plot
#' ggsummarystats(
#'   df,
#'   x = "dose", y = "len",
#'   ggfunc = ggboxplot, add = "jitter"
#' )
#'
#' # Color by groups
#' ggsummarystats(
#'   df,
#'   x = "dose", y = "len",
#'   ggfunc = ggboxplot, add = "jitter",
#'   color = "dose", palette = "npg"
#' )
#'
#' # Create a barplot
#' ggsummarystats(
#'   df,
#'   x = "dose", y = "len",
#'   ggfunc = ggbarplot, add = c("jitter", "median_iqr"),
#'   color = "dose", palette = "npg"
#' )
#'
#' # Facet
#' # ::::::::::::::::::::::::::::::::::::::::::::::::
#' # Specify free.panels = TRUE for free panels
#' ggsummarystats(
#'   df,
#'   x = "dose", y = "len",
#'   ggfunc = ggboxplot, add = "jitter",
#'   color = "dose", palette = "npg",
#'   facet.by = c("supp", "qc"),
#'   labeller = "label_both"
#' )
#'
#' @describeIn ggsummarystats Create a table of summary stats
#' @export
ggsummarytable <- function(data, x, y, digits = 0, size = 3, color = "black", palette = NULL,
                           facet.by = NULL, labeller = "label_value", position = "identity",
                           angle = 0, ggtheme = theme_pubr(), ...) {
  if (missing(ggtheme) & !is.null(facet.by)) {
    ggtheme <- theme_pubr(border = TRUE)
  }
  if (is.null(names(y))) names(y) <- y
  y_values <- as.vector(y)

  df <- as.data.frame(data)
  df$x <- df[[x]]
  if (color %in% colnames(df)) {
    if (missing(position)) position <- position_dodge(0.8)
    group <- color
  } else {
    group <- 1
  }

  df <- df %>%
    mutate(across(where(is.double), ~ round(.x, digits))) %>%
    unite(col = "label", !!!syms(y_values), sep = "\n") %>%
    mutate(y = paste(names(y), collapse = "\n"))
  p <- ggplot(data, aes(x, y)) +
    geom_exec(
      geom_text,
      data = df,
      label = "label", size = size, color = color, group = group,
      position = position, angle = angle
    )
  p <- ggpar(p, ggtheme = ggtheme, palette = palette, xlab = x, ...)
  if (!is.null(facet.by)) p <- facet(p, facet.by = facet.by, labeller = labeller, ...)
  p + rremove("ylab")
}


#' @describeIn ggsummarystats Create a ggplot with a summary stat table under the plot.
#' @export
ggsummarystats <- function(data, x, y, summaries = c("n", "median", "iqr"),
                           ggfunc = ggboxplot,
                           color = "black", fill = "white", palette = NULL,
                           facet.by = NULL, free.panels = FALSE, labeller = "label_value",
                           heights = c(0.80, 0.20), digits = 0, table.font.size = 3,
                           ggtheme = theme_pubr(), ...) {
  if (missing(ggtheme) & !is.null(facet.by)) {
    ggtheme <- theme_pubr(border = TRUE)
  }

  env <- c(as.list(environment()), list(...))
  # Sugar: `comparisons =` implies the comparison-figure builder ggcompare(),
  # unless `ggfunc` was given explicitly. Without this, `comparisons` would fall
  # into `...` and be silently dropped. `comparisons = NULL` leaves env unchanged.
  if (missing(ggfunc) && !is.null(env$comparisons)) {
    env$ggfunc <- ggcompare
  }
  if (is.null(facet.by)) {
    results <- do.call(ggsummarystats_core, env)
  } else {
    if (free.panels) {
      results <- do.call(ggsummarystats_free_facet, env)
    } else {
      results <- do.call(ggsummarystats_core, env)
    }
  }
  results
}


#' @method print ggsummarystats
#' @param x an object of class \code{ggsummarystats}.
#' @param heights a numeric vector of length 2, specifying the heights of the
#'   main and the summary table, respectively.
#' @rdname ggsummarystats
#' @export
print.ggsummarystats <- function(x, heights = c(0.80, 0.20), ...) {
  res <- ggarrange(plotlist = x, heights = heights, align = "v", ncol = 1)
  print(res)
  invisible(res)
}

#' @method print ggsummarystats_list
#' @param legend character specifying legend position. Allowed values are one of
#'   c("top", "bottom", "left", "right", "none"). To remove the legend use
#'   legend = "none".
#' @param x a list of \code{ggsummarystats}.
#' @rdname ggsummarystats
#' @export
print.ggsummarystats_list <- function(x, heights = c(0.80, 0.20), legend = NULL, ...) {
  # Create a common legend, if legend exists
  legend.grob <- get_legend(x[[1]]$main.plot, position = legend)
  has.legend <- !is.null(legend.grob)
  remove_legend <- function(ggsummarystats) {
    ggsummarystats[[1]] <- ggsummarystats[[1]] + theme(legend.position = "none")
    ggsummarystats
  }
  if (has.legend) {
    x <- map(x, remove_legend)
  }
  # Combining each ggsummarystats
  x <- x %>%
    map(
      function(x, ...) {
        ggarrange(plotlist = x, ...)
      },
      heights = heights, align = "v", ncol = 1
    )
  # Combine the list of ggsumarystats
  res <- ggarrange(plotlist = x, legend = legend, legend.grob = legend.grob)
  # Add legend if exist
  print(res)
  invisible(res)
}


ggsummarystats_core <- function(data, x, y, summaries = c("n", "median", "iqr"),
                                ggfunc = ggboxplot,
                                color = "black", fill = "white", palette = NULL,
                                ggtheme = theme_pubr(), heights = c(0.80, 0.20),
                                facet.by = NULL, free.panels = FALSE, labeller = "label_value",
                                digits = 0, table.font.size = 3, ...) {
  groups <- c(x, color, fill, facet.by) %>%
    unique() %>%
    intersect(colnames(data))
  summary.stats <- suppressWarnings(
    data %>%
      group_by(!!!syms(groups)) %>%
      get_summary_stats(!!y)
  )
  # No need to repeat the panel label on the table
  table.facet.by <- facet.by
  if (free.panels) table.facet.by <- NULL

  main.plot <- ggfunc(
    data,
    x = x, y = y, color = color, fill = fill,
    palette = palette,
    ggtheme = ggtheme,
    facet.by = facet.by, labeller = labeller, ...
  )
  summary.plot <- ggsummarytable(
    summary.stats,
    x = x, y = summaries,
    color = color, palette = palette, legend = "none",
    ggtheme = ggtheme,
    facet.by = table.facet.by, labeller = labeller,
    digits = digits, size = table.font.size
  ) +
    clean_table_theme()

  plots <- list(
    main.plot = main.plot,
    summary.plot = summary.plot
  )
  class(plots) <- c("ggsummarystats", "list")
  plots
}


ggsummarystats_free_facet <- function(data, x, y, facet.by, labeller = "label_value", ...) {
  labeller_func <- switch(labeller,
    label_both = rstatix::df_label_both,
    label_value = rstatix::df_label_value
  )
  groups <- facet.by
  data.grouped <- data %>%
    df_split_by(vars = groups, label_col = "panel", labeller = labeller_func) %>%
    mutate(
      plots = map(data, ggsummarystats_core, x = x, y = y, facet.by = "panel", ...)
    )
  plots <- data.grouped$plots
  names(plots) <- data.grouped$panel
  class(plots) <- c("ggsummarystats_list", "list")
  plots
}


#' Restyle a ggsummarystats Composite
#' @description Adds ggplot components (themes, labels, scales, ...) to the
#'   sub-plots of a \code{\link{ggsummarystats}()} object and returns the updated
#'   object, so the whole composite can be restyled in one call. This is a
#'   convenience wrapper around editing the \code{$main.plot} and
#'   \code{$summary.plot} elements directly.
#' @param p a \code{"ggsummarystats"} object.
#' @param main a ggplot component (or a list of components) added to the main
#'   plot (\code{$main.plot}), e.g. \code{theme_bw()} or
#'   \code{list(theme_bw(), labs(title = "My title"))}.
#' @param table a ggplot component (or list of components) added to the summary
#'   table (\code{$summary.plot}).
#' @return the updated \code{"ggsummarystats"} object.
#' @seealso \code{\link{ggsummarystats}}.
#' @examples
#' data("ToothGrowth")
#' df <- ToothGrowth
#' df$dose <- as.factor(df$dose)
#' p <- ggsummarystats(df, x = "dose", y = "len",
#'   ggfunc = ggboxplot, color = "dose", palette = "npg")
#'
#' # Restyle the main plot in one call
#' style_summarystats(p, main = list(theme_bw(), labs(title = "Tooth growth")))
#' @export
style_summarystats <- function(p, main = NULL, table = NULL) {
  if (!inherits(p, "ggsummarystats")) {
    stop("`p` must be a `ggsummarystats` object (from ggsummarystats()).",
      call. = FALSE)
  }
  add_all <- function(plot, additions) {
    if (is.null(additions)) return(plot)
    if (inherits(additions, "gg") || !is.list(additions)) {
      additions <- list(additions)
    }
    for (a in additions) plot <- plot + a
    plot
  }
  p$main.plot <- add_all(p$main.plot, main)
  p$summary.plot <- add_all(p$summary.plot, table)
  p
}
