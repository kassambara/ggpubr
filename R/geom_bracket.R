#' @include utilities.R
NULL

StatBracket <- ggplot2::ggproto("StatBracket", ggplot2::Stat,
                                required_aes = c("x", "y", "group"),
                                setup_params = function(data, params) {
                                  if(length(params$tip.length) == 1) params$tip.length <- rep(params$tip.length, max(length(params$xmin), 1) * 2)
                                  if(length(params$tip.length) == length(params$xmin)) params$tip.length <- rep(params$tip.length, each=2)
                                  return(params)
                                },
                                compute_group = function(data, scales, tip.length) {
                                  yrange <- scales$y$range$range
                                  y.scale.range <- yrange[2] - yrange[1]
                                  xmin <- data$xmin
                                  xmax <- data$xmax
                                  y.position <- data$y.position + (y.scale.range*data$step.increase) + data$bracket.nudge.y
                                  label <- data$label
                                  if(is.character(xmin)){
                                    xmin <- scales$x$map(xmin)
                                  }
                                  if(is.character(xmax)){
                                    xmax <- scales$x$map(xmax)
                                  }
                                  if("tip.length" %in% colnames(data)){
                                    tip.length <-  rep(data$tip.length, each=2)
                                  }
                                  # Preparing bracket data
                                  data <- dplyr::bind_rows(data, data, data)
                                  data$x <- c(xmin, xmin, xmax)
                                  data$xend = c(xmin, xmax, xmax)
                                  data$y <- c(y.position - y.scale.range*tip.length[seq_len(length(tip.length))%% 2 == 1], y.position, y.position)
                                  data$yend <- c(y.position, y.position, y.position-y.scale.range*tip.length[seq_len(length(tip.length))%% 2 == 0])
                                  data$annotation <- rep(label, 3)
                                  data
                                }
)


#' Add Brackets with Labels to a GGPlot
#' @description add brackets with label annotation to a ggplot. Helpers for
#'   adding p-value or significance levels to a plot.
#' @param label character vector with alternative label, if not null test is
#'   ignored
#' @param type the label type. Can be one of "text" and "expression" (for
#'   parsing plotmath expression).
#' @param xmin numeric vector with the positions of the left sides of the
#'   brackets
#' @param xmax numeric vector with the positions of the right sides of the
#'   brackets
#' @param y.position numeric vector with the y positions of the brackets
#' @param size change the width of the lines of the bracket
#' @param label.size change the size of the label text
#' @param family change the font used for the text
#' @param vjust move the text up or down relative to the bracket
#' @param step.increase numeric vector with the increase in fraction of total
#'   height for every additional comparison to minimize overlap.
#' @param bracket.nudge.y Vertical adjustment to nudge brackets by. Useful to
#'   move up or move down the bracket. If positive value, brackets will be moved
#'   up; if negative value, brackets are moved down.
#' @param step.group.by a variable name for grouping brackets before adding
#'   step.increase. Useful to group bracket by facet panel.
#' @param tip.length numeric vector with the fraction of total height that the
#'   bar goes down to indicate the precise column
#' @param na.rm If \code{FALSE} (the default), removes missing values with a
#'   warning.  If \code{TRUE} silently removes missing values.
#' @param ... other arguments passed on to \code{\link{layer}}. These are often
#'   aesthetics, used to set an aesthetic to a fixed value, like \code{color =
#'   "red"} or \code{size = 3}. They may also be parameters to the paired
#'   geom/stat.
#' @inheritParams ggplot2::layer
#' @examples
#' df <- ToothGrowth
#' df$dose <- factor(df$dose)
#'
#' # Add bracket with labels
#' ggboxplot(df, x = "dose", y = "len") +
#'   geom_bracket(
#'     xmin = "0.5", xmax = "1", y.position = 30,
#'     label = "t-test, p < 0.05"
#'   )
#'
#' # Customize bracket tip.length tip.length
#' ggboxplot(df, x = "dose", y = "len") +
#'   geom_bracket(
#'     xmin = "0.5", xmax = "1", y.position = 30,
#'     label = "t-test, p < 0.05", tip.length = c(0.2, 0.02)
#'   )
#'
#' #Using plotmath expression
#' ggboxplot(df, x = "dose", y = "len") +
#'  geom_bracket(
#'    xmin = "0.5", xmax = "1", y.position = 30,
#'    label = "list(~italic(p)<=0.001)", type = "expression",
#'    tip.length = c(0.2, 0.02)
#'  )
#'
#' # Specify multiple brackets manually
#' ggboxplot(df, x = "dose", y = "len") +
#'   geom_bracket(
#'     xmin = c("0.5", "1"), xmax = c("1", "2"),
#'     y.position = c(30, 35), label = c("***", "**"),
#'     tip.length = 0.01
#'   )
#'
#' # Compute statistical tests and add p-values
#' stat.test <- compare_means(len ~ dose, ToothGrowth, method = "t.test")
#' ggboxplot(df, x = "dose", y = "len") +
#'   geom_bracket(
#'     aes(xmin = group1, xmax = group2, label = signif(p, 2)),
#'     data = stat.test, y.position = 35
#'   )
#'
#' # Increase step length between brackets
#' ggboxplot(df, x = "dose", y = "len") +
#'   geom_bracket(
#'     aes(xmin = group1, xmax = group2, label = signif(p, 2)),
#'     data = stat.test, y.position = 35, step.increase = 0.1
#'   )
#'
#' # Or specify the positions of each comparison
#' ggboxplot(df, x = "dose", y = "len") +
#'   geom_bracket(
#'     aes(xmin = group1, xmax = group2, label = signif(p, 2)),
#'     data = stat.test, y.position = c(32, 35, 38)
#'    )
#' @rdname geom_bracket
#' @export
stat_bracket <- function(mapping = NULL, data = NULL,
                         position = "identity", na.rm = FALSE, show.legend = NA,
                         inherit.aes = TRUE,
                         label = NULL, type = c("text", "expression"), y.position=NULL, xmin = NULL, xmax = NULL,
                         step.increase = 0, step.group.by = NULL,  tip.length = 0.03,
                         bracket.nudge.y = 0,
                         size = 0.3, label.size = 3.88, family="", vjust = 0,
                         ...) {
  if(! is.null(data) & ! is.null(mapping)){
    if(! "x" %in% names(data)) mapping$x <- 1
    if(! "y" %in% names(data)) mapping$y <- 1
  }
  type <- match.arg(type)
  ggplot2::layer(
    stat = StatBracket, data = data, mapping = mapping, geom = "bracket",
    position = position, show.legend = show.legend, inherit.aes = inherit.aes,
    params = list(
      label=label, type = type,
      y.position=y.position,xmin=xmin, xmax=xmax,
      step.increase=step.increase, bracket.nudge.y = bracket.nudge.y, step.group.by = step.group.by,
      tip.length=tip.length, size=size, label.size=label.size,
      family=family, vjust=vjust, na.rm = na.rm, ...)
  )
}


GeomBracket <- ggplot2::ggproto("GeomBracket", ggplot2::Geom,
                                required_aes = c("x", "xend", "y", "yend", "annotation"),
                                default_aes = ggplot2::aes(
                                  shape = 19, colour = "black", label.size = 3.88, angle = 0, hjust = 0.5,
                                  vjust = 0, alpha = NA, family = "", fontface = 1, lineheight = 1.2, linetype=1, size = 0.3,
                                  xmin = NULL, xmax = NULL, label = NULL, y.position = NULL, step.increase = 0, bracket.nudge.y = 0 # Added to avoid aesthetics warning
                                  ),
                                # draw_key = function(...){grid::nullGrob()},
                                # for legend:
                                draw_key = draw_key_path,
                                draw_group = function(data, panel_params, coord, type = "text") {
                                  lab <- as.character(data$annotation)
                                  if(type == "expression"){
                                    lab <- parse_as_expression(lab)
                                  }
                                  coords <- coord$transform(data, panel_params)
                                  grid::gList(
                                    grid::textGrob(
                                      label = lab,
                                      x = mean(c(coords$x[1], tail(coords$xend, n=1))),
                                      y = max(c(coords$y, coords$yend))+0.01,
                                      default.units = "native",
                                      hjust = coords$hjust, vjust = coords$vjust,
                                      rot = coords$angle,
                                      gp = grid::gpar(
                                        col = scales::alpha(coords$colour, coords$alpha),
                                        fontsize = coords$label.size * ggplot2::.pt,
                                        fontfamily = coords$family,
                                        fontface = coords$fontface,
                                        lineheight = coords$lineheight
                                      )
                                    ),
                                    grid::segmentsGrob(
                                      coords$x, coords$y,
                                      default.units = "native",
                                      coords$xend, coords$yend,
                                      gp = grid::gpar(
                                        col = scales::alpha(coords$colour, coords$alpha),
                                        lty = coords$linetype,
                                        lwd = coords$size * ggplot2::.pt
                                      )
                                    )
                                  )
                                }
)

#' @rdname geom_bracket
#' @export
geom_bracket <- function(mapping = NULL, data = NULL, stat = "bracket",
                         position = "identity", na.rm = FALSE, show.legend = NA,
                         inherit.aes = TRUE,
                         label = NULL, type = c("text", "expression"), y.position = NULL, xmin = NULL, xmax = NULL,
                         step.increase = 0, step.group.by = NULL, tip.length = 0.03, bracket.nudge.y = 0,
                         size = 0.3, label.size = 3.88, family="", vjust = 0,
                         ...) {
  type <- match.arg(type)
  data <- build_signif_data(
    data = data, label = label, y.position = y.position,
    xmin = xmin, xmax = xmax, step.increase = step.increase,
    bracket.nudge.y = bracket.nudge.y, step.group.by = step.group.by, vjust = vjust
    )
  mapping <- build_signif_mapping(mapping, data)
  ggplot2::layer(
    stat = stat, geom = GeomBracket, mapping = mapping,  data = data,
    position = position, show.legend = show.legend, inherit.aes = inherit.aes,
    params = list(
      type = type,
      tip.length = tip.length,
      size = size, label.size = label.size,
      family=family, na.rm = na.rm, ...
    )
  )
}


# Guess column to be used as significance labem
guess_signif_label_column <- function(data){
  potential.label <- c(
    "label", "labels", "p.adj.signif", "p.adj", "padj",
    "p.signif", "p.value", "pval", "p.val", "p"
  )
  res <- intersect(potential.label, colnames(data))
  if(length(res) > 0){
    res <- res[1]
  }
  else{
    stop("label is missing")
  }
  res
}

build_signif_data <- function(data = NULL, label = NULL, y.position = NULL,
                              xmin = NULL, xmax = NULL, step.increase = 0,
                              bracket.nudge.y = 0, step.group.by = NULL, vjust = 0){

  add_step_increase <- function(data, step.increase){
    comparisons.number <- 0:(nrow(data)-1)
    step.increase <- step.increase*comparisons.number
    data <- data %>% mutate(step.increase = !!step.increase)
    data
  }
  if(is.null(data)){
    data <- data.frame(
      label = label, y.position = y.position,
      xmin = xmin, xmax = xmax
    ) %>%
      mutate(vjust = !!vjust)
  }
  else{
    if(!is.null(label)) data <- data %>% mutate(label = !!label)
    if(!is.null(y.position)) data <- data %>% mutate(y.position = !!y.position)
    if(!is.null(xmin)) data <- data %>% mutate(xmin = !!xmin)
    if(!is.null(xmax)) data <- data %>% mutate(xmax = !!xmax)
    if(!identical(vjust, 0)) data <- data %>% mutate(vjust = !!vjust)
  }
  # add vjust column if doesn't exist
  if(!("vjust" %in% colnames(data))) data <- data %>% mutate(vjust = !!vjust)
  if(!("bracket.nudge.y" %in% colnames(data))) data <- data %>% mutate(bracket.nudge.y = !!bracket.nudge.y)

  if(is.null(step.group.by)){
    data <- data %>% add_step_increase(step.increase)
  }
  else{
    data <- data %>%
      dplyr::arrange(!!!syms(c(step.group.by, "y.position"))) %>%
      group_by(!!!syms(step.group.by)) %>%
      tidyr::nest() %>%
      dplyr::mutate(step.increase = purrr::map(data, add_step_increase, !!step.increase)) %>%
      dplyr::select(-data) %>%
      unnest(cols = "step.increase")
  }
  data
}


build_signif_mapping <- function(mapping, data){
  if(is.null(mapping)){
    # Check if required variables are present in data
    required.vars <- c("xmin", "xmax", "y.position")
    missing.required.vars <- setdiff(required.vars, colnames(data))
    if(length(missing.required.vars) > 0){
      stop(
        "Required variables are missing in the data: ",
        paste(missing.required.vars, collapse = ", ")
      )
    }
    mapping <- ggplot2::aes()
  }
  if(is.null(mapping$label)){
    label.col <- guess_signif_label_column(data)
    data$label <- data %>% dplyr::pull(!!label.col)
    mapping$label <- data$label
  }
  if(is.null(mapping$xmin)) mapping$xmin <- data$xmin
  if(is.null(mapping$xmax)) mapping$xmax <- data$xmax
  if(is.null(mapping$y.position)) mapping$y.position <- data$y.position
  if(is.null(mapping$group)) mapping$group <- 1:nrow(data)
  if(is.null(mapping$step.increase)) mapping$step.increase <- data$step.increase
  if(is.null(mapping$vjust)) mapping$vjust <- data$vjust
  if(is.null(mapping$bracket.nudge.y)) mapping$bracket.nudge.y <- data$bracket.nudge.y
  if(! "x" %in% names(mapping)){
    mapping$x <- mapping$xmin
  }
  if(! "y" %in% names(mapping)){
    mapping$y <- mapping$y.position
  }
  mapping
}


# Source: https://github.com/tidyverse/ggplot2/issues/2864
parse_as_expression <- function(text) {
  stopifnot(is.character(text))
  out <- vector("expression", length(text))
  for (i in seq_along(text)) {
    expr <- parse(text = text[[i]])
    out[[i]] <- if (length(expr) == 0) NA else expr[[1]]
  }
  out
}
