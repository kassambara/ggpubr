#' @include utilities.R
NULL



#' Add Brackets with Labels to a GGPlot
#' @description add brackets with label annotation to a ggplot. Helpers for
#'   adding p-value or significance levels to a plot.
#' @param label character vector with alternative label, if not null test is
#'   ignored
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
#' @param bracket.shorten a small numeric value in [0-1] for shortening the with
#'   of bracket.
#' @param step.group.by a variable name for grouping brackets before adding
#'   step.increase. Useful to group bracket by facet panel.
#' @param tip.length numeric vector with the fraction of total height that the
#'   bar goes down to indicate the precise column
#' @param na.rm If \code{FALSE} (the default), removes missing values with a
#'   warning.  If \code{TRUE} silently removes missing values.
#' @param coord.flip logical. If \code{TRUE}, flip x and y coordinates so that
#'   horizontal becomes vertical, and vertical, horizontal. When adding the
#'   p-values to a horizontal ggplot (generated using
#'   \code{\link[ggplot2]{coord_flip}()}), you need to specify the option
#'   \code{coord.flip = TRUE}.
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
#'   geom_pwc(
#'     xmin = "0.5", xmax = "1", y.position = 30,
#'     label = "t-test, p < 0.05"
#'   )
#'
#' # Customize bracket tip.length tip.length
#' ggboxplot(df, x = "dose", y = "len") +
#'   geom_pwc(
#'     xmin = "0.5", xmax = "1", y.position = 30,
#'     label = "t-test, p < 0.05", tip.length = c(0.2, 0.02)
#'   )
#'
#' #Using plotmath expression
#' ggboxplot(df, x = "dose", y = "len") +
#'  geom_pwc(
#'    xmin = "0.5", xmax = "1", y.position = 30,
#'    label = "list(~italic(p)<=0.001)", type = "expression",
#'    tip.length = c(0.2, 0.02)
#'  )
#'
#' # Specify multiple brackets manually
#' ggboxplot(df, x = "dose", y = "len") +
#'   geom_pwc(
#'     xmin = c("0.5", "1"), xmax = c("1", "2"),
#'     y.position = c(30, 35), label = c("***", "**"),
#'     tip.length = 0.01
#'   )
#'
#' # Compute statistical tests and add p-values
#' stat.test <- compare_means(len ~ dose, ToothGrowth, method = "t.test")
#' ggboxplot(df, x = "dose", y = "len") +
#'   geom_pwc(
#'     aes(xmin = group1, xmax = group2, label = signif(p, 2)),
#'     data = stat.test, y.position = 35
#'   )
#'
#' # Increase step length between brackets
#' ggboxplot(df, x = "dose", y = "len") +
#'   geom_pwc(
#'     aes(xmin = group1, xmax = group2, label = signif(p, 2)),
#'     data = stat.test, y.position = 35, step.increase = 0.1
#'   )
#'
#' # Or specify the positions of each comparison
#' ggboxplot(df, x = "dose", y = "len") +
#'   geom_pwc(
#'     aes(xmin = group1, xmax = group2, label = signif(p, 2)),
#'     data = stat.test, y.position = c(32, 35, 38)
#'    )
#' @rdname geom_pwc
#' @export
stat_pwc <- function(mapping = NULL, data = NULL,
                     position = "identity", na.rm = FALSE, show.legend = NA,
                     inherit.aes = TRUE,
                     label = "{method}, p = {p.format}",
                     y.position=NULL,
                     step.increase = 0.12, tip.length = 0.03,
                     bracket.nudge.y = 0.05, bracket.shorten = 0,
                     size = 0.3, label.size = 3.88, family="", vjust = 0, hjust = 0.5,
                     p.adjust.method = "holm", p.adjust.by = c("group", "panel"),
                     symnum.args = list(), group.by = NULL,  parse = FALSE,
                         ...) {
  p.adjust.by <- match.arg(p.adjust.by)
  if(missing(parse) & is_plotmath_expression(label)){
    parse <- TRUE
  }
  ggplot2::layer(
    stat = StatPwc, data = data, mapping = mapping, geom = "pwc",
    position = position, show.legend = show.legend, inherit.aes = inherit.aes,
    params = list(
      stat.label = fortify_label(label),
      y.position = y.position,
      step.increase = step.increase, bracket.nudge.y = bracket.nudge.y,
      bracket.shorten = bracket.shorten,
      tip.length = tip.length, size=size, label.size=label.size,
      family=family, vjust=vjust, hjust = hjust, na.rm = na.rm,
      p.adjust.method = p.adjust.method, p.adjust.by = p.adjust.by,
      symnum.args = fortify_signif_symbols_encoding(symnum.args),
      group.by = group.by, parse = parse,
      ...)
  )
}

StatPwc <- ggplot2::ggproto("StatPwc", ggplot2::Stat,
                            required_aes = c("x", "y", "group"),
                            setup_params = function(data, params) {
                              if(length(params$tip.length) == 1) params$tip.length <- rep(params$tip.length, max(length(params$xmin), 1) * 2)
                              if(length(params$tip.length) == length(params$xmin)) params$tip.length <- rep(params$tip.length, each=2)
                              return(params)
                            },
                            compute_panel = function(data, scales, tip.length, stat.label, y.position, step.increase,
                                                     bracket.nudge.y, bracket.shorten, p.adjust.method, p.adjust.by,
                                                     symnum.args, group.by) {
                              data <- data %>% mutate(x = as.factor(.data$x))
                              df <- data
                              is.grouped.plots <- contains_multiple_grouping_vars(df)

                              formula <- y ~ x
                              if(is.grouped.plots){
                                if(is.null(group.by)) group.by <- "x.var"
                                grouping.var <- switch(
                                  group.by,
                                  x.var = "x",
                                  legend.var = "group",
                                  "x"
                                )
                                formula <- switch(
                                  grouping.var,
                                  x = y ~ group,
                                  group = y ~ x
                                )
                                df <- df %>% rstatix::df_group_by(vars = grouping.var)
                              }

                              stat.test <- rstatix::t_test(
                                df, formula = formula, detailed = TRUE,
                                p.adjust.method = p.adjust.method
                                ) %>%
                                rstatix::add_x_position(x = "x", dodge = 0.8)

                              # P-value adjustement, formatting and significance
                              if(!("p.adj" %in% colnames(stat.test))){
                                # Case of one comparison of two groups
                                stat.test <- stat.test %>% rstatix::adjust_pvalue(method = p.adjust.method)
                              }
                              # Adjust all the p-values in a given panel
                              if(p.adjust.by == "panel"){
                                stat.test <- stat.test %>% rstatix::adjust_pvalue(method = p.adjust.method)
                              }

                              sy <- symnum.args
                              stat.test <- stat.test %>%
                                rstatix::add_significance(p.col = "p", cutpoints = sy$cutpoints, symbols = sy$symbols) %>%
                                rstatix::add_significance(p.col = "p.adj", cutpoints = sy$cutpoints, symbols = sy$symbols) %>%
                                rstatix::p_format(p, p.adj, new.col = TRUE, accuracy = 1e-4) %>%
                                add_stat_n() %>%
                                keep_only_tbl_df_classes() %>%
                                add_stat_label(label = stat.label)

                              # Grouped bracket colors
                              # When data is grouped by legend variable and
                              # stat test is computed between x variable groups
                              # labels are stacked on y-axis. labels group ids are group (legend var)
                              if(is.grouped.plots){
                                if(grouping.var == "group" & "colour" %in% colnames(data)){
                                  color.data <- data %>%
                                    select(.data$group, .data$colour) %>%
                                    distinct(.data$group, .keep_all = TRUE)
                                  stat.test <- stat.test %>%
                                    dplyr::left_join(color.data, by = "group")
                                }
                              }

                              # Bracket groups, used for spacing vertically brackets
                              bracket.group = 1:nrow(stat.test)
                              step.group.by <-  "legend.var"
                              if(is.grouped.plots){
                                if(grouping.var == "x"){
                                  nb.comparisons.by.group <- stat.test %>%
                                    rstatix::df_group_by(vars = grouping.var) %>%
                                    dplyr::summarise(n = dplyr::n())
                                  bracket.group <- unlist(purrr::map(nb.comparisons.by.group$n, seq))
                                }
                                else if(grouping.var == "group" & step.group.by == "legend.var"){
                                  stat.test <- stat.test %>%
                                    dplyr::arrange(.data$group1, .data$group2)
                                }
                              }
                              #bracket.group = 1:nrow(stat.test)

                              print(as.data.frame(stat.test))

                              # Parameters for customizing brackets
                              stat.test <- stat.test %>%
                                mutate(
                                  group =  1:nrow(stat.test),
                                  bracket.group = bracket.group,
                                  step.increase = step.increase,
                                  bracket.nudge.y = bracket.nudge.y,
                                  bracket.shorten = bracket.shorten
                                )



                              # Bracket x positions
                              bracket.shorten <- stat.test$bracket.shorten/2
                              xmin <- as.numeric(stat.test$xmin) + bracket.shorten
                              xmax <- as.numeric(stat.test$xmax) - bracket.shorten

                              # Bracket y positions
                              yrange <- scales$y$range$range
                              y.scale.range <- yrange[2] - yrange[1]
                              bracket.nudge.y <- stat.test$bracket.nudge.y
                              step.increase <- stat.test$step.increase
                              if(is.null(y.position)){
                                y.position <- yrange[2] + y.scale.range * bracket.nudge.y + y.scale.range * step.increase * (bracket.group-1)
                              }
                              else if(length(y.position) == 1) {
                                y.position <- y.position + y.scale.range * bracket.nudge.y + y.scale.range * step.increase * (bracket.group-1)
                              }
                              else if(length(y.position) >= length(stat.test$group)){
                                y.position <- y.position[stat.test$group]
                              }

                              if("tip.length" %in% colnames(stat.test)){
                                tip.length <-  rep(stat.test$tip.length, each=2)
                              }
                              # Preparing bracket data
                              stat.test <- dplyr::bind_rows(stat.test, stat.test, stat.test)
                              stat.test$x <- c(xmin, xmin, xmax)
                              stat.test$xend = c(xmin, xmax, xmax)
                              stat.test$y <- c(y.position - y.scale.range*tip.length[seq_len(length(tip.length))%% 2 == 1], y.position, y.position)
                              stat.test$yend <- c(y.position, y.position, y.position-y.scale.range*tip.length[seq_len(length(tip.length))%% 2 == 0])
                              stat.test %>% select(-step.increase, -bracket.nudge.y, -bracket.shorten, -.data$.y.)
                            }
)



#' @rdname geom_pwc
#' @export
geom_pwc <- function(mapping = NULL, data = NULL, stat = "pwc",
                     position = "identity", na.rm = FALSE, show.legend = NA,
                     inherit.aes = TRUE,
                     label = "{method}, p = {p.format}",
                     y.position = NULL,
                     step.increase = 0.12, step.group.by = NULL, tip.length = 0.03,
                     bracket.nudge.y = 0.05, bracket.shorten = 0,
                     size = 0.3, label.size = 3.88, family="", vjust = 0, hjust = 0.5,
                     coord.flip = FALSE,
                     p.adjust.method = "holm", p.adjust.by = c("group", "panel"),
                     symnum.args = list(), parse = FALSE, ...) {
  p.adjust.by <- match.arg(p.adjust.by)
  if(missing(parse) & is_plotmath_expression(label)){
    parse <- TRUE
  }
  ggplot2::layer(
    stat = stat, geom = GeomPwc, mapping = mapping,  data = data,
    position = position, show.legend = show.legend, inherit.aes = inherit.aes,
    params = list(
      stat.label = fortify_label(label),
      y.position = y.position,
      step.increase = step.increase, bracket.nudge.y = bracket.nudge.y,
      bracket.shorten = bracket.shorten, tip.length = tip.length,
      size = size, label.size = label.size,
      family = family, na.rm = na.rm, coord.flip = coord.flip,
      p.adjust.method = p.adjust.method, p.adjust.by = p.adjust.by,
      symnum.args = fortify_signif_symbols_encoding(symnum.args),
      parse = parse,
      ...
    )
  )
}



GeomPwc <- ggplot2::ggproto("GeomPwc", ggplot2::Geom,
                            required_aes = c("x", "xend", "y", "yend", "label"),
                            default_aes = ggplot2::aes(
                              shape = 19, colour = "black", label.size = 3.88, angle = NA, hjust = 0.5,
                              vjust = 0, alpha = NA, family = "", fontface = 1, lineheight = 1.2, linetype=1, size = 0.3
                            ),
                            # draw_key = function(...){grid::nullGrob()},
                            # for legend:
                            draw_key = draw_key_path,
                            draw_panel = function(data, panel_params, coord,
                                                  coord.flip = FALSE, parse) {



                              lab <- as.character(data$label)
                              if(parse){
                                lab <- parse_as_expression(lab)
                              }
                              coords <- coord$transform(data, panel_params)

                              label_coords <- get_pwc_label_coords(coords, coord.flip = coord.flip )
                              label.x <- label_coords$x
                              label.y <- label_coords$y
                              label.angle <- label_coords$angle

                              grid::gList(
                                grid::textGrob(
                                  lab,
                                  x = label.x,
                                  y = label.y,
                                  default.units = "native",
                                  hjust = coords$hjust, vjust = coords$vjust,
                                  rot = label.angle,
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

# Compute the coordinates  of pairwise comparison labels
get_pwc_label_coords <- function(coords, coord.flip = FALSE){
  get_label_x <- function(coords){mean(c(coords$x[1], tail(coords$xend, n=1)))}
  get_label_y <- function(coords){max(c(coords$y, coords$yend))+0.01}
  get_label_angle <- function(coords, coord.flip = FALSE){
    label.angle <- coords$angle[1]
    if(coord.flip & is.na(label.angle)) label.angle <- -90
    if(is.na(label.angle)) label.angle <- 0
    label.angle[1]
    }

  if(coord.flip){
    get_label_x <- function(coords){max(c(coords$x, coords$xend))+0.01}
    get_label_y <- function(coords){mean(c(coords$y[1], tail(coords$yend, n=1)))}
  }

  if(!("group" %in% colnames(coords))){
    coords$group <- 1
  }
  data <- NULL
  coords %>%
    rstatix::df_nest_by(vars = "group") %>%
    dplyr::transmute(
      x = unlist(map(data, get_label_x)),
      y = unlist(map(data, get_label_y)),
      angle =  unlist(map(data, get_label_angle, coord.flip = coord.flip))
    )
}
