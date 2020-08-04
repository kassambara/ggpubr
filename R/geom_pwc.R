#' @include utilities.R
NULL

#'Add Pairwise Comparisons P-values to a GGPlot
#'@description add pairwise comparison p-values to a ggplot such as box plots,
#'  dot plots and stripcharts.
#'@param method a character string indicating which method to be used for
#'  pairwise comparisons. Default is \code{"wilcox_test"}. Allowed methods
#'  include pairwise comparisons methods implemented in the \code{rstatix} R
#'  package. These methods are: \code{"wilcox_test", "t_test", "sign_test",
#'  "dunn_test", "emmeans_test", "tukey_hsd", "games_howell_test"}.
#'@param method.args a list of additional arguments used for the test method.
#'  For example one might use \code{method.args = list(alternative = "greater")}
#'  for wilcoxon test.
#'@param label character string specifying label. Can be: \itemize{ \item the
#'  column containing the label (e.g.: \code{label = "p"} or \code{label =
#'  "p.adj"}), where \code{p} is the p-value. Other possible values are
#'  \code{"p.signif", "p.adj.signif", "p.format", "p.adj.format"}. \item an
#'  expression that can be formatted by the \code{\link[glue]{glue}()} package.
#'  For example, when specifying \code{label = "Wilcoxon, p = \{p\}"}, the
#'  expression \{p\} will be replaced by its value. \item a combination of
#'  plotmath expressions and glue expressions. You may want some of the
#'  statistical parameter in italic; for example:\code{label = "Wilcoxon,
#'  italic(p)= {p}"}}.
#'@param y.position numeric vector with the y positions of the brackets
#'@param group.by (optional) character vector specifying the grouping variable;
#'  it should be used only for grouped plots. Possible values are : \itemize{
#'  \item \code{"x.var"}: Group by the x-axis variable and perform the test
#'  between legend groups. In other words, the p-value is compute between legend
#'  groups at each x position \item \code{"legend.var"}: Group by the legend
#'  variable and perform the test between x-axis groups. In other words, the
#'  test is performed between the x-groups for each legend level. }
#'@param dodge dodge width for grouped ggplot/test. Default is 0.8. It's used to
#'  dodge the brackets position when \code{group.by = "legend.var"}.
#'@param stack logical value. Default is FALSE; should be set to TRUE for
#'  stacked bar plots or line plots. If TRUE, then the brackets are
#'  automatically removed and the \code{dodge} value is set to zero.
#'@param bracket.nudge.y Vertical adjustment to nudge brackets by (in fraction
#'  of the total height). Useful to move up or move down the bracket. If
#'  positive value, brackets will be moved up; if negative value, brackets are
#'  moved down.
#'@param bracket.shorten a small numeric value in [0-1] for shortening the width
#'  of bracket.
#'@param bracket.group.by (optional); a variable name for grouping brackets
#'  before adding step.increase. Useful for grouped plots. Possible values
#'  include \code{"x.var"} and \code{"legend.var"}.
#'@param step.increase numeric vector with the increase in fraction of total
#'  height for every additional comparison to minimize overlap.
#'@param tip.length numeric vector with the fraction of total height that the
#'  bar goes down to indicate the precise column/
#'@param size change the width of the lines of the bracket
#'@param label.size change the size of the label text
#'@param family change the font used for the text
#'@param vjust move the text up or down relative to the bracket.
#'@param hjust move the text left or right relative to the bracket.
#'@param p.adjust.method method for adjusting p values (see
#'  \code{\link[stats]{p.adjust}}).  Has impact only in a situation, where
#'  multiple pairwise tests are performed; or when there are multiple grouping
#'  variables. Ignored when the specified method is \code{"tukey_hsd"} or
#'  \code{"games_howell_test"} because they come with internal p adjustment
#'  method. Allowed values include "holm", "hochberg", "hommel", "bonferroni",
#'  "BH", "BY", "fdr", "none". If you don't want to adjust the p value (not
#'  recommended), use p.adjust.method = "none".
#'@param p.adjust.by possible value is one of \code{c("group", "panel")}.
#'  Default is \code{"group"}: for a grouped data, if pairwise test is
#'  performed, then the p-values are adjusted for each group level
#'  independently. P-values are adjusted by panel when \code{p.adjust.by =
#'  "panel"}.
#'@param symnum.args a list of arguments to pass to the function
#'  \code{\link[stats]{symnum}} for symbolic number coding of p-values. For
#'  example, \code{symnum.args <- list(cutpoints = c(0, 0.0001, 0.001, 0.01,
#'  0.05, Inf), symbols = c("****", "***", "**", "*",  "ns"))}.
#'
#'  In other words, we use the following convention for symbols indicating
#'  statistical significance: \itemize{ \item \code{ns}: p > 0.05 \item
#'  \code{*}: p <= 0.05 \item \code{**}: p <= 0.01 \item \code{***}: p <= 0.001
#'  \item \code{****}:  p <= 0.0001 }
#'@param hide.ns can be logical value or a character vector.
#'@param remove.bracket logical, if \code{TRUE}, brackets are removed from the
#'  plot. \itemize{ \item Case when logical value. If TRUE, hide ns symbol when
#'  displaying significance levels. Filter is done by checking the column
#'  \code{p.adj.signif}, \code{p.signif}, \code{p.adj} and \code{p}. \item Case
#'  when character value. Possible values are "p" or "p.adj", for filtering out
#'  non significant. }
#'@param na.rm If \code{FALSE} (the default), removes missing values with a
#'  warning.  If \code{TRUE} silently removes missing values.
#'@param parse logical for parsing plotmath expression.
#'@param ... other arguments passed on to \code{\link{layer}}. These are often
#'  aesthetics, used to set an aesthetic to a fixed value, like \code{color =
#'  "red"} or \code{size = 3}. They may also be parameters to the paired
#'  geom/stat.
#'@inheritParams ggplot2::layer
#' @examples
#' df <- ToothGrowth
#' df$dose <- factor(df$dose)
#'
#'@rdname geom_pwc
#'@export
stat_pwc <- function(mapping = NULL, data = NULL,
                     method = "wilcox_test", method.args = list(),
                     label = "p.format",
                     y.position = NULL,
                     group.by = NULL, dodge = 0.8,
                     bracket.nudge.y = 0.05, bracket.shorten = 0,
                     bracket.group.by = c("x.var", "legend.var"),
                     step.increase = 0.12, tip.length = 0.03,
                     size = 0.3, label.size = 3.88, family="", vjust = 0, hjust = 0.5,
                     p.adjust.method = "holm", p.adjust.by = c("group", "panel"),
                     symnum.args = list(), hide.ns = FALSE,
                     position = "identity", na.rm = FALSE, show.legend = NA,
                     inherit.aes = TRUE, parse = FALSE, ...) {

  p.adjust.by <- match.arg(p.adjust.by)
  if(missing(parse) & is_plotmath_expression(label)){
    parse <- TRUE
  }
  bracket.group.by <- match.arg(bracket.group.by)
  ggplot2::layer(
    stat = StatPwc, data = data, mapping = mapping, geom = "pwc",
    position = position, show.legend = show.legend, inherit.aes = inherit.aes,
    params = list(
      method = method, method.args = method.args,
      stat.label = label,
      y.position = y.position, group.by = group.by, dodge = dodge,
      bracket.nudge.y = bracket.nudge.y, bracket.shorten = bracket.shorten,
      bracket.group.by = bracket.group.by, step.increase = step.increase,
      tip.length = tip.length, size=size, label.size=label.size,
      family=family, vjust=vjust, hjust = hjust, na.rm = na.rm,
      p.adjust.method = p.adjust.method, p.adjust.by = p.adjust.by,
      symnum.args = fortify_signif_symbols_encoding(symnum.args),
      hide.ns = hide.ns, parse = parse, ...)
  )
}

StatPwc <- ggplot2::ggproto("StatPwc", ggplot2::Stat,
                            required_aes = c("x", "y", "group"),
                            setup_params = function(data, params) {
                              if(length(params$tip.length) == 1) params$tip.length <- rep(params$tip.length, max(length(params$xmin), 1) * 2)
                              if(length(params$tip.length) == length(params$xmin)) params$tip.length <- rep(params$tip.length, each=2)

                              # Statistical test methods functions and arguments
                              method <- params$method
                              method.args <- params$method.args
                              if(is.null(method.args$p.adjust.method)){
                                method.args$p.adjust.method <- params$p.adjust.method
                              }
                              pwc_func <- get_pwc_stat_function(method, method.args)
                              params$method <- pwc_func$method
                              params$method.args <- pwc_func$method.args
                              return(params)
                            },
                            compute_panel = function(self, data, scales, method, method.args,
                                                     tip.length, stat.label, y.position, step.increase,
                                                     bracket.nudge.y, bracket.shorten, bracket.group.by,
                                                     p.adjust.method, p.adjust.by,
                                                     symnum.args, hide.ns, group.by, dodge) {

                              # Compute the statistical tests
                              df <- data %>% mutate(x = as.factor(.data$x))
                              is.grouped.plots <- contains_multiple_grouping_vars(df)
                              formula <- y ~ x
                              grouping.var <- NULL
                              if(is.grouped.plots){
                                if(is.null(group.by)) group.by <- "x.var"
                                if(group.by == "legend.var"){
                                  grouping.var <- "group"
                                  formula <- y ~ x
                                }
                                else {
                                  grouping.var <- "x"
                                  formula <- y ~ group
                                }
                                df <- df %>% rstatix::df_group_by(vars = grouping.var)
                              }
                              method.args <- method.args %>% .add_item(data = df, formula = formula)
                              stat.test <- do.call(method, method.args)

                              # Add method name
                              method.name <- rstatix::get_description(stat.test)
                              if(method.name == "") {
                                stat.label <- gsub(pattern = "\\{method\\},\\s?", replacement = "", stat.label)
                              }
                              stat.test$method <- method.name

                              # P-value adjustment, formatting and significance
                              if(!("p.adj" %in% colnames(stat.test))){
                                # Case of one comparison of two groups
                                stat.test <- stat.test %>% rstatix::adjust_pvalue(method = p.adjust.method)
                              }

                              # Adjust all the p-values in a given panel
                              # no matter the grouping
                              if(p.adjust.by == "panel"){
                                if("p" %in% colnames(stat.test)){
                                  stat.test <- stat.test %>%
                                    rstatix::adjust_pvalue(method = p.adjust.method)
                                }
                                else{
                                  warning(
                                    "p-values can't be adjusted by panel for the specified stat method.\n",
                                    "The result of the method doesn't contain the p column.\n",
                                    "Note that, tests such as tukey_hsd or games_howell_test handle p-value adjustement ",
                                    "internally; they only return the p.adj.",
                                    call. = FALSE
                                    )
                                }
                              }

                              if(!("p" %in% colnames(stat.test))){
                                # Case when method is tukey or games-howel, etc
                                # doesn't return p but p.adj
                                stat.test <- stat.test %>%
                                  tibble::add_column(p = NA, .before = "p.adj")
                                stat.label <- gsub(pattern = "p.format", replacement = "p.adj.format", stat.label)
                              }

                              sy <- symnum.args
                              stat.test <- stat.test %>%
                                add_x_position(x = "x", group = "group", dodge = dodge) %>%
                                rstatix::add_significance(p.col = "p", cutpoints = sy$cutpoints, symbols = sy$symbols) %>%
                                rstatix::add_significance(p.col = "p.adj", cutpoints = sy$cutpoints, symbols = sy$symbols) %>%
                                rstatix::p_format(p, p.adj, new.col = TRUE, accuracy = 1e-4) %>%
                                add_stat_n() %>%
                                keep_only_tbl_df_classes() %>%
                                add_stat_label(label = stat.label)

                              # Hide NS
                              if(is.logical(hide.ns)){
                                if(hide.ns) stat.test <- remove_ns(stat.test)
                              }
                              else if (is.character(hide.ns)){
                                if(hide.ns == "p") stat.test <- stat.test %>% filter(.data$p <= 0.05)
                                else if(hide.ns == "p.adj") stat.test <- stat.test %>% filter(.data$p.adj <= 0.05)
                              }

                              # Grouped bracket colors
                              # When data is grouped by legend variable and
                              # stat test is computed between x variable groups
                              # labels are stacked on y-axis. labels group ids are group (legend var)
                              if(is.grouped.plots){
                                if(grouping.var == "group" & "colour" %in% colnames(data)){
                                  color.data <- data %>%
                                    select(.data$group, .data$colour) %>%
                                    distinct(.data$group, .keep_all = TRUE)
                                  stat.test <- stat.test %>% dplyr::left_join(color.data, by = "group")
                                }
                              }

                              # Bracket groups, used for spacing vertically brackets
                              bracket.group <- 1
                              if(nrow(stat.test) > 1) bracket.group <- 1:nrow(stat.test)
                              if(is.grouped.plots){
                                if(grouping.var == "x"){
                                  nb.comparisons.by.group <- stat.test %>%
                                    rstatix::df_group_by(vars = grouping.var) %>%
                                    dplyr::summarise(n = dplyr::n())
                                  bracket.group <- unlist(purrr::map(nb.comparisons.by.group$n, seq))
                                }
                                else if(grouping.var == "group" & bracket.group.by == "legend.var"){
                                  stat.test <- stat.test %>% dplyr::arrange(.data$group1, .data$group2)
                                }
                              }

                              # Parameters for customizing brackets
                              group <- 1
                              if(nrow(stat.test) > 1) group <- 1:nrow(stat.test)
                              stat.test <- stat.test %>%
                                mutate(
                                  group =  group,
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
                              stat.test %>% select(-step.increase, -bracket.nudge.y, -bracket.shorten)
                            }
)



#' @rdname geom_pwc
#' @export
geom_pwc <- function(mapping = NULL, data = NULL, stat = "pwc",
                     method = "wilcox_test", method.args = list(),
                     label = "p.format",
                     y.position = NULL, group.by = NULL, dodge = 0.8, stack = FALSE,
                     step.increase = 0.12,  tip.length = 0.03,
                     bracket.nudge.y = 0.05, bracket.shorten = 0, bracket.group.by = c("x.var", "legend.var"),
                     size = 0.3, label.size = 3.88, family="", vjust = 0, hjust = 0.5,
                     p.adjust.method = "holm", p.adjust.by = c("group", "panel"),
                     symnum.args = list(), hide.ns = FALSE, remove.bracket = FALSE,
                     position = "identity", na.rm = FALSE,
                     show.legend = NA, inherit.aes = TRUE, parse = FALSE, ...) {
  p.adjust.by <- match.arg(p.adjust.by)
  if(missing(parse) & is_plotmath_expression(label)){
    parse <- TRUE
  }
  bracket.group.by <- match.arg(bracket.group.by)
  if(stack){
    # for stacked bar plots/line plots
    dodge <- 0
    if (group.by == "x.var") remove.bracket <- TRUE
  }
  ggplot2::layer(
    stat = stat, geom = GeomPwc, mapping = mapping,  data = data,
    position = position, show.legend = show.legend, inherit.aes = inherit.aes,
    params = list(
      method = method, method.args = method.args,
      stat.label = label,
      y.position = y.position, group.by = group.by, dodge = dodge,
      bracket.nudge.y = bracket.nudge.y,
      bracket.shorten = bracket.shorten, bracket.group.by = bracket.group.by,
      step.increase = step.increase, tip.length = tip.length,
      size = size, label.size = label.size,
      family = family, na.rm = na.rm, hjust = hjust, vjust = vjust,
      p.adjust.method = p.adjust.method, p.adjust.by = p.adjust.by,
      symnum.args = fortify_signif_symbols_encoding(symnum.args),
      hide.ns = hide.ns, remove.bracket = remove.bracket,
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
                                                  parse = FALSE,
                                                  remove.bracket = FALSE) {

                              coords <- coord$transform(data, panel_params)
                              coord.flip <- inherits(coord, "CoordFlip")
                              if(remove.bracket){
                                text_grob <- get_text_grob(data, coords, coord.flip = coord.flip, parse = parse)
                                return(text_grob)
                              }
                              grid::gList(
                                get_text_grob(data, coords, coord.flip = coord.flip, parse = parse),
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

# Text grob ------------------------------
get_text_grob <- function(data, coords, coord.flip = FALSE, parse = FALSE){
  lab <- as.character(data$label)
  if(parse) lab <- parse_as_expression(lab)
  label_coords <- get_pwc_label_coords(coords, coord.flip = coord.flip )
  grid::textGrob(
    lab,
    x = label_coords$x,
    y = label_coords$y,
    default.units = "native",
    hjust = coords$hjust, vjust = coords$vjust,
    rot = label_coords$angle,
    gp = grid::gpar(
      col = scales::alpha(coords$colour, coords$alpha),
      fontsize = coords$label.size * ggplot2::.pt,
      fontfamily = coords$family,
      fontface = coords$fontface,
      lineheight = coords$lineheight
    )
  )
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

# Compute the coordinates  of pairwise comparison labels
get_pwc_label_coords <- function(coords, coord.flip = FALSE){
  get_label_x <- function(coords){mean(c(coords$x[1], utils::tail(coords$xend, n=1)))}
  get_label_y <- function(coords){max(c(coords$y, coords$yend))+0.01}
  get_label_angle <- function(coords, coord.flip = FALSE){
    label.angle <- coords$angle[1]
    if(coord.flip & is.na(label.angle)) label.angle <- -90
    if(is.na(label.angle)) label.angle <- 0
    label.angle[1]
    }

  if(coord.flip){
    get_label_x <- function(coords){max(c(coords$x, coords$xend))+0.01}
    get_label_y <- function(coords){mean(c(coords$y[1], utils::tail(coords$yend, n=1)))}
  }

  if(!("group" %in% colnames(coords))){
    coords$group <- 1
  }
  data <- NULL
  coords %>%
    rstatix::df_nest_by(vars = c("group", "bracket.group")) %>%
    dplyr::transmute(
      x = unlist(map(data, get_label_x)),
      y = unlist(map(data, get_label_y)),
      angle =  unlist(map(data, get_label_angle, coord.flip = coord.flip))
    )
}

# Get the pairwise comparison stat function
# returns a list(method, method.args)
get_pwc_stat_function <- function(method, method.args){
  if(is.character(method)){
    # replace t.test by t_test, etc
    method <- gsub(pattern = ".", replacement = "_", method, fixed = TRUE)
    if(method == "wilcoxon") method <- "wilcox_test"
    else if(method == "emmeans") method <- "emmeans_test"
    else if(method == "games_howell") method <- "games_howell_test"
    allowed.methods <- c(
      "wilcox_test", "wilcoxon", "t_test", "sign_test",
      "dunn_test", "emmeans_test", "emmeans", "tukey_hsd",
      "games_howell_test"
    )
    if(!(method %in% allowed.methods)){
      stop(
        "Unknown stat method: '", method, "'. ",
        "Allowed method names are: ", paste(allowed.methods, collapse = ","),
        call. = FALSE
      )
    }
    # Ignoring p.adjust.method for tukey_hsd and games_howell
    # because they come with internal adjustment method
    if(method %in% c("tukey_hsd", "games_howell_test"))
      method.args$p.adjust.method <- NULL
    stat_func <- switch(
      method,
      wilcox_test = rstatix::wilcox_test,
      t_test = rstatix::t_test,
      sign_test = rstatix::sign_test,
      dunn_test = rstatix::dunn_test,
      emmeans_test = rstatix::emmeans_test,
      tukey_hsd = function(data, formula, ...){rstatix::tukey_hsd(data, formula, ...)},
      games_howell_test = rstatix::games_howell_test,
      games_howell = rstatix::games_howell_test
    )
  }
  else if(is.function(method)){
    stat_func <- method
  }
  else{
    stop("The argument method is required.")
  }
  list(
    method = stat_func,
    method.args = method.args
  )
}


# stat.test: rstatix test
# x: x variable
# group: group variable (legend variable)
# add_grouped_x_coords (stat.test, data, x = "x", group = "group", dodge = 0.8)
add_x_position <- function(stat.test, x, group = NULL, dodge = 0.8){

  # Checking
  groups.exist <- all(c("group1", "group2") %in% colnames(stat.test))
  if(!groups.exist) stop("data should contain group1 and group2 columns")
  if(any(stat.test$group1 %in% c("all", ".all."))) {
    # case when ref.group = "all"
    stat.test$group1 <- stat.test$group2
  }
  groups <- c(as.character(stat.test$group1), as.character(stat.test$group2)) %>%
    unique() %>%
    setdiff(c("all", ".all."))   # case when ref.group = "all"

  is_rstatix_test <- inherits(stat.test, "rstatix_test")
  is.null.model <- ("null model" %in% stat.test$group2) & all(stat.test$group1 %in% 1)
  is.grouped.by.legend <- stat.test %>% is_stat_test_grouped_by(group)
  is.grouped.by.x <- stat.test %>% is_stat_test_grouped_by(x)
  is.basic <- !is.grouped.by.x & !is.grouped.by.legend

  # Data preparation
  if(is_rstatix_test) {
    data <- attr(stat.test, "args")$data
  }
  else if(is.basic){
    data <- data.frame(x = groups, stringsAsFactors = FALSE)
    x <- "x"
  }
  else{
    if(is.grouped.by.x)
      data <- expand.grid(x = unique(stat.test[[x]]), group = groups)
    else if(is.grouped.by.legend)
      data <- expand.grid(x = groups, group = stat.test[[group]])
    colnames(data) <- c(x, group)
  }
  if(is.null.model) {
    data$group <- rep(1, nrow(data))
    group <- "group"
  }

  # Add xmin and x max
  if(is.basic){
    x_coords <- as_numeric_group(data[[x]])
    xmin_id <- as.character(stat.test$group1)
    xmax_id <- as.character(stat.test$group2)
  }
  else{
    x_coords <- get_grouped_x_position(data, x = x, group = group, dodge = dodge)
    if(is.grouped.by.legend){
      # Add x position to stat test when the test is grouped by the legend variable
      # Case when you group by legend and pairwise compare between x-axis groups
      xmin_id <- paste(stat.test$group1, stat.test[[group]], sep = "_")
      xmax_id <- paste(stat.test$group2, stat.test[[group]], sep = "_")
    }
    else if(is.grouped.by.x){
      # Add x position to stat test when the test is grouped by the x variable
      # Case when you pairwise compare legend groups at each x-axis position,
      # so the data is grouped by x position
      xmin_id <- paste(stat.test[[x]], stat.test$group1, sep = "_")
      xmax_id <- paste(stat.test[[x]], stat.test$group2, sep = "_")
      stat.test$x <- as_numeric_group(stat.test[[x]])
    }
  }
  stat.test$xmin <- x_coords[xmin_id]
  stat.test$xmax <- x_coords[xmax_id]
  if(is.null.model) stat.test$xmax <- stat.test$xmin
  stat.test
}


# Compute grouped x positions or coordinates
# data is a dataframe containing the x and the group columns
get_grouped_x_position<- function(data, x, group, dodge = 0.8){
  data <- data.frame(x = data[[x]], group = data[[group]]) %>%
    dplyr::distinct(.data$x, .data$group)
  x.position <- as_numeric_group(data$x)
  group.ranks <- as_numeric_group(data$group)
  # Compute grouped x coords
  n <- length(unique(data$group))
  x_coords <-  (((dodge - dodge*n) / (2*n)) + ((group.ranks - 1) * (dodge / n))) + x.position
  names(x_coords) <- paste(data$x, data$group, sep = "_")
  x_coords
}




# Check if a stat test is grouped by a given variable
is_stat_test_grouped_by <- function(test, x = NULL){
  answer <- FALSE
  if(!is.null(x)){
    if(x %in% colnames(test)){
      answer <- TRUE
    }
  }
  answer
}

# Return a numeric named vector
# c("a", "b", "a") -> c(a = 1, b = 2, a = 1)
as_numeric_group <- function(x){
  grp <- x %>% as.factor() %>% as.numeric()
  names(grp) <- x
  grp
}

