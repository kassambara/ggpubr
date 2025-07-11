#' @include utilities.R utils_stat_test_label.R utils-aes.R
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
#'@param ref.group a character string or a numeric value specifying the
#'  reference group. If specified, for a given grouping variable, each of the
#'  group levels will be compared to the reference group (i.e. control group).
#'
#'  \code{ref.group} can be also \code{"all"}. In this case, each of the
#'  grouping variable levels is compared to all (i.e. basemean).
#'
#'  Allowed values can be: \itemize{ \item \strong{numeric value}: specifying
#'  the rank of the reference group. For example, use \code{ref.group = 1} when
#'  the first group is the reference; use \code{ref.group = 2} when the second
#'  group is the reference, and so on. This works for all situations, including
#'  i) when comparisons are performed between x-axis groups and ii) when
#'  comparisons are performed between legend groups. \item \strong{character
#'  value}: For example, you can use \code{ref.group = "ctrl"} instead of using
#'  the numeric rank value of the "ctrl" group. \item \strong{"all"}: In this
#'  case, each of the grouping variable levels is compared to all (i.e.
#'  basemean). }
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
#'  example, \code{symnum.args = list(cutpoints = c(0, 0.0001, 0.001, 0.01,
#'  0.05, Inf), symbols = c("****", "***", "**", "*",  "ns"))}.
#'
#'  In other words, we use the following convention for symbols indicating
#'  statistical significance: \itemize{ \item \code{ns}: p > 0.05 \item
#'  \code{*}: p <= 0.05 \item \code{**}: p <= 0.01 \item \code{***}: p <= 0.001
#'  \item \code{****}:  p <= 0.0001 }
#'@param hide.ns can be logical value (\code{TRUE} or \code{FALSE}) or a character vector (\code{"p.adj"} or \code{"p"}).
#'@param remove.bracket logical, if \code{TRUE}, brackets are removed from the
#'  plot. \itemize{ \item Case when logical value. If TRUE, hide ns symbol when
#'  displaying significance levels. Filter is done by checking the column
#'  \code{p.adj.signif}, \code{p.signif}, \code{p.adj} and \code{p}. \item Case
#'  when character value. Possible values are "p" or "p.adj", for filtering out
#'  non significant. }
#'@param na.rm If \code{FALSE} (the default), removes missing values with a
#'  warning.  If \code{TRUE} silently removes missing values.
#'@param parse logical for parsing plotmath expression.
#'@param ... other arguments passed on to \code{\link[ggplot2]{layer}()}). These are often
#'  aesthetics, used to set an aesthetic to a fixed value, like \code{color =
#'  "red"} or \code{size = 3}. They may also be parameters to the paired
#'  geom/stat.
#' @details
#' \bold{Notes on adjusted p-values and facet}. When using the ggplot facet functions, the p-values are computed and adjusted by panel, without taking into account the other panels. This is by design in ggplot2.
#'
#' In this case, when there is only one computed p-value by panel, then using `label = "p"` or `label = "p.adj"` will give the same results using `geom_pwc()`. Again, p-value computation and adjustment in a given facet panel is done independently to the other panels.
#'
#' One might want to adjust the p-values of all the facet panels together. There are two solutions for that:
#'
#' \itemize{
#' \item Using \code{\link{ggadjust_pvalue}(p)} after creating the plot \code{p}
#' \item or adding the adjusted p-value manually using \code{\link{stat_pvalue_manual}()}. Read more at:
#' \itemize{
#'   \item \href{https://www.datanovia.com/en/blog/how-to-add-p-values-to-ggplot-facets/}{How to Add P-values to GGPLOT Facets}
#'   \item \href{https://www.datanovia.com/en/blog/add-p-values-to-ggplot-facets-with-different-scales/}{Add P-values to GGPLOT Facets with Different Scales}
#' }
#' }
#' @inheritParams ggpubr-common-params
#'@inheritParams ggplot2::layer
#' @examples
#' df <- ToothGrowth
#' df$dose <- factor(df$dose)
#'
#'@rdname geom_pwc
#' @seealso \code{\link{ggadjust_pvalue}}
#'@examples
#' # Data preparation
#' #%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
#' # Transform `dose` into factor variable
#' df <- ToothGrowth
#' df$dose <- as.factor(df$dose)
#' # Add a random grouping variable
#' df$group <- factor(rep(c("grp1", "grp2"), 30))
#' head(df, 3)
#'
#'
#' # Two groups by x position
#' #%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
#'
#' # Create a box plot
#' # Add 10% spaces between the p-value labels and the plot border
#' bxp <- ggboxplot(
#'   df, x = "dose", y = "len",
#'   color = "supp", palette = c("#00AFBB", "#E7B800")
#' ) +
#'  scale_y_continuous(expand = expansion(mult = c(0.05, 0.10)))
#'
#'
#' # Add p-values onto the box plots
#' # label can be "p.format"  or "p.adj.format"
#' bxp + geom_pwc(
#'   aes(group = supp), tip.length = 0,
#'   method = "t_test", label = "p.format"
#' )
#'
#' # Show adjusted p-values and significance levels
#' # Hide ns (non-significant)
#' bxp + geom_pwc(
#'   aes(group = supp), tip.length = 0,
#'   method = "t_test", label = "{p.adj.format}{p.adj.signif}",
#'   p.adjust.method = "bonferroni", p.adjust.by = "panel",
#'   hide.ns = TRUE
#' )
#'
#' # Complex cases
#' #%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
#' # 1. Add p-values of OJ vs VC at each dose group
#' bxp.complex <- bxp +
#'   geom_pwc(
#'     aes(group = supp), tip.length = 0,
#'     method = "t_test", label = "p.adj.format",
#'     p.adjust.method = "bonferroni", p.adjust.by = "panel"
#'   )
#' # 2. Add pairwise comparisons between dose levels
#' # Nudge up the brackets by 20% of the total height
#' bxp.complex <- bxp.complex +
#'   geom_pwc(
#'     method = "t_test", label = "p.adj.format",
#'     p.adjust.method = "bonferroni",
#'     bracket.nudge.y = 0.2
#'   )
#' # 3. Display the plot
#' bxp.complex
#'
#'
#' # Three groups by x position
#' #%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
#'
#' # Simple plots
#' #_____________________________________
#'
#' # Box plots with p-values
#' bxp <- ggboxplot(
#'   df, x = "supp", y = "len", fill = "dose",
#'   palette = "npg"
#' )
#' bxp +
#'   geom_pwc(
#'     aes(group = dose), tip.length = 0,
#'     method = "t_test", label = "p.adj.format",
#'     bracket.nudge.y = -0.08
#'   ) +
#'   scale_y_continuous(expand = expansion(mult = c(0, 0.1)))
#'
#' # Bar plots with p-values
#' bp <- ggbarplot(
#'   df, x = "supp", y = "len", fill = "dose",
#'   palette = "npg", add = "mean_sd",
#'   position = position_dodge(0.8)
#' )
#' bp +
#'   geom_pwc(
#'     aes(group = dose), tip.length = 0,
#'     method = "t_test", label = "p.adj.format",
#'     bracket.nudge.y = -0.08
#'   ) +
#'   scale_y_continuous(expand = expansion(mult = c(0, 0.1)))
#'
#'@export
stat_pwc <- function(mapping = NULL, data = NULL,
                     method = "wilcox_test", method.args = list(), ref.group = NULL,
                     label = "p.format",
                     y.position = NULL,
                     group.by = NULL, dodge = 0.8,
                     bracket.nudge.y = 0.05, bracket.shorten = 0,
                     bracket.group.by = c("x.var", "legend.var"),
                     step.increase = 0.12, tip.length = 0.03,
                     size = 0.3, label.size = 3.88, family="", vjust = 0, hjust = 0.5,
                     p.adjust.method = "holm", p.adjust.by = c("group", "panel"),
                     symnum.args = list(), hide.ns = FALSE, remove.bracket = FALSE,
                     position = "identity", na.rm = FALSE, show.legend = NA,
                     inherit.aes = TRUE, parse = FALSE, ...) {

  p.adjust.by <- match.arg(p.adjust.by)
  if(missing(parse) & is_plotmath_expression(label)){
    parse <- TRUE
  }
  if(is.null(group.by)) group.by <- "x.var"
  bracket.group.by <- match.arg(bracket.group.by)

  if(!is.null(ref.group)){
    if(ref.group %in% c("all", ".all.")){
      if(missing(step.increase)) step.increase <- 0
      remove.bracket <- TRUE
    }
  }

  # Keep legend variable in memory, we'll be useful to guess ref.group id
  # related question: https://stackoverflow.com/questions/63640543/how-to-access-to-a-legend-group-id-inside-a-ggplot2-extension
  legend.var <- aes_get_group(mapping)
  if(!is.null(legend.var)) mapping$legend.var <- as.name(legend.var)

  ggplot2::layer(
    stat = StatPwc, data = data, mapping = mapping, geom = "pwc",
    position = position, show.legend = show.legend, inherit.aes = inherit.aes,
    params = list(
      method = method, method.args = method.args, ref.group = ref.group,
      stat.label = label,
      y.position = y.position, group.by = group.by, dodge = dodge,
      bracket.nudge.y = bracket.nudge.y, bracket.shorten = bracket.shorten,
      bracket.group.by = bracket.group.by, step.increase = step.increase,
      tip.length = tip.length, size=size, label.size=label.size,
      remove.bracket = remove.bracket,
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
                            compute_panel = function(self, data, scales, method, method.args, ref.group,
                                                     tip.length, stat.label, y.position, step.increase,
                                                     bracket.nudge.y, bracket.shorten, bracket.group.by,
                                                     p.adjust.method, p.adjust.by,
                                                     symnum.args, hide.ns, group.by, dodge, remove.bracket) {

                              # Compute the statistical tests
                              df <- data %>% mutate(x = as.factor(.data$x))
                              is.grouped.plots <- contains_multiple_grouping_vars(df)
                              formula <- y ~ x
                              grouping.var <- NULL
                              if(is.grouped.plots){
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
                              is.comparisons.between.legend.grps <- is.grouped.plots & group.by == "x.var"

                              # Comparison against reference group
                              ref.group.id <- get_ref_group_id(
                                scales = scales, data = df, ref.group = ref.group,
                                is.comparisons.between.legend.grps = is.comparisons.between.legend.grps
                                )
                              if(!is.null(ref.group.id)) {
                                method.args <- method.args %>% .add_item(ref.group = ref.group.id)
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
                                rstatix::add_x_position(x = "x", group = "group", dodge = dodge) %>%
                                rstatix::add_significance(p.col = "p", cutpoints = sy$cutpoints, symbols = sy$symbols) %>%
                                rstatix::add_significance(p.col = "p.adj", cutpoints = sy$cutpoints, symbols = sy$symbols) %>%
                                rstatix::p_format(p, p.adj, new.col = TRUE, accuracy = 1e-4) %>%
                                add_stat_n() %>%
                                keep_only_tbl_df_classes() %>%
                                add_stat_label(label = stat.label)

                              if(!is.null(ref.group)){
                                if(!(ref.group %in% c(".all.", "all"))){
                                  # when comparisons is done against reference group
                                  if(remove.bracket) stat.test$xmin <- stat.test$xmax
                                }
                              }

                              # Hide NS if hide.ns not null
                              stat.test <- rstatix::remove_ns(stat.test, col = hide.ns)


                              # Grouped bracket colors
                              # When data is grouped by legend variable and
                              # stat test is computed between x variable groups
                              # labels are stacked on y-axis. labels group ids are group (legend var)
                              if(is.grouped.plots){
                                if(grouping.var == "group" & "colour" %in% colnames(data)){
                                  color.data <- data %>%
                                    select("group", "colour") %>%
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
                     method = "wilcox_test", method.args = list(), ref.group = NULL,
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
  if(is.null(group.by)) group.by <- "x.var"
  if(stack){
    # for stacked bar plots/line plots
    dodge <- 0
    if (group.by == "x.var") remove.bracket <- TRUE
  }

  if(!is.null(ref.group)){
    if(ref.group %in% c("all", ".all.")){
      if(missing(step.increase)) step.increase <- 0
      remove.bracket <- TRUE
    }
  }

  # Keep legend variable in memory, we'll be useful to guess ref.group id
  # related question: https://stackoverflow.com/questions/63640543/how-to-access-to-a-legend-group-id-inside-a-ggplot2-extension
  legend.var <- aes_get_group(mapping)
  if(!is.null(legend.var)) mapping$legend.var <- as.name(legend.var)

  ggplot2::layer(
    stat = stat, geom = GeomPwc, mapping = mapping,  data = data,
    position = position, show.legend = show.legend, inherit.aes = inherit.aes,
    params = list(
      method = method, method.args = method.args, ref.group = ref.group,
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
                              vjust = 0, alpha = NA, family = "", fontface = 1, lineheight = 1.2, linetype=1, size = 0.3,
                              legend.var = NA
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

# Statistical tests --------------------------------------------------
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

# Get ref group rank id for comparison against reference -------------------------------------
# param scales: ggproto scales
# param data: ggproto data
# param ref.group: group name
# Returns 1 (for the first group), 2 for the second group
get_ref_group_id <- function(scales, data, ref.group = NULL, is.comparisons.between.legend.grps = FALSE){
  if(is.null(ref.group)) return(NULL)
  else if(ref.group %in% c(".all.", "all")) return(ref.group)
  # Grouped plots
  if(is.comparisons.between.legend.grps){
    if(is.character(ref.group)){
      if("legend.var" %in% colnames(data)){
        ref.group <- get_group_id(data$legend.var, ref.group)
        if(is.na(ref.group)) stop("The ref.group ('", ref.group, "') doesn't exist.", call. = FALSE)
      }
      else{
        stop(
          "ref.group should be a numeric value indicating the rank of the ",
          "legend group to be used as the reference. \n",
          "For example, use ref.group = 1 when the first group is the reference; \n",
          "use ref.group = 2 when the second group is the reference and so on.",
          call. = FALSE
        )
      }
    }
  }
  # Basic plot: comparison between x-axis groups
  else if(is.character(ref.group)){
    ref.group <- scales$x$map(ref.group) %>% as.character()
  }
  as.character(ref.group)
}


# x a factor
# group a level
get_group_id <- function(x, group){
  ids <- x %>% as.factor() %>% as.numeric()
  names(ids) <- x
  unname(ids[group]) %>% unique()
}
