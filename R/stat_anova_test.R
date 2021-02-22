#' @include utilities.R utils_stat_test_label.R
NULL
#'Add Anova Test P-values to a GGPlot
#'@description Adds automatically one-way and two-way ANOVA test p-values to a
#'  ggplot, such as box blots, dot plots and stripcharts.
#'@inheritParams ggplot2::layer
#'@inheritParams stat_pvalue_manual
#'@param method ANOVA test methods. Possible values are one of
#'  \code{c("one_way", "one_way_repeated", "two_way", "two_way_repeated",
#'  "two_way_mixed")}.
#'@param wid (factor) column name containing individuals/subjects identifier.
#'  Should be unique per individual. Required only for repeated measure tests
#'  (\code{"one_way_repeated", "two_way_repeated", "friedman_test", etc}).
#'@param group.by (optional) character vector specifying the grouping variable;
#'  it should be used only for grouped plots. Possible values are : \itemize{
#'  \item \code{"x.var"}: Group by the x-axis variable and perform the test
#'  between legend groups. In other words, the p-value is compute between legend
#'  groups at each x position \item \code{"legend.var"}: Group by the legend
#'  variable and perform the test between x-axis groups. In other words, the
#'  test is performed between the x-groups for each legend level. }
#'@param type the type of sums of squares for ANOVA. Allowed values are either
#'  1, 2 or 3. \code{type = 2} is the default because this will yield identical
#'  ANOVA results as type = 1 when data are balanced but type = 2 will
#'  additionally yield various assumption tests where appropriate. When the data
#'  are unbalanced the \code{type = 3} is used by popular commercial softwares
#'  including SPSS.
#'@param effect.size the effect size to compute and to show in the ANOVA
#'  results. Allowed values can be either "ges" (generalized eta squared) or
#'  "pes" (partial eta squared) or both. Default is "ges".
#'@param error (optional) for a linear model, an lm model object from which the
#'  overall error sum of squares and degrees of freedom are to be calculated.
#'  Read more in \code{\link[car]{Anova}()} documentation.
#'@param correction character. Used only in repeated measures ANOVA test to
#'  specify which correction of the degrees of freedom should be reported for
#'  the within-subject factors. Possible values are: \itemize{ \item{"GG"}:
#'  applies Greenhouse-Geisser correction to all within-subjects factors even if
#'  the assumption of sphericity is met (i.e., Mauchly's test is not
#'  significant, p > 0.05). \item{"HF"}: applies Hyunh-Feldt correction to all
#'  within-subjects factors even if the assumption of sphericity is met,
#'  \item{"none"}: returns the ANOVA table without any correction and
#'  \item{"auto"}: apply automatically GG correction to only within-subjects
#'  factors violating the sphericity assumption (i.e., Mauchly's test p-value is
#'  significant, p <= 0.05). }
#'@param p.adjust.method method for adjusting p values (see
#'  \code{\link[stats]{p.adjust}}).  Has impact only in a situation, where
#'  multiple pairwise tests are performed; or when there are multiple grouping
#'  variables. Allowed values include "holm", "hochberg", "hommel",
#'  "bonferroni", "BH", "BY", "fdr", "none". If you don't want to adjust the p
#'  value (not recommended), use p.adjust.method = "none".
#'@param significance a list of arguments specifying the signifcance cutpoints
#'  and symbols. For example, \code{significance <- list(cutpoints = c(0,
#'  0.0001, 0.001, 0.01, 0.05, Inf), symbols = c("****", "***", "**", "*",
#'  "ns"))}.
#'
#'  In other words, we use the following convention for symbols indicating
#'  statistical significance: \itemize{ \item \code{ns}: p > 0.05 \item
#'  \code{*}: p <= 0.05 \item \code{**}: p <= 0.01 \item \code{***}: p <= 0.001
#'  \item \code{****}:  p <= 0.0001 }
#'@param label character string specifying label. Can be: \itemize{ \item the
#'  column containing the label (e.g.: \code{label = "p"} or \code{label =
#'  "p.adj"}), where \code{p} is the p-value. Other possible values are
#'  \code{"p.signif", "p.adj.signif", "p.format", "p.adj.format"}. \item an
#'  expression that can be formatted by the \code{\link[glue]{glue}()} package.
#'  For example, when specifying \code{label = "Anova, p = \{p\}"}, the
#'  expression \{p\} will be replaced by its value. \item a combination of
#'  plotmath expressions and glue expressions. You may want some of the
#'  statistical parameter in italic; for example:\code{label = "Anova, italic(p)
#'  = {p}"}. \item a constant: \code{label = "as_italic"}: display statistical
#'  parameters in italic; \code{label = "as_detailed"}: detailed plain text;
#'  \code{label = "as_detailed_expression"} or \code{label =
#'  "as_detailed_italic"}: detailed plotmath expression. Statistical parameters
#'  will be displayed in italic.}.
#'@param label.x.npc,label.y.npc can be \code{numeric} or \code{character}
#'  vector of the same length as the number of groups and/or panels. If too
#'  short they will be recycled. \itemize{ \item If \code{numeric}, value should
#'  be between 0 and 1. Coordinates to be used for positioning the label,
#'  expressed in "normalized parent coordinates". \item If \code{character},
#'  allowed values include: i) one of c('right', 'left', 'center', 'centre',
#'  'middle') for x-axis; ii) and one of c( 'bottom', 'top', 'center', 'centre',
#'  'middle') for y-axis.}
#'@param parse If TRUE, the labels will be parsed into expressions and displayed
#'  as described in \code{?plotmath}.
#'@param label.x,label.y \code{numeric} Coordinates (in data units) to be used
#'  for absolute positioning of the label. If too short they will be recycled.
#'@param step.increase numeric value in with the increase in fraction of total
#'  height for every additional comparison to minimize overlap. The step value
#'  can be negative to reverse the order of groups.
#'@param ... other arguments to pass to
#'  \code{\link[ggplot2:geom_text]{geom_text}}, such as:\itemize{ \item
#'  \code{hjust}: horizontal justification of the text. Move the text left or
#'  right and \item \code{vjust}: vertical justification of the text. Move the
#'  text up or down. }
#'@param na.rm If FALSE (the default), removes missing values with a warning. If
#'  TRUE silently removes missing values.
#'
#'@section Computed variables: \itemize{ \item{DFn}: Degrees of Freedom in the
#'  numerator (i.e. DF effect). \item{DFd}:	Degrees of Freedom in the
#'  denominator (i.e., DF error). \item{ges}:	Generalized Eta-Squared measure of
#'  effect size. Computed only when the option \code{effect.size = "ges"}.
#'  \item{pes}:	Partial Eta-Squared measure of effect size. Computed only when
#'  the option \code{effect.size = "pes"}. \item{F}:	F-value. \item{p}:	p-value.
#'  \item{p.adj}: Adjusted p-values. \item{p.signif}: P-value significance.
#'  \item{p.adj.signif}: Adjusted p-value significance. \item{p.format}:
#'  Formated p-value. \item{p.adj.format}: Formated adjusted p-value. \item{n}:
#'  number of samples. }
#'
#' @examples
#' # Data preparation
#' #%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
#' # Transform `dose` into factor variable
#' df <- ToothGrowth
#' df$dose <- as.factor(df$dose)
#' # Add individuals id
#' df$id <- rep(1:10, 6)
#' # Add a random grouping variable
#' set.seed(123)
#' df$group <- sample(factor(rep(c("grp1", "grp2", "grp3"), 20)))
#' df$len <- ifelse(df$group == "grp2", df$len+2, df$len)
#' df$len <- ifelse(df$group == "grp3", df$len+7, df$len)
#' head(df, 3)
#'
#'
#' # Basic boxplot
#' #%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
#' # Create a basic boxplot
#' # Add 5% and 10% space to the plot bottom and the top, respectively
#' bxp <- ggboxplot(df, x = "dose", y = "len") +
#'   scale_y_continuous(expand = expansion(mult = c(0.05, 0.1)))
#'
#' # Add the p-value to the boxplot
#' bxp + stat_anova_test()
#'
#' # Change the label position
#' # Using coordinates in data units
#' bxp + stat_anova_test(label.x = "1", label.y = 10, hjust = 0)
#'
#' # Format the p-value differently
#' custom_p_format <- function(p) {
#'   rstatix::p_format(p, accuracy = 0.0001, digits = 3, leading.zero = FALSE)
#' }
#' bxp + stat_anova_test(
#'   label = "Anova, italic(p) = {custom_p_format(p)}{p.signif}"
#' )
#'
#' # Show a detailed label in italic
#' bxp + stat_anova_test(label = "as_detailed_italic")
#'
#'
#' # Faceted plots
#' #%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
#' # Create a ggplot facet
#' bxp <- ggboxplot(df, x = "dose", y = "len", facet.by = "supp") +
#'   scale_y_continuous(expand = expansion(mult = c(0.05, 0.1)))
#' # Add p-values
#' bxp + stat_anova_test()
#'
#'
#' # Grouped plots
#' #%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
#' bxp2 <- ggboxplot(df, x = "group", y = "len", color = "dose", palette = "npg")
#'
#' # For each x-position, computes tests between legend groups
#' bxp2 + stat_anova_test(aes(group = dose), label = "p = {p.format}{p.signif}")
#'
#' #  For each legend group, computes tests between x variable groups
#' bxp2 + stat_anova_test(aes(group = dose, color = dose), group.by = "legend.var")
#'
#'
#' # Two-way ANOVA: Independent measures
#' #%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
#' # Visualization: box plots with p-values
#' # Two-way interaction p-values between x and legend (group) variables
#' bxp3 <- ggboxplot(
#'   df, x = "supp", y = "len",
#'   color = "dose", palette = "jco"
#' )
#' bxp3 + stat_anova_test(aes(group = dose),  method = "two_way")
#'
#'
#' # One-way repeatead measures ANOVA
#' #%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
#' df$id <- as.factor(c(rep(1:10, 3), rep(11:20, 3)))
#' ggboxplot(df, x = "dose", y = "len") +
#'   stat_anova_test(method = "one_way_repeated", wid = "id")
#'
#' # Two-way repeatead measures ANOVA
#' #%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
#' df$id <- as.factor(rep(1:10, 6))
#' ggboxplot(df, x = "dose", y = "len", color = "supp", palette = "jco") +
#'   stat_anova_test(aes(group = supp), method = "two_way_repeated", wid = "id")
#'
#' # Grouped one-way repeated measures ANOVA
#' ggboxplot(df, x = "dose", y = "len", color = "supp", palette = "jco") +
#'   stat_anova_test(aes(group = supp, color = supp),
#'   method = "one_way_repeated", wid = "id", group.by = "legend.var")
#'@export
stat_anova_test <- function(mapping = NULL, data = NULL,
                            method = c("one_way", "one_way_repeated", "two_way", "two_way_repeated", "two_way_mixed"),
                            wid = NULL, group.by = NULL,
                            type = NULL, effect.size = "ges", error = NULL,
                            correction = c("auto", "GG", "HF", "none"),
                            label = "{method}, p = {p.format}",
                            label.x.npc = "left", label.y.npc = "top",
                            label.x = NULL, label.y = NULL, step.increase = 0.1,
                            p.adjust.method = "holm", significance = list(),
                            geom = "text", position = "identity",  na.rm = FALSE, show.legend = FALSE,
                            inherit.aes = TRUE, parse = FALSE,  ...) {

  label <- get_anova_test_label_template(label)
  if(effect.size == "pes"){
    label <- gsub(pattern = "ges", replacement = "pes", x = label, fixed = TRUE)
    label <- gsub(pattern = "eta2[g]", replacement = "eta2[p]", x = label, fixed = TRUE)
  }
  if(missing(parse) & is_plotmath_expression(label)){
    parse <- TRUE
  }
  correction <-  match.arg(correction)
  method <- match.arg(method)

  if(!is.null(wid)){
    if(!is.null(mapping)) mapping$wid <- as.name(wid)
    else mapping <- create_aes(list(wid = wid))
  }

  layer(
    stat = StatCompareMultipleMeans, data = data, mapping = mapping, geom = geom,
    position = position, show.legend = show.legend, inherit.aes = inherit.aes,
    params = list(
      method = method,
      method.args = list(type = type, effect.size = effect.size, error = error),
      group.by = group.by,
      correction = correction,
      na.rm = na.rm, stat.label = label,
      label.x.npc  = label.x.npc , label.y.npc  = label.y.npc,
      label.x = label.x, label.y = label.y, parse = parse,
      is.group.specified = is_group_aes_specified(mapping),
      step.increase = step.increase, p.adjust.method = p.adjust.method,
      significance = fortify_signif_symbols_encoding(significance), ...
    )
  )
}


StatCompareMultipleMeans <- ggproto("StatCompareMultipleMeans", Stat,
                         required_aes = c("x", "y"),
                         default_aes = aes(wid = NULL),

                         setup_params = function(data, params){

                           # Initialize parameters
                           methods_for_repeated_measures <- c(
                             "one_way_repeated", "two_way_repeated", "two_way_mixed",
                             "friedman_test"
                           )
                           if(params$method %in% methods_for_repeated_measures){
                             if(is.null(data$wid)){
                               stop(
                                 "The argument `wid` (sample id) is required for repeated measures tests.\n",
                                 "Specify it using wid = 'id_col_name', where id_col_name is the column containing ids.",
                                 call. = FALSE
                               )
                             }
                           }
                           return(params)
                         },

                         compute_panel = function(data, scales, method, method.args, group.by,
                                                  correction, p.adjust.method,
                                                  stat.label, label.x.npc, label.y.npc, label.x, label.y,
                                                  significance, is.group.specified, step.increase){
                           p <- p.adj <- x <- NULL
                           if(method %in% c("one_way_repeated", "friedman_test")){
                             # One-way repeated measures ANOVA
                             if(!is.group.specified) data$group <- data$x
                           }

                           df <- data
                           is.grouped.plots <- contains_multiple_grouping_vars(df)


                           # Statistical test parameters
                           # test_args <- get_test_args(data, between, within)
                           test_args <- get_test_args(data, method = method, group.by = group.by)
                           wid <- test_args$wid
                           between <- test_args$between
                           within <- test_args$within
                           group <- test_args$group

                           nb_vars <- unique(c(between, within)) %>% length()
                           is_two_way <- nb_vars >= 2
                           # label group id. Used to shift coordinates to avoid overlaps.
                           x.group <- y.group <-  1

                           # Run statistical tests
                           df <- df %>% rstatix::convert_as_factor(vars = c(between, within))
                           if(!is.null(group)){
                             df <- df %>% rstatix::df_group_by(vars = group)
                           }

                           anova_methods <- c("one_way", "one_way_repeated", "two_way", "two_way_repeated", "two_way_mixed")
                           if(method %in% anova_methods){
                             method.args <- method.args %>%
                               .add_item(data = df, dv = "y", between = between, within = within, wid = wid)
                             stat.test <- do.call(rstatix::anova_test, method.args) %>%
                               rstatix::get_anova_table(correction = correction)
                             method.name <- "Anova"
                           }
                           else if(method == "kruskal_test"){
                             .formula <- paste0("y ~", between) %>% as.formula()
                             method.name <- "Kruskal-Wallis"
                             stat.test <- rstatix::kruskal_test(df, .formula)
                             stat.test$statistic <- round(stat.test$statistic, 2)
                           }
                           else if(method == "welch_anova_test"){
                             .formula <- paste0("y ~", between) %>% as.formula()
                             method.name <- "Welch Anova"
                             stat.test <- rstatix::welch_anova_test(df, .formula)
                             stat.test$statistic <- round(stat.test$statistic, 2)
                           }
                           else if(method == "friedman_test"){
                             if(is.null(within)) stop("The argument 'within' is required.")
                             if(is.null(wid)) stop("The argument 'wid' is required.")
                             .formula <- paste0("y ~", within, " | ", wid) %>% as.formula()
                             method.name <- "Friedman test"
                             stat.test <- rstatix::friedman_test(df, .formula)
                             stat.test$statistic <- round(stat.test$statistic, 2)
                           }
                           else stop("Don't support the method: ", method, call. = FALSE)

                           # Prepare the output for visualization
                           if(is_two_way){
                             stat.test <- get_interaction_row(stat.test)
                           }
                           # Grouped tests
                           else if(!is.null(group)){
                             # When data is grouped by x variable and
                             # stat test computed between legend groups at each x position
                             # labels group ids are x
                             if(group == "x"){
                               x.group  <- stat.test$x
                               if(is.null(label.x) & length(label.x.npc) == 1)
                                 label.x <- stat.test$x
                             }
                             # When data is grouped by legend variable and
                             # stat test is computed between x variable groups
                             # labels are stacked on y-axis. labels group ids are group (legend var)
                             else if(group == "group"){
                               if("colour" %in% colnames(df)) stat.test$colour <- .levels(df$colour)
                               y.group  <- stat.test$group
                             }
                           }

                           sy <- significance
                           stat.test <- stat.test %>%
                             rstatix::adjust_pvalue(method = p.adjust.method) %>%
                             rstatix::add_significance(p.col = "p", cutpoints = sy$cutpoints, symbols = sy$symbols) %>%
                             rstatix::add_significance(p.col = "p.adj", cutpoints = sy$cutpoints, symbols = sy$symbols) %>%
                             rstatix::p_format(p, p.adj, new.col = TRUE, accuracy = 1e-4) %>%
                             add_stat_n() %>%
                             keep_only_tbl_df_classes() %>%
                             mutate(method = method.name) %>%
                             add_stat_label(label = stat.label)


                           stat.test$x <- get_coord(
                             data.ranges = scales$x$dimension(),
                             coord = label.x, npc = label.x.npc,
                             margin.npc = 0, group = x.group
                           )
                           stat.test$y <- get_coord(
                             data.ranges = scales$y$dimension(),
                             coord = label.y, npc = label.y.npc,
                             margin.npc = 0, group = y.group,
                             step = -step.increase
                           )

                           # set label horizontal and vertical justification
                           stat.test <- set_label_hvjust(stat.test, data = df, group)
                           stat.test
                         }
)


# Returns arguments for the tests
# data: layer data
get_test_args <- function(data, method = "one_way", group.by = NULL){
  # Sample within id (considered for repeated measures)
  wid <- between <- within <- group <-  NULL
  if("wid" %in% colnames(data)) wid <- "wid"
  # Grouped or not grouped plots
  is_grouped_plots <- data %>% contains_multiple_grouping_vars()

  if(method %in% c("one_way", "kruskal_test", "welch_anova_test")){
    between <- "x"
    if(is_grouped_plots){
      if(is.null(group.by)) group.by = "x.var"
      # Group by x axis variable and compute test between legend variable groups
      if(group.by == "x.var") {
        group <- "x"
        between <-"group"
      }
      # Group by legend variable and compute test between x axis groups
      else if (group.by == "legend.var") {
        group <- "group"
        between <- "x"
      }
    }
  }
  else if(method %in% c("one_way_repeated", "friedman_test")){
    within <- "x"
    if(is_grouped_plots){
      if(is.null(group.by)) group.by = "x.var"
      # Group by x axis variable and compute test between legend variable groups
      if(group.by == "x.var") {
        group <- "x"
        within <-"group"
      }
      # Group by legend variable and compute test between x axis groups
      else if (group.by == "legend.var") {
        group <- "group"
        within <- "x"
      }
    }
  }
  else if(method == "two_way"){
    between <- c("x", "group")
  }
  else if(method == "two_way_repeated"){
    within <- c("x", "group")
  }
  else if(method == "two_way_mixed"){
    # Auto-detect between- and within-subjects variables
    # ID should be unique for one of the grouping variable
    # It's unique for the within-ss variable when data is in a wide format
    wid.freq.group <- data %>%
      dplyr::group_by(.data$group) %>%
      dplyr::count(.data$wid) %>%
      dplyr::pull(.data$n) %>%
      unique()
    wid.freq.x <- data %>%
      dplyr::group_by(.data$x) %>%
      dplyr::count(.data$wid) %>%
      dplyr::pull(.data$n) %>%
      unique()
    if(!(all(wid.freq.group == 1) | all(wid.freq.x == 1))){
      stop("This is not a mixed design. Check your data.", call. = FALSE)
    }

    if(all(wid.freq.group == 1)){
      within <- "group"
      between <- "x"
    }
    else if(all(wid.freq.x == 1)){
      within <- "x"
      between <- "group"
    }
  } # end two-way-mixed

  list(dv = "y", between = between, within = within, wid = wid, group = group)

}



# Take the last row of the ANOVA table
# Corresponds to the interaction in the two-way model.
# In one-way: there is only one row
get_interaction_row <- function(stat.test){
  utils::tail(stat.test, 1)
}


get_anova_test_label_template <- function(label){
  switch(
    label,
    as_italic = "{method}, italic(p) = {p.format}",
    as_detailed = "{method}, F({DFn}, {DFd}) = {F}, eta2[g] = {ges}, p = {p.format}, n = {n}",
    as_detailed_italic = "{method}, italic(F)({DFn}, {DFd}) = {F}, eta2[g] = {ges}, italic(p) = {p.format}, italic(n) = {n}",
    as_detailed_expression = "{method}, italic(F)({DFn}, {DFd}) = {F}, eta2[g] = {ges}, italic(p) = {p.format}, italic(n) = {n}",
    label
  )
}


# set label horizontal and vertical justification
# group: grouping variable; either "x" or "group"
set_label_hvjust <- function(stat.test, data, group){
  # Adapt hjust and x position for grouped plots
  hjust <- 0.2
  vjust <- 0
  if(!is.null(group)){
    if(group == "x"){
      # Center label at each x position
      hjust <- 0.5
    }
    else if(group == "group"){
      n <- length(stat.test$group)
      dodge <- 0.8
      if(all(stat.test$x == 1)){
        # Move labels to the left
        stat.test$x <- round(((dodge - dodge*n) / (2*n))  + stat.test$x, 2)
      }
    }
  }
  # Two-way anova: move label to the left
  else if("Effect" %in% colnames(stat.test)){
    if(all(stat.test$Effect == "x:group") & all(stat.test$x == 1)){
      n <- length(unique(data$group))
      dodge <- 0.8
      stat.test$x <- round(((dodge - dodge*n) / (2*n))  + stat.test$x, 2)
    }
  }
  stat.test <- stat.test %>% tibble::add_column(hjust = hjust, vjust = vjust)
  stat.test
}

