#' @include utilities.R utils_stat_test_label.R
NULL
#'Add Anova Test P-values to a GGPlot
#'@description Add one-way and two-way ANOVA test p-values to a ggplot, such as
#'  box blots, dot plots and stripcharts.
#'@inheritParams ggplot2::layer
#'@inheritParams stat_pvalue_manual
#'@param between (optional) between-subject factor variables. Can be specified
#'  for \strong{independent measures test}. Possible values include: \itemize{
#'  \item \code{"x"}: default for comparing the x-axis variable groups. \item
#'  \code{"group"}: for grouped plots. Can be used to compare legend variable
#'  groups at each x-position. \item \code{c("x", "group")}: for grouped plots.
#'  Can be used to compute the significance of a two-way interaction model. }
#'@param within (optional) within-subject factor variables. Can be specified for
#'  \strong{repeated-measures test}. Possible values include: \itemize{ \item
#'  \code{"x"}: default for comparing the x-axis variable groups. \item
#'  \code{"group"}: for grouped plots. Can be used to compare legend variable
#'  groups at each x-position. \item \code{c("x", "group")}: for grouped plots.
#'  Can be used to compute the significance of a two-way interaction model. }
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
#'  variables. Allowed values include "holm", "hochberg",
#'  "hommel", "bonferroni", "BH", "BY", "fdr", "none". If you don't want to
#'  adjust the p value (not recommended), use p.adjust.method = "none".
#'@param significance a list of arguments specifying the signifcance cutpoints and symbols. For
#'  example, \code{significance <- list(cutpoints = c(0, 0.0001, 0.001, 0.01,
#'  0.05, Inf), symbols = c("****", "***", "**", "*",  "ns"))}.
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
#'@section Computed variables: \itemize{ \item{DFn}: Degrees of Freedom in the numerator (i.e. DF effect).
#'  \item{DFd}:	Degrees of Freedom in the denominator (i.e., DF error).
#'  \item{ges}:	Generalized Eta-Squared measure of effect size. Computed only when the option \code{effect.size = "ges"}.
#'  \item{pes}:	Partial Eta-Squared measure of effect size. Computed only when the option \code{effect.size = "pes"}.
#'  \item{F}:	F-value.
#'  \item{p}:	p-value.
#'  \item{p.adj}: Adjusted p-values.
#'  \item{p.signif}: P-value significance.
#'  \item{p.adj.signif}: Adjusted p-value significance.
#'  \item{p.format}: Formated p-value.
#'  \item{p.adj.format}: Formated adjusted p-value.
#'  \item{n}: number of samples.
#'   }
#'
#'@export
stat_anova_test <- function(mapping = NULL, data = NULL, between = NULL, within = NULL,
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

  layer(
    stat = StatAnovaTest, data = data, mapping = mapping, geom = geom,
    position = position, show.legend = show.legend, inherit.aes = inherit.aes,
    params = list(
      between = between, within = within,
      type = type, effect.size = effect.size, error = error,
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

StatAnovaTest <- ggproto("StatAnovaTest", Stat,
                         required_aes = c("x", "y"),
                         default_aes = aes(wid = NULL),

                         setup_params = function(data, params){
                           if(!is.null(params$within) & is.null(data$wid)){
                            stop(
                              "The variable `wid` (sample id) is required when the argument `within` is specified (repeated measures).\n",
                              "Specify it using aes(wid = id_col_name), where id_col_name is the column containing ids.",
                              call. = FALSE
                            )
                           }
                           return(params)
                         },


                         compute_panel = function(data, scales, between, within, type, effect.size,error,
                                                  correction, p.adjust.method,
                                                  stat.label, label.x.npc, label.y.npc, label.x, label.y,
                                                  significance, is.group.specified, step.increase){
                           p <- p.adj <- x <- NULL
                           if(length(within) == 1 & is.null(between)){
                             # One-way repeated measures ANOVA
                             if(!is.group.specified) data$group <- data$x
                           }

                           df <- data
                           is.grouped.plots <- contains_multiple_grouping_vars(df)


                           nb_vars <- unique(c(between, within)) %>% length()
                           is_two_way <- nb_vars >= 2
                           # label group id. Used to shift coordinates to avoid overlaps.
                           x.group <- y.group <-  1

                           # Statistical test parameters
                           test_args <- get_test_args(data, between, within)
                           wid <- test_args$wid
                           between <- test_args$between
                           within <- test_args$within
                           group <- test_args$group

                           # Run statistical tests
                           df <- df %>% rstatix::convert_as_factor(vars = c(between, within))
                           if(!is.null(group)){
                             df <- df %>% rstatix::df_group_by(vars = group)
                           }

                           stat.test <- suppressMessages(rstatix::anova_test(
                             df, dv = "y", between = between,
                             within = within, wid = wid, type = type,
                             effect.size = effect.size, error = error
                           )) %>%
                             rstatix::get_anova_table(correction = correction)

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
                             tibble::add_column(method = "Anova") %>%
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
# between: independent test grouping variable
# within: repeated measures grouping variables
get_test_args <- function(data, between = NULL, within = NULL){
  # Sample within id (considered for repeated measures)
  wid <- NULL
  if("wid" %in% colnames(data)) wid <- "wid"
  # Test type: one-way or two-way
  nb_vars <- unique(c(between, within)) %>% length()
  is_two_way <- nb_vars >= 2
  # Grouped or not grouped plots
  is_grouped_plots <- data %>% contains_multiple_grouping_vars()

  results <- list(dv = "y", between = between, within = within, wid = wid)
  if(is_two_way){
    # There is no possible grouping by panel
    results <- results %>% .add_item(group = NULL)
  }
  else{
    # Grouped one-way
    if(is_grouped_plots){
      if(is.null(between) & is.null(within)){
        # Default is to group by x and perform test by legend groups
        results <- results %>% .add_item(group = "x", between = "group")
      }
      else if(!is.null(between)){
        if(between == "x") results$group <- "group"
        else if(between == "group") results$group <- "x"
      }
      else if (!is.null(within)){
        if(within == "x") results$group <- "group"
        else if(within == "group") results$group <- "x"
      }
    }
    # Standard One-way
    else{
      if(is.null(between) & is.null(within)) results$between <- "x"
      results <- results %>% .add_item(group = NULL)
    }
  }
  results
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

