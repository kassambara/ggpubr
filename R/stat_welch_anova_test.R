#' @include utilities.R utils_stat_test_label.R stat_anova_test.R
NULL
#'Add Welch One-Way ANOVA Test P-values to a GGPlot
#'@description Add Welch one-way ANOVA test p-values to a ggplot, such as
#'  box blots, dot plots and stripcharts.
#'@inheritParams ggplot2::layer
#'@inheritParams stat_pvalue_manual
#'@inheritParams stat_anova_test
#'
#'@section Computed variables: \itemize{
#'  \item{statistic}:	the value of the test statistic (F-value)
#'  \item{DFn}: Degrees of Freedom in the numerator (i.e. DF effect)
#'  \item{DFd}:	Degrees of Freedom in the denominator (i.e., DF error)
#'  \item{p}:	p-value.
#'  \item{p.adj}: Adjusted p-values.
#'  \item{p.signif}: P-value significance.
#'  \item{p.adj.signif}: Adjusted p-value significance.
#'  \item{p.format}: Formated p-value.
#'  \item{p.adj.format}: Formated adjusted p-value.
#'  \item{n}: number of samples.
#'   }
#'
#' @examples
#' # Data preparation
#' #%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
#' # Transform `dose` into factor variable
#' df <- ToothGrowth
#' df$dose <- as.factor(df$dose)
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
#' bxp + stat_welch_anova_test()
#'
#' # Change the label position
#' # Using coordinates in data units
#' bxp + stat_welch_anova_test(label.x = "1", label.y = 10, hjust = 0)
#'
#' # Format the p-value differently
#' custom_p_format <- function(p) {
#'   rstatix::p_format(p, accuracy = 0.0001, digits = 3, leading.zero = FALSE)
#' }
#' bxp + stat_welch_anova_test(
#'   label = "Welch Anova, italic(p) = {custom_p_format(p)}{p.signif}"
#' )
#'
#' # Show a detailed label in italic
#' bxp + stat_welch_anova_test(label = "as_detailed_italic")
#'
#'
#' # Faceted plots
#' #%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
#' # Create a ggplot facet
#' bxp <- ggboxplot(df, x = "dose", y = "len", facet.by = "supp") +
#'  scale_y_continuous(expand = expansion(mult = c(0.05, 0.1)))
#' # Add p-values
#' bxp + stat_welch_anova_test()
#'
#'
#' # Grouped plots
#' #%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
#' bxp2 <- ggboxplot(df, x = "group", y = "len", color = "dose", palette = "npg")
#'
#' # For each x-position, computes tests between legend groups
#' bxp2 + stat_welch_anova_test(aes(group = dose), label = "p = {p.format}{p.signif}")
#'
#' #  For each legend group, computes tests between x variable groups
#' bxp2 + stat_welch_anova_test(aes(group = dose, color = dose), group.by = "legend.var")
#'
#' @export
stat_welch_anova_test <- function(mapping = NULL, data = NULL, group.by = NULL,
                            label = "{method}, p = {p.format}",
                            label.x.npc = "left", label.y.npc = "top",
                            label.x = NULL, label.y = NULL, step.increase = 0.1,
                            p.adjust.method = "holm", significance = list(),
                            geom = "text", position = "identity",  na.rm = FALSE, show.legend = FALSE,
                            inherit.aes = TRUE, parse = FALSE,  ...) {

  label <- get_welch_anova_test_label_template(label)
  if(missing(parse) & is_plotmath_expression(label)){
    parse <- TRUE
  }

  layer(
    stat = StatCompareMultipleMeans, data = data, mapping = mapping, geom = geom,
    position = position, show.legend = show.legend, inherit.aes = inherit.aes,
    params = list(
      method = "welch_anova_test",
      method.args = list(),
      group.by = group.by,
      correction = "none",
      na.rm = na.rm, stat.label = label,
      label.x.npc  = label.x.npc , label.y.npc  = label.y.npc,
      label.x = label.x, label.y = label.y, parse = parse,
      is.group.specified = is_group_aes_specified(mapping),
      step.increase = step.increase, p.adjust.method = p.adjust.method,
      significance = fortify_signif_symbols_encoding(significance), ...
    )
  )
}

get_welch_anova_test_label_template <- function(label){
  switch(
    label,
    as_italic = "{method}, italic(p) = {p.format}",
    as_detailed = "{method}, F({DFn}, {DFd}) = {statistic}, p = {p.format}, n = {n}",
    as_detailed_italic = "{method}, italic(F)({DFn}, {DFd}) = {F}, italic(p) = {p.format}, italic(n) = {n}",
    as_detailed_expression = "{method}, italic(F)({DFn}, {DFd}) = {F}, italic(p) = {p.format}, italic(n) = {n}",
    label
  )
}
