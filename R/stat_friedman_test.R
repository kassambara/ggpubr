#' @include utilities.R utils_stat_test_label.R stat_anova_test.R utils-aes.R
NULL
#'Add Friedman Test P-values to a GGPlot
#'@description Add automatically Friedman test p-values to a ggplot, such as
#'  box blots, dot plots and stripcharts.
#'@inheritParams ggplot2::layer
#'@inheritParams stat_pvalue_manual
#'@inheritParams stat_anova_test
#'
#'@section Computed variables: \itemize{
#'  \item{statistic}:	the value of the test statistic (Chi-squared).
#'  \item{df}: the degrees of freedom of the approximate chi-squared distribution of the test statistic.
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
#' df$id <- as.factor(c(rep(1:10, 3), rep(11:20, 3)))
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
#' bxp + stat_friedman_test(aes(wid = id))
#'
#' # Change the label position
#' # Using coordinates in data units
#' bxp + stat_friedman_test(aes(wid = id), label.x = "1", label.y = 10, hjust = 0)
#'
#' # Format the p-value differently
#' custom_p_format <- function(p) {
#'   rstatix::p_format(p, accuracy = 0.0001, digits = 3, leading.zero = FALSE)
#' }
#' bxp + stat_friedman_test(
#'   aes(wid = id),
#'   label = "Friedman test, italic(p) = {custom_p_format(p)}{p.signif}"
#' )
#'
#' # Show a detailed label in italic
#' bxp + stat_friedman_test(aes(wid = id), label = "as_detailed_italic")
#'
#'
#' # Faceted plots
#' #%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
#' # Create a ggplot facet
#' df$id <- rep(1:10,6)
#' bxp <- ggboxplot(df, x = "dose", y = "len", facet.by = "supp") +
#'  scale_y_continuous(expand = expansion(mult = c(0.05, 0.1)))
#' # Add p-values
#' bxp + stat_friedman_test(aes(wid = id))
#'
#'
#' # Grouped plots
#' #%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
#' df$id <- rep(1:10,6)
#' bxp <- ggboxplot(df, x = "dose", y = "len", color = "supp", palette = "jco")
#'
#' # For each legend group, computes tests within x variable groups
#' bxp + stat_friedman_test(aes(wid = id, group = supp, color = supp), within = "x")
#'
#' # For each x-position, computes tests within legend variable groups
#' bxp + stat_friedman_test(
#'   aes(wid = id, group = supp, color = supp),
#'   within =  "group", label = "p = {p.format}"
#' )
#'
#' @export
stat_friedman_test <- function(mapping = NULL, data = NULL, within = NULL,
                            label = "{method}, p = {p.format}",
                            label.x.npc = "left", label.y.npc = "top",
                            label.x = NULL, label.y = NULL, step.increase = 0.1,
                            p.adjust.method = "holm", significance = list(),
                            geom = "text", position = "identity",  na.rm = FALSE, show.legend = FALSE,
                            inherit.aes = TRUE, parse = FALSE,  ...) {

  label <- get_friedman_test_label_template(label)
  if(missing(parse) & is_plotmath_expression(label)){
    parse <- TRUE
  }

  group <- aes_get_group(mapping)
  if(is.null(within)){
    if(is.null(group)) within <- "x"
    else within <- "group"
  }

  layer(
    stat = StatCompareMultipleMeans, data = data, mapping = mapping, geom = geom,
    position = position, show.legend = show.legend, inherit.aes = inherit.aes,
    params = list(
      method = "friedman_test",
      method.args = list(),
      between = NULL, within = within,
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

get_friedman_test_label_template <- function(label){
  switch(
    label,
    as_italic = "{method}, italic(p) = {p.format}",
    as_detailed = "{method}, X2({df}) = {statistic}, p = {p.format}, n = {n}",
    as_detailed_italic = "{method}, italic(chi)^2 ({df}) = {statistic}, italic(p) = {p.format}, italic(n) = {n}",
    as_detailed_expression = "{method}, italic(chi)^2({df}) = {statistic}, italic(p) = {p.format}, italic(n) = {n}",
    label
  )
}

