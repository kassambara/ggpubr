#' @include utilities.R utils_stat_test_label.R stat_anova_test.R
NULL
#'Add Kruskal-Wallis Test P-values to a GGPlot
#'@description Add Kruskal-Wallis test p-values to a ggplot, such as
#'  box blots, dot plots and stripcharts.
#'@inheritParams ggplot2::layer
#'@inheritParams stat_pvalue_manual
#'@inheritParams stat_anova_test
#'
#'@section Computed variables: \itemize{
#'  \item{statistic}:	the kruskal-wallis rank sum statistic used to compute the p-value.
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
stat_kruskal_test <- function(mapping = NULL, data = NULL, between = NULL,
                            label = "{method}, p = {p.format}",
                            label.x.npc = "left", label.y.npc = "top",
                            label.x = NULL, label.y = NULL, step.increase = 0.1,
                            p.adjust.method = "holm", significance = list(),
                            geom = "text", position = "identity",  na.rm = FALSE, show.legend = FALSE,
                            inherit.aes = TRUE, parse = FALSE,  ...) {

  label <- get_kruskal_test_label_template(label)
  if(missing(parse) & is_plotmath_expression(label)){
    parse <- TRUE
  }

  layer(
    stat = StatCompareMultipleMeans, data = data, mapping = mapping, geom = geom,
    position = position, show.legend = show.legend, inherit.aes = inherit.aes,
    params = list(
      method = "kruskal_test",
      method.args = list(),
      between = between, within = within,
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

get_kruskal_test_label_template <- function(label){
  switch(
    label,
    as_italic = "{method}, italic(p) = {p.format}",
    as_detailed = "{method}, X2({df}) = {statistic}, p = {p.format}, n = {n}",
    as_detailed_italic = "{method}, italic(chi)^2 ({df}) = {statistic}, italic(p) = {p.format}, italic(n) = {n}",
    as_detailed_expression = "{method}, italic(chi)^2({df}) = {statistic}, italic(p) = {p.format}, italic(n) = {n}",
    label
  )
}

