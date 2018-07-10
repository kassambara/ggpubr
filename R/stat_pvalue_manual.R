#' @include utilities.R
#' @importFrom dplyr pull
#' @importFrom dplyr tibble
NULL
#'Add Manually P-values to a ggplot
#'
#'@description Add manually p-values to a ggplot, such as box blots, dot plots
#'  and stripcharts.
#'@inheritParams ggsignif::geom_signif
#'@param data a data frame containing statitistical test results. The expected
#'  default format should contain the following columns: \code{group1 | group2 |
#'  p | y.position | etc}. \code{group1} and \code{group2} are the groups that
#'  have been compared. \code{p} is the resulting p-value. \code{y.position} is
#'  the y coordinates of the p-values in the plot.
#'@param label the column containing the label (e.g.: label = "p"). Default
#'  value is "p", for p-value.
#'@param y.position \code{numeric} column containing the coordinates (in data
#'  units) to be used for absolute positioning of the label. Default value is
#'  "y.position".
#'@param xmin column containing the position of the left sides of the brackets.
#'  Default value is "group1".
#'@param xmax  (optional) column containing the position of the right sides of
#'  the brackets. Default value is "group2". If NULL, the p-values are plotted
#'  as a simple text.
#'@param tip.length numeric vector with the fraction of total height that the
#'  bar goes down to indicate the precise column. Default is 0.03.
#'@seealso \code{\link{stat_compare_means}}
#'@examples
#'
#' require("dplyr")
#'
#' #:::::::::::::::::::::::::::::::::::
#' # Simple box plot
#' #:::::::::::::::::::::::::::::::::::
#' # Pairwise t-test between groups
#' stat.test <- compare_means(
#'   len ~ dose, data = ToothGrowth,
#'   method = "t.test"
#' ) %>%
#'   mutate(y.position = c(29, 35, 39))
#' stat.test
#' # Create a box plot and add the p-value
#' ggboxplot(ToothGrowth, x = "dose", y = "len") +
#'   stat_pvalue_manual(stat.test, label = "p.adj")
#'
#' #:::::::::::::::::::::::::::::::::::
#' # Use facet
#' #:::::::::::::::::::::::::::::::::::
#' # Pairwise t-test between groups
#' stat.test <- ToothGrowth %>%
#'   group_by(dose) %>%
#'   do(t_test(data = ., len ~ supp)) %>%
#'   ungroup() %>%
#'   adjust_pvalue() %>%
#'   mutate(y.position = 35)
#' stat.test
#' # Create a box plot and add the p-value
#' p <- ggboxplot(
#'   ToothGrowth, x = "supp", y = "len",
#'   color = "supp", palette = "jco",
#'   facet.by = "dose", ylim = c(0, 40)
#' )
#'
#' p + stat_pvalue_manual(stat.test, label = "p.adj")
#'
#'@export
stat_pvalue_manual <- function(
  data, label = "p", y.position = "y.position",
  xmin = "group1", xmax = "group2",
  tip.length = 0.03,
  ...
  )
{

  available.variables <- colnames(data)

  if(!(label %in% available.variables))
    stop("can't find the label variable '", label, "' in the data")
  if(!(xmin %in% available.variables))
    stop("can't find the xmin variable '", xmin, "' in the data")

  # If xmax is null, pvalue is drawn as text
  if(!is.null(xmax)) {
    xmax <- data %>% pull(xmax)
    pvalue.geom <- "bracket"
  }
  else {
    xmax <- NA
    pvalue.geom <- "text"
  }

  # Build the statistical table for plotting
  data <- data %>%
    dplyr::mutate(
      label = data %>% pull(label),
      y.position = data %>% pull(y.position),
      xmin = data %>% pull(xmin),
      xmax = xmax
    )

  if(pvalue.geom == "bracket"){
    mapping <- aes(
      xmin = xmin, xmax = xmax,
      annotations = label, y_position = y.position
    )

    ggsignif::geom_signif(
      mapping = mapping, data = data,
      manual= TRUE, tip_length =  tip.length,
      ...
    )
  }
  else{
    # X axis variable should be a factor
    if(!is.factor(data$xmin))
      data$xmin <- factor(data$xmin, levels = unique(data$xmin))

    geom_text(
      aes(x = xmin, y = y.position, label = label),
      data = data, ...
    )
  }
}
