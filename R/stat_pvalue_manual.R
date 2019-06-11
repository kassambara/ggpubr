#' @include utilities.R
#' @importFrom dplyr pull
#' @importFrom dplyr tibble
#' @importFrom glue glue
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
#'  value is "p", for p-value. Can be also an expression that can be formatted
#'  by the \code{\link[glue]{glue}()} package. For example, when specifying
#'  label = "t-test, p = \{p\}", the expression \{p\} will be replaced by its
#'  value.
#'@param y.position column containing the coordinates (in data units) to be used
#'  for absolute positioning of the label. Default value is "y.position". Can be
#'  also a numeric vector.
#'@param xmin column containing the position of the left sides of the brackets.
#'  Default value is "group1".
#'@param xmax  (optional) column containing the position of the right sides of
#'  the brackets. Default value is "group2". If NULL, the p-values are plotted
#'  as a simple text.
#'@param x x position of the p-value. Should be used only when you want plot the
#'  p-value as text (without brackets).
#'@param size,label.size size of label text.
#'@param bracket.size Width of the lines of the bracket.
#'@param tip.length numeric vector with the fraction of total height that the
#'  bar goes down to indicate the precise column. Default is 0.03.
#'@param remove.bracket logical, if \code{TRUE}, brackets are removed from the
#'  plot. Considered only in the situation, where comparisons are performed
#'  against reference group or against "all".
#'@param position position adjustment, either as a string, or the result of a
#'  call to a position adjustment function.
#'@seealso \code{\link{stat_compare_means}}
#'@examples
#'
#'# T-test
#'stat.test <- compare_means(
#'  len ~ dose, data = ToothGrowth,
#'  method = "t.test"
#')
#'stat.test
#'
#'# Create a simple box plot
#'p <- ggboxplot(ToothGrowth, x = "dose", y = "len")
#'p
#'
#'# Perform a t-test between groups
#'stat.test <- compare_means(
#'  len ~ dose, data = ToothGrowth,
#'  method = "t.test"
#')
#'stat.test
#'
#'# Add manually p-values from stat.test data
#'# First specify the y.position of each comparison
#'stat.test <- stat.test %>%
#'  mutate(y.position = c(29, 35, 39))
#'p + stat_pvalue_manual(stat.test, label = "p.adj")
#'
#'# Customize the label with glue expression
#'# (https://github.com/tidyverse/glue)
#'p + stat_pvalue_manual(stat.test, label = "p = {p.adj}")
#'
#'
#' # Grouped bar plots
#' #%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
#' ToothGrowth$dose <- as.factor(ToothGrowth$dose)
#' # Comparisons against reference
#' stat.test <- compare_means(
#'   len ~ dose, data = ToothGrowth, group.by = "supp",
#'   method = "t.test", ref.group = "0.5"
#' )
#' stat.test
#' # Plot
#' bp <- ggbarplot(ToothGrowth, x = "supp", y = "len",
#'                 fill = "dose", palette = "jco",
#'                 add = "mean_sd", add.params = list(group = "dose"),
#'                 position = position_dodge(0.8))
#' bp + stat_pvalue_manual(
#'   stat.test, x = "supp", y.position = 33,
#'   label = "p.signif",
#'   position = position_dodge(0.8)
#' )
#'
#'@export
stat_pvalue_manual <- function(
  data, label = "p", y.position = "y.position",
  xmin = "group1", xmax = "group2", x = NULL,
  size = 3.88, label.size = size, bracket.size = 0.3, tip.length = 0.03,
  remove.bracket = FALSE, position = "identity", ...
  )
{
  asserttat_group_columns_exists(data)
  comparison <- detect_comparison_type(data)
  # detect automatically if xmin and xmax exists in the data.
  if(is.null(x) & missing(xmin) & missing(xmax)){
    if(all(c("xmin", "xmax") %in% colnames(data))){
      xmin <- "xmin"
      xmax <- "xmax"
    }
  }
  # If label is a glue package expression
  if(.contains_curlybracket(label)){
    data <- data %>% mutate(label = glue(label))
    label <- "label"
  }
  # P-value displayed as text (without brackets)
  if(!missing(x)){
    xmin <- x
    xmax <- NULL
  }

  available.variables <- colnames(data)

  if(!(label %in% available.variables))
    stop("can't find the label variable '", label, "' in the data")
  if(!(xmin %in% available.variables))
    stop("can't find the xmin variable '", xmin, "' in the data")

  if(remove.bracket){
    group1.length <- data %>% pull(!!xmin) %>%
      unique() %>% length()
    if(group1.length == 1) {
      xmax <- NULL
      if(missing(xmin)) xmin <- "group2"
    }
    else warning("Pairwise comparison: bracket can't be removed", call. = FALSE)
  }

  y.position <- .valide_y_position(y.position, data)
  if(is.numeric(y.position))
    data$y.position <- y.position

  # If xmax is null, pvalue is drawn as text
  if(!is.null(xmax)) {
    xmax <- data %>% pull(!!xmax)
    pvalue.geom <- "bracket"
  }
  else {
    xmax <- NA
    pvalue.geom <- "text"
  }

  # Build the statistical table for plotting
  xxmax <-xmax  # so that mutate will avoid re-using an existing xmax in the data
  data <- data %>%
    dplyr::mutate(
      label = as.character(data %>% pull(!!label)),
      y.position = data %>% pull(y.position),
      xmin = data %>% pull(!!xmin),
      xmax = xxmax
    )

  if(pvalue.geom == "bracket"){
    if(identical(data$xmin, data$xmax)){
      # case when ref.group = "all"
      bracket.size = 0
    }
    mapping <- aes(
      xmin = xmin, xmax = xmax,
      annotations = label, y_position = y.position,
      group = 1:nrow(data)
    )
    ggsignif::geom_signif(
      mapping = mapping, data = data,
      manual= TRUE, tip_length =  tip.length,
      textsize = label.size, size = bracket.size,
      position = position, ...
    )
  }
  else{
    if(comparison == "each_vs_ref"){
      ref.group <- unique(data$group1)
      group2 <- NULL
      data <- add_ctr_rows(data, ref.group = ref.group)
      mapping <- aes(x = xmin, y = y.position, label = label, group = group2)
      if(missing(position) & !missing(x)){
        if (is_grouping_variable(x))
          position <- position_dodge(0.8)
      }
    }
    else{
      mapping <- aes(x = xmin, y = y.position, label = label)
    }
    # X axis variable should be a factor
    #if(!is.factor(data$xmin))
      #data$xmin <- factor(data$xmin, levels = unique(data$xmin))
    geom_text(mapping, data = data, size = label.size, position = position, ...)
  }
}

asserttat_group_columns_exists <- function(data){
  groups.exist <- all(c("group1", "group2") %in% colnames(data))
  if(!groups.exist){
    stop("data should contain group1 and group2 columns")
  }
}

# get validate p-value y-position
.valide_y_position <- function(y.position, data){
  if(is.numeric(y.position)){
    number.of.test <- nrow(data)
    number.of.ycoord <- length(y.position)
    xtimes <- number.of.test/number.of.ycoord

    if(number.of.ycoord < number.of.test)
      y.position <- rep(y.position, xtimes)
  }
  else if(is.character(y.position)){
    if(!(y.position %in% colnames(data)))
      stop("can't find the y.position variable '", y.position, "' in the data")
  }
  return(y.position)
}


# Check if a string contains curly bracket
.contains_curlybracket <- function(x){
  grepl("\\{|\\}", x, perl = TRUE)
}

# For ctr rows: the comparaison of ctr against itself
# useful only when positionning the label of grouped bars
add_ctr_rows <- function(data, ref.group){
  xmin <- NULL
  ctr <- data %>%
    dplyr::distinct(xmin, .keep_all = TRUE) %>%
    mutate(group2 = ref.group) %>%
    mutate(label = " ")
  dplyr::bind_rows(ctr, data)
}

# Returns the type of comparisons: one_group, two_groups, each_vs_ref, pairwise
detect_comparison_type <- function(data){
  ngroup1 <- unique(data$group1) %>% length()
  ngroup2 <- unique(data$group2) %>% length()
  if(is_null_model(data)){
    type = "one_group"
  }
  else if(ngroup1 == 1 & ngroup2 >= 2){
    type = "each_vs_ref"
  }
  else if(ngroup1 == 1 & ngroup2 == 1){
    type = "two_groups"
  }
  else if (ngroup1 >= 2 & ngroup2 >= 2){
    type = "pairwise"
  }
  else{
    stop("Make sure that group1 and group2 columns exist in the data")
  }
  type
}


is_null_model <- function(data){
  group2 <- unique(data$group2)
  .diff <- setdiff(group2, "null model")
  length(.diff) == 0
}

is_grouping_variable <- function(x){
  !(x %in% c("group1", "group2"))
}
