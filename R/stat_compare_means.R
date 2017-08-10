#' @include utilities.R utilities_label.R
NULL
#'Add Mean Comparison P-values to a ggplot
#'@description Add mean comparison p-values to a ggplot, such as box blots, dot
#'  plots and stripcharts.
#'@inheritParams ggplot2::layer
#'@inheritParams compare_means
#'@param method a character string indicating which method to be used for
#'  comparing means.
#'@param comparisons A list of length-2 vectors. The entries in the vector are
#'  either the names of 2 values on the x-axis or the 2 integers that correspond
#'  to the index of the groups of interest, to be compared.
#'@param hide.ns logical value. If TRUE, hide ns symbol when displaying
#'  significance levels.
#'@param label character string specifying label type. Allowed values include
#'  "p.signif" (shows the significance levels), "p.format" (shows the formatted p value).
#'@param label.sep a character string to separate the terms. Default is ", ", to
#'  separate the correlation coefficient and the p.value.
#'@param label.x.npc,label.y.npc can be \code{numeric} or \code{character}
#'  vector of the same length as the number of groups and/or panels. If too
#'  short they will be recycled. \itemize{ \item If \code{numeric}, value should
#'  be between 0 and 1. Coordinates to be used for positioning the label,
#'  expressed in "normalized parent coordinates". \item If \code{character},
#'  allowed values include: i) one of c('right', 'left', 'center', 'centre',
#'  'middle') for x-axis; ii) and one of c( 'bottom', 'top', 'center', 'centre',
#'  'middle') for y-axis.}
#'
#'  If too short they will be recycled.
#'@param label.x,label.y \code{numeric} Coordinates (in data units) to be used
#'  for absolute positioning of the label. If too short they will be recycled.
#'
#'@param ... other arguments to pass to \code{\link[ggplot2]{geom_text}} or
#'  \code{\link[ggplot2]{geom_label}}.
#'@param na.rm If FALSE (the default), removes missing values with a warning. If
#'  TRUE silently removes missing values.
#'@seealso \code{\link{compare_means}}
#' @examples
#' # Load data
#' data("ToothGrowth")
#' head(ToothGrowth)
#'
#' # Two independent groups
#' #:::::::::::::::::::::::::::::::::::::::::::::::::
#' p <- ggboxplot(ToothGrowth, x = "supp", y = "len",
#'   color = "supp", palette = "npg", add = "jitter")
#'
#' #  Add p-value
#' p + stat_compare_means()
#' # Change method
#' p + stat_compare_means(method = "t.test")
#'
#'  # Paired samples
#'  #:::::::::::::::::::::::::::::::::::::::::::::::::
#'  ggpaired(ToothGrowth, x = "supp", y = "len",
#'    color = "supp", line.color = "gray", line.size = 0.4,
#'    palette = "npg")+
#'  stat_compare_means(paired = TRUE)
#'
#' # More than two groups
#' #:::::::::::::::::::::::::::::::::::::::::::::::::
#' # Pairwise comparisons: Specify the comparisons you want
#' my_comparisons <- list( c("0.5", "1"), c("1", "2"), c("0.5", "2") )
#' ggboxplot(ToothGrowth, x = "dose", y = "len",
#'           color = "dose", palette = "npg")+
#' # Add pairwise comparisons p-value
#' stat_compare_means(comparisons = my_comparisons, label.y = c(29, 35, 40))+
#' stat_compare_means(label.y = 45)     # Add global Anova p-value
#'
#' # Multiple pairwise test against a reference group
#' ggboxplot(ToothGrowth, x = "dose", y = "len",
#'     color = "dose", palette = "npg")+
#' stat_compare_means(method = "anova", label.y = 40)+ # Add global p-value
#' stat_compare_means(aes(label = ..p.signif..),
#'                   method = "t.test", ref.group = "0.5")
#'
#' # Multiple grouping variables
#' #:::::::::::::::::::::::::::::::::::::::::::::::::
#' # Box plot facetted by "dose"
#'p <- ggboxplot(ToothGrowth, x = "supp", y = "len",
#'               color = "supp", palette = "npg",
#'               add = "jitter",
#'               facet.by = "dose", short.panel.labs = FALSE)
#'# Use only p.format as label. Remove method name.
#'p + stat_compare_means(
#'  aes(label = paste0("p = ", ..p.format..))
#')
#'
#'@export
stat_compare_means <- function(mapping = NULL, data = NULL,
                     method = NULL, paired = FALSE, ref.group = NULL,
                     comparisons = NULL, hide.ns = FALSE, label.sep = ", ",
                     label = NULL, label.x.npc = "left", label.y.npc = "top",
                     label.x = NULL, label.y = NULL,
                     geom = "text", position = "identity",  na.rm = FALSE, show.legend = NA,
                    inherit.aes = TRUE, ...) {

  if(!is.null(comparisons)){

    method.info <- .method_info(method)
    method <- method.info$method

    test.args <- list(paired = paired)
    if(method == "wilcox.test")
      test.args$exact <- FALSE

    pms <- list(...)
    size <- ifelse(is.null(pms$size), 0.3, pms$size)
    color <- ifelse(is.null(pms$color), "black", pms$color)

    map_signif_level <- FALSE

    if(.is_p.signif_in_mapping(mapping) | !.is_empty(label %in% "p.signif"))
      {
      map_signif_level <- c("****"=0.0001, "***"=0.001, "**"=0.01,  "*"=0.05, " "=1)
      if(hide.ns) map_signif_level[5] <- c(" "=1)
    }

    step_increase <- ifelse(is.null(label.y), 0.12, 0)
    ggsignif::geom_signif(comparisons = comparisons, y_position = label.y,
                          test = method, test.args = test.args,
                          step_increase = step_increase, size = size, color = color,
                          map_signif_level = map_signif_level)
  }

  else{
    mapping <- .update_mapping(mapping, label)
    layer(
      stat = StatCompareMeans, data = data, mapping = mapping, geom = geom,
      position = position, show.legend = show.legend, inherit.aes = inherit.aes,
      params = list(label.x.npc  = label.x.npc , label.y.npc  = label.y.npc,
                    label.x = label.x, label.y = label.y, label.sep = label.sep,
                    method = method, paired = paired, ref.group = ref.group,
                    hide.ns = hide.ns, na.rm = na.rm, ...)
    )

  }

}


StatCompareMeans<- ggproto("StatCompareMeans", Stat,
                  required_aes = c("x", "y"),
                  default_aes = aes(hjust = ..hjust.., vjust = ..vjust..),

                  compute_panel = function(data, scales, method, paired, ref.group,
                                           hide.ns, label.x.npc, label.y.npc,
                                           label.x, label.y, label.sep)
                    {
                    . <- x <- NULL
                    .is.multiple.grouping.vars <- !all(data$x == data$group)

                    if(!is.null(ref.group)) {
                      if(ref.group != ".all.") ref.group <- scales$x$map(ref.group)
                    }

                    # Guess the number of group to be compared
                    #::::::::::::::::::::::::::::::::::::::::::::::::::
                    if(.is.multiple.grouping.vars)
                      x.levels <- .levels(data$group)
                    else x.levels <- .levels(data$x)
                    two.groups <- length(x.levels) == 2 | !is.null(ref.group)
                    multi.groups <- length(x.levels) > 2

                    # Guess the test to be performed
                    #::::::::::::::::::::::::::::::::::::::::::::::::::
                    if(two.groups & is.null(method))
                      method <- "wilcox.test"
                    else if(multi.groups & is.null(method))
                      method <- "kruskal.test"

                    # Perform group comparisons
                    #::::::::::::::::::::::::::::::::::::::::::::::::::
                    if(.is.multiple.grouping.vars){
                      .test <- compare_means(y~group, data = data, method = method, group.by = "x",
                                             paired = paired, ref.group = ref.group)

                    }
                    else{
                      .test <- compare_means(y~x, data = data, method = method,
                                             paired = paired, ref.group = ref.group)
                    }

                    pvaltxt <- ifelse(.test$p < 2.2e-16, "p < 2.2e-16",
                                      paste("p =", signif(.test$p, 2)))
                    .test$label <- paste(.test$method, pvaltxt, sep =  label.sep)

                    # Options for label positioning
                    #::::::::::::::::::::::::::::::::::::::::::::::::::
                    label.opts <- list(data = data, scales = scales,
                                       label.x.npc = label.x.npc, label.y.npc = label.y.npc,
                                       label.x = label.x, label.y = label.y, .by = "panel" )

                    if(.is.multiple.grouping.vars){

                      if(is.null(label.x) & length(label.x.npc) == 1)
                        label.opts$label.x <- .test$x

                      .label.pms <- label.opts %>%
                        .add_item(group.ids = .test$x) %>%
                        do.call(.label_params_by_group, .) # Returns a data frame with label: x, y, hjust, vjust
                      # .test <- dplyr::select(.test, -x)
                      .label.pms <- dplyr::select(.label.pms, -x)

                    }

                    else{
                      .label.pms <- label.opts %>%
                        do.call(.label_params, .) %>% # Returns a data frame with label: x, y, hjust, vjust
                        dplyr::mutate(hjust = 0.2)
                    }
                    if(!is.null(ref.group)){
                      group.ids <- as.numeric(.test$group2)
                      if(!is.null(label.y) & ref.group != ".all."){
                        if(length(label.y) == length(group.ids))
                          label.opts$label.y <- c(0, label.y)
                      }
                      .label.pms <- label.opts %>%
                        .add_item(group.ids = group.ids) %>%
                        do.call(.label_params_by_group, .)
                    }

                    res <- cbind(.test, .label.pms)

                    if(!is.null(ref.group)){
                      # Set label x value to group names
                      other.group.index <- as.numeric(res$group2)
                      res$x <- scales$x$range$range[other.group.index ]
                      res <- res %>% dplyr::mutate(hjust = 0.5)
                    }

                    if(hide.ns){
                      p.signif <- res$p.signif
                      p.signif[p.signif == "ns"] <- " "
                      res$p.signif <- p.signif
                    }
                    res
                  }

)


# Check if p.signif is in mapping
.is_p.signif_in_mapping <- function(mapping){

  res <- FALSE
  if(!is.null(mapping)){
    if(!is.null(mapping$label)){
      .label <- as.character(mapping$label)
      res <- "..p.signif.." %in% .label
    }
  }
  return(res)
}

# Update mapping with label
.update_mapping <- function (mapping, label){

  allowed.label <- list(
    "p.signif" = quote(..p.signif..),
    "..p.signif.." = quote(..p.signif..),
    "p.format" = quote(paste0("p = ",..p.format..)),
    "..p.format.." = quote(paste0("p = ",..p.format..)),
    "p" = quote(paste0("p = ",..p.format..)),
    "..p.." = quote(paste0("p = ",..p.format..))
  )

  if(!is.null(label)){
    if(!label %in% names(allowed.label) )
      stop("Allowed values for label are: ", .collapse(names(allowed.label) , sep = ", "))
  }

  if(!is.null(mapping) & is.character(label)){
    mapping$label <- allowed.label[[label]]
  }
  else if(is.character(label)){
    mapping <- aes()
    mapping$label <- allowed.label[[label]]
  }
  mapping
}




