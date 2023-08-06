#' @include utilities.R geom_pwc.R
NULL

#'Adjust p-values Displayed on a GGPlot
#'@description Adjust p-values produced by \code{\link{geom_pwc}()} on a ggplot.
#'  This is mainly useful when using facet, where p-values are generally
#'  computed and adjusted by panel without taking into account the other panels.
#'  In this case, one might want to adjust after the p-values of all panels together.
#'@inheritParams geom_pwc
#'@param p a ggplot
#'@param layer An integer indicating the statistical layer rank in the ggplot
#'  (in the order added to the plot).
#'@param output character. Possible values are one of \code{c("plot",
#'  "stat_test")}. Default is "plot".
#'@examples
#' # Data preparation
#' #:::::::::::::::::::::::::::::::::::::::
#' df <- ToothGrowth
#' df$dose <- as.factor(df$dose)
#' # Add a random grouping variable
#' df$group <- factor(rep(c("grp1", "grp2"), 30))
#' head(df, 3)
#'
#' # Boxplot: Two groups by panel
#' #:::::::::::::::::::::::::::::::::::::::
#' # Create a box plot
#' bxp <- ggboxplot(
#'   df, x = "supp", y = "len", fill = "#00AFBB",
#'   facet.by = "dose"
#' )
#' # Make facet and add p-values
#' bxp <- bxp + geom_pwc(method = "t_test")
#' bxp
#' # Adjust all p-values together after
#' ggadjust_pvalue(
#'   bxp, p.adjust.method = "bonferroni",
#'   label = "{p.adj.format}{p.adj.signif}", hide.ns = TRUE
#' )
#'
#'
#' # Boxplot: Three groups by panel
#' #:::::::::::::::::::::::::::::::::::::::
#' # Create a box plot
#' bxp <- ggboxplot(
#'   df, x = "dose", y = "len", fill = "#00AFBB",
#'   facet.by = "supp"
#' )
#' # Make facet and add p-values
#' bxp <- bxp + geom_pwc(method = "t_test")
#' bxp
#' # Adjust all p-values together after
#' ggadjust_pvalue(
#'   bxp, p.adjust.method = "bonferroni",
#'   label = "{p.adj.format}{p.adj.signif}"
#' )
#'@export
ggadjust_pvalue <- function(p, layer = NULL, p.adjust.method = "holm", label = "p.adj",
                            hide.ns = NULL, symnum.args = list(), output = c("plot", "stat_test")){
  output <- match.arg(output)
  .build <- ggplot_build(p)
  .build_data <- .build$data

  # Pairwise comparison--------------------------
  # Find layer containing statistical test data
  key_columns <- c("group1", "group2", "p")
  if(is.null(layer)){
    for(i in 1:length(.build_data)){
      if(all( key_columns %in% colnames(.build_data[[i]]))){
        layer <- i
        break
      }
    }
  }
  if(is.null(layer)){
    stop("Can't find any layer containing statiscal tests")
  }

  stat_test <- .build$data[[layer]]
  sy <- fortify_signif_symbols_encoding(symnum.args)
  if(all(is.na(stat_test$p))){
    warning(
      "p-values can't be adjusted for the specified stat method.\n",
      "The result of the method doesn't contain the p column.\n",
      "Note that, tests such as tukey_hsd or games_howell_test handle p-value adjustement ",
      "internally; they only return the p.adj.",
      call. = FALSE
    )
    label <- gsub(pattern = "p.format", replacement = "p.adj.format", label)
  }
  else{
    padjusted <- stat_test %>%
      dplyr::select(dplyr::all_of(c("PANEL", "group", "group1", "group2", "p"))) %>%
      dplyr::distinct(.keep_all = TRUE) %>%
      rstatix::adjust_pvalue(method = p.adjust.method)
    # Hide NS if hide.ns not null
    if(!is.null(hide.ns))
      padjusted <- rstatix::remove_ns(padjusted, col = hide.ns)

    p <- p.adj <- NULL
    stat_test <- stat_test %>%
      dplyr::select(-dplyr::one_of(c("p", "p.adj", "p.format", "p.adj.format", "label"))) %>%
      dplyr::inner_join(padjusted, by = c("PANEL", "group", "group1", "group2")) %>%
      rstatix::p_format(p, p.adj, new.col = TRUE, accuracy = 1e-4) %>%
      rstatix::add_significance(p.col = "p", cutpoints = sy$cutpoints, symbols = sy$symbols) %>%
      rstatix::add_significance(p.col = "p.adj", cutpoints = sy$cutpoints, symbols = sy$symbols) %>%
      add_stat_label(label = label)
    .build$data[[layer]] <- stat_test
  }
  if(output == "stat_test"){
    return(stat_test)
  }
  as_ggplot(ggplot_gtable(.build))
}
