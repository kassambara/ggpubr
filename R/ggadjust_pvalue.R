#' @include utilities.R geom_pwc.R p_format_utils.R
NULL

#' Adjust p-values Displayed on a GGPlot
#' @description Adjust p-values produced by \code{\link{geom_pwc}()} on a ggplot.
#'  This is mainly useful when using facet, where p-values are generally
#'  computed and adjusted by panel without taking into account the other panels.
#'  In this case, one might want to adjust after the p-values of all panels together.
#' @inheritParams geom_pwc
#' @param p a ggplot
#' @param layer An integer indicating the statistical layer rank in the ggplot
#'  (in the order added to the plot).
#' @param output character. Possible values are one of \code{c("plot",
#'  "stat_test")}. Default is "plot".
#' @examples
#' # Data preparation
#' # :::::::::::::::::::::::::::::::::::::::
#' df <- ToothGrowth
#' df$dose <- as.factor(df$dose)
#' # Add a random grouping variable
#' df$group <- factor(rep(c("grp1", "grp2"), 30))
#' head(df, 3)
#'
#' # Boxplot: Two groups by panel
#' # :::::::::::::::::::::::::::::::::::::::
#' # Create a box plot
#' bxp <- ggboxplot(
#'   df,
#'   x = "supp", y = "len", fill = "#00AFBB",
#'   facet.by = "dose"
#' )
#' # Make facet and add p-values
#' bxp <- bxp + geom_pwc(method = "t_test")
#' bxp
#' # Adjust all p-values together after
#' ggadjust_pvalue(
#'   bxp,
#'   p.adjust.method = "bonferroni",
#'   label = "{p.adj.format}{p.adj.signif}", hide.ns = TRUE
#' )
#'
#'
#' # Boxplot: Three groups by panel
#' # :::::::::::::::::::::::::::::::::::::::
#' # Create a box plot
#' bxp <- ggboxplot(
#'   df,
#'   x = "dose", y = "len", fill = "#00AFBB",
#'   facet.by = "supp"
#' )
#' # Make facet and add p-values
#' bxp <- bxp + geom_pwc(method = "t_test")
#' bxp
#' # Adjust all p-values together after
#' ggadjust_pvalue(
#'   bxp,
#'   p.adjust.method = "bonferroni",
#'   label = "{p.adj.format}{p.adj.signif}"
#' )
#' @param signif.cutoffs numeric vector of p-value cutoffs in descending order
#'  for assigning significance symbols. For example, \code{c(0.10, 0.05, 0.01)}
#'  means p < 0.10 gets "*", p < 0.05 gets "**", p < 0.01 gets "***".
#'  Default is NULL, which uses the package defaults.
#' @param signif.symbols character vector of symbols corresponding to
#'  \code{signif.cutoffs}. If NULL, auto-generated as "*", "**", "***"
#'  (and "****" if \code{use.four.stars = TRUE}).
#' @param ns.symbol character string for non-significant results. Default is "ns".
#'  Use "" (empty string) to show nothing.
#' @param use.four.stars logical. If TRUE, allows four stars (****) for the most
#'  significant level. Default is FALSE.
#' @export
ggadjust_pvalue <- function(p, layer = NULL, p.adjust.method = "holm", label = "p.adj",
                            hide.ns = NULL, symnum.args = list(),
                            p.format.style = "default", p.digits = NULL,
                            p.leading.zero = NULL, p.min.threshold = NULL,
                            p.decimal.mark = NULL,
                            signif.cutoffs = NULL, signif.symbols = NULL,
                            ns.symbol = "ns", use.four.stars = FALSE,
                            output = c("plot", "stat_test")) {
  # Build symnum.args from new parameters
  symnum.args <- build_symnum_args(
    signif.cutoffs = signif.cutoffs,
    signif.symbols = signif.symbols,
    ns.symbol = ns.symbol,
    use.four.stars = use.four.stars,
    symnum.args = symnum.args
  )

  output <- match.arg(output)
  .build <- ggplot_build(p)
  .build_data <- .build$data

  # Pairwise comparison--------------------------
  # Find layer containing statistical test data
  key_columns <- c("group1", "group2", "p")
  if (is.null(layer)) {
    for (i in seq_along(.build_data)) {
      if (all(key_columns %in% colnames(.build_data[[i]]))) {
        layer <- i
        break
      }
    }
  }
  if (is.null(layer)) {
    stop("Can't find any layer containing statiscal tests")
  }

  stat_test <- .build$data[[layer]]
  sy <- symnum.args
  if (all(is.na(stat_test$p))) {
    warning(
      "p-values can't be adjusted for the specified stat method.\n",
      "The result of the method doesn't contain the p column.\n",
      "Note that, tests such as tukey_hsd or games_howell_test handle p-value adjustement ",
      "internally; they only return the p.adj.",
      call. = FALSE
    )
    label <- gsub(pattern = "p\\.format(?!\\.signif)", replacement = "p.adj.format", label, perl = TRUE)
    label <- gsub(pattern = "p\\.signif", replacement = "p.adj.signif", label)
  } else {
    padjusted <- stat_test %>%
      dplyr::select(dplyr::all_of(c("PANEL", "group", "group1", "group2", "p"))) %>%
      dplyr::distinct(.keep_all = TRUE) %>%
      rstatix::adjust_pvalue(method = p.adjust.method)
    # Hide NS if hide.ns not null
    if (!is.null(hide.ns)) {
      padjusted <- rstatix::remove_ns(padjusted, col = hide.ns)
    }

    p <- p.adj <- NULL
    stat_test <- stat_test %>%
      dplyr::select(-dplyr::any_of(c("p", "p.adj", "p.format", "p.adj.format", "label"))) %>%
      dplyr::inner_join(padjusted, by = c("PANEL", "group", "group1", "group2"))

    # Format p-values using the specified style
    if (p.format.style == "default") {
      stat_test <- stat_test %>%
        rstatix::p_format(p, p.adj, new.col = TRUE, accuracy = 1e-4)
    } else {
      stat_test <- stat_test %>%
        dplyr::mutate(
          p.format = format_p_value(p,
            style = p.format.style,
            digits = p.digits, leading.zero = p.leading.zero,
            min.threshold = p.min.threshold,
            decimal.mark = p.decimal.mark
          ),
          p.adj.format = format_p_value(p.adj,
            style = p.format.style,
            digits = p.digits, leading.zero = p.leading.zero,
            min.threshold = p.min.threshold,
            decimal.mark = p.decimal.mark
          )
        )
    }

    stat_test <- stat_test %>%
      rstatix::add_significance(p.col = "p", cutpoints = sy$cutpoints, symbols = sy$symbols) %>%
      rstatix::add_significance(p.col = "p.adj", cutpoints = sy$cutpoints, symbols = sy$symbols) %>%
      add_stat_label(label = label)
    .build$data[[layer]] <- stat_test
  }
  if (output == "stat_test") {
    return(stat_test)
  }
  as_ggplot(ggplot_gtable(.build))
}
