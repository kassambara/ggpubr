#' @include utilities.R p_format_utils.R
NULL
#' Comparison of Means
#' @description Performs one or multiple mean comparisons.
#' @param formula a formula of the form \code{x ~ group} where \code{x} is a
#'  numeric variable giving the data values and \code{group} is a factor with
#'  one or multiple levels giving the corresponding groups. For example,
#'  \code{formula = TP53 ~ cancer_group}.
#'
#'  It's also possible to perform the test for multiple response variables at
#'  the same time. For example, \code{formula = c(TP53, PTEN) ~ cancer_group}.
#' @param data a data.frame containing the variables in the formula.
#' @param method the type of test. Default is \link[stats]{wilcox.test}. Allowed
#'  values include: \itemize{ \item \code{\link[stats]{t.test}} (parametric) and
#'  \code{\link[stats]{wilcox.test}} (non-parametric). Perform comparison
#'  between two groups of samples. If the grouping variable contains more than
#'  two levels, then a pairwise comparison is performed. \item
#'  \code{\link[stats]{anova}} (parametric) and
#'  \code{\link[stats]{kruskal.test}} (non-parametric). Perform one-way ANOVA
#'  test comparing multiple groups. }
#' @param paired a logical indicating whether you want a paired test. Used only
#'  in \code{\link[stats]{t.test}} and in \link[stats]{wilcox.test}.
#' @param group.by  a character vector containing the name of grouping variables.
#' @param ref.group a character string specifying the reference group. If
#'  specified, for a given grouping variable, each of the group levels will be
#'  compared to the reference group (i.e. control group).
#'
#'  \code{ref.group} can be also \code{".all."}. In this case, each of the
#'  grouping variable levels is compared to all (i.e. basemean).
#' @param symnum.args a list of arguments to pass to the function
#'  \code{\link[stats]{symnum}} for symbolic number coding of p-values. For
#'  example, \code{symnum.args <- list(cutpoints = c(0, 0.0001, 0.001,
#'  0.01, 0.05, Inf), symbols = c("****", "***", "**", "*",  "ns"))}.
#'
#'  In other words, we use the following convention for symbols indicating
#'  statistical significance: \itemize{ \item \code{ns}: p > 0.05 \item
#'  \code{*}: p <= 0.05 \item \code{**}: p <= 0.01 \item \code{***}: p <= 0.001 \item \code{****}:  p <= 0.0001 }
#'
#'  Note: If \code{signif.cutoffs} is provided, it takes precedence over
#'  \code{symnum.args}.
#' @param signif.cutoffs numeric vector of p-value cutoffs in descending order
#'  for assigning significance symbols. For example, \code{c(0.10, 0.05, 0.01)}
#'  means p < 0.10 gets "*", p < 0.05 gets "**", p < 0.01 gets "***".
#'  If \code{use.four.stars = TRUE}, can include a fourth level (e.g.,
#'  \code{c(0.10, 0.05, 0.01, 0.001)} for "****" at p < 0.001).
#'  Default is NULL, which uses the package defaults (backward compatible).
#' @param signif.symbols character vector of symbols corresponding to
#'  \code{signif.cutoffs}. If NULL, auto-generated as "*", "**", "***"
#'  (and "****" if \code{use.four.stars = TRUE}). Must have the same length
#'  as \code{signif.cutoffs}.
#' @param ns.symbol character string for non-significant results. Default is "ns".
#'  Use "" (empty string) to show nothing for non-significant results.
#' @param use.four.stars logical. If TRUE and \code{signif.symbols} is NULL,
#'  allows four stars (****) for the most significant level when
#'  \code{signif.cutoffs} has 4 levels. Default is FALSE.
#' @param p.adjust.method method for adjusting p values (see
#'  \code{\link[stats]{p.adjust}}). Has impact only in a situation, where
#'  multiple pairwise tests are performed; or when there are multiple grouping
#'  variables. Allowed values include "holm", "hochberg", "hommel",
#'  "bonferroni", "BH", "BY", "fdr", "none". If you don't want to adjust the p
#'  value (not recommended), use p.adjust.method = "none".
#'
#'  Note that, when the \code{formula} contains multiple variables, the p-value
#'  adjustment is done independently for each variable.
#' @param p.format.style character string specifying the p-value formatting style.
#'  One of: \code{"default"} (backward compatible, uses scientific notation),
#'  \code{"apa"} (APA style, no leading zero), \code{"nejm"} (NEJM style),
#'  \code{"lancet"} (Lancet style), \code{"ama"} (AMA style), \code{"graphpad"}
#'  (GraphPad style), or \code{"scientific"} (scientific notation for GWAS).
#'  See \code{\link{list_p_format_styles}} for details.
#' @param p.digits integer specifying the number of decimal places for p-values.
#'  If provided, overrides the style default.
#' @param p.leading.zero logical indicating whether to include leading zero before
#'  decimal point (e.g., "0.05" vs ".05"). If provided, overrides the style default.
#' @param p.min.threshold numeric specifying the minimum p-value to display exactly.
#'  Values below this threshold are shown as "< threshold". Set to NULL to always
#'  show exact values. If provided, overrides the style default.
#' @param p.decimal.mark character string to use as the decimal mark. If NULL,
#'  uses \code{getOption("OutDec")}.
#' @return return a data frame with the following columns:
#' \itemize{
#' \item \code{.y.}: the y variable used in the test.
#' \item \code{group1,group2}: the compared groups in the pairwise tests.
#' Available only when \code{method = "t.test"} or \code{method = "wilcox.test"}.
#' \item \code{p}: the p-value.
#' \item \code{p.adj}: the adjusted p-value. Default for \code{p.adjust.method = "holm"}.
#' \item \code{p.format}: the formatted p-value.
#' \item \code{p.format.signif}: the formatted p-value with significance symbols.
#' \item \code{p.signif}: the significance level.
#' \item \code{method}: the statistical test used to compare groups.
#'
#'
#' }
#' @param ... Other arguments to be passed to the test function.
#' @examples
#' # Load data
#' # :::::::::::::::::::::::::::::::::::::::
#' data("ToothGrowth")
#' df <- ToothGrowth
#'
#' # One-sample test
#' # :::::::::::::::::::::::::::::::::::::::::
#' compare_means(len ~ 1, df, mu = 0)
#'
#' # Two-samples unpaired test
#' # :::::::::::::::::::::::::::::::::::::::::
#' compare_means(len ~ supp, df)
#'
#' # Two-samples paired test
#' # :::::::::::::::::::::::::::::::::::::::::
#' compare_means(len ~ supp, df, paired = TRUE)
#'
#' # Compare supp levels after grouping the data by "dose"
#' # ::::::::::::::::::::::::::::::::::::::::
#' compare_means(len ~ supp, df, group.by = "dose")
#'
#' # pairwise comparisons
#' # ::::::::::::::::::::::::::::::::::::::::
#' # As dose contains more thant two levels ==>
#' # pairwise test is automatically performed.
#' compare_means(len ~ dose, df)
#'
#' # Comparison against reference group
#' # ::::::::::::::::::::::::::::::::::::::::
#' compare_means(len ~ dose, df, ref.group = "0.5")
#'
#' # Comparison against all
#' # ::::::::::::::::::::::::::::::::::::::::
#' compare_means(len ~ dose, df, ref.group = ".all.")
#'
#' # Anova and kruskal.test
#' # ::::::::::::::::::::::::::::::::::::::::
#' compare_means(len ~ dose, df, method = "anova")
#' compare_means(len ~ dose, df, method = "kruskal.test")

#' @rdname compare_means
#' @export
compare_means <- function(formula, data, method = "wilcox.test",
                          paired = FALSE,
                          group.by = NULL, ref.group = NULL,
                          symnum.args = list(), p.adjust.method = "holm",
                          p.format.style = "default", p.digits = NULL,
                          p.leading.zero = NULL, p.min.threshold = NULL,
                          p.decimal.mark = NULL,
                          signif.cutoffs = NULL, signif.symbols = NULL,
                          ns.symbol = "ns", use.four.stars = FALSE, ...) {
  . <- NULL

  method.info <- .method_info(method)
  method <- method.info$method
  method.name <- method.info$name

  # Build symnum.args from new parameters or use defaults
  symnum.args <- build_symnum_args(
    signif.cutoffs = signif.cutoffs,
    signif.symbols = signif.symbols,
    ns.symbol = ns.symbol,
    use.four.stars = use.four.stars,
    symnum.args = symnum.args
  )

  if (!inherits(data, "data.frame")) {
    stop("data must be a data.frame.")
  }

  variables <- response.var <- .formula_left_variables(formula)
  group <- .formula_right_variables(formula)
  if (group == "1") group <- NULL # NULL model

  if (!.is_empty(group)) {
    group.vals <- .select_vec(data, group)
    if (!is.factor(group.vals)) data[[group]] <- factor(group.vals, levels = unique(group.vals))
  }

  # Keep only variables of interest
  data <- data %>%
    df_select(vars = c(group.by, group, variables))

  # Case of formula with multiple variables
  #   1. Gather the data
  #   2. group by variable
  #   3. Perform pairwise test between levels of each grouing variable
  # :::::::::::::::::::::::::::::::::::::::::::::::::::::
  # ex: formula = c(GATA3, XBP1, DEPDC1) ~ group
  if (.is_multi_formula(formula)) {
    data <- df_gather(data, cols = variables, names_to = ".y.", values_to = ".value.") %>%
      dplyr::mutate(.y. = factor(.data$.y., levels = unique(.data$.y.)))
    response.var <- ".value."
    group.by <- c(group.by, ".y.")
    formula <- .collapse(response.var, group, sep = " ~ ") %>% stats::as.formula()
  }

  # Check if comparisons should be done against a reference group
  # :::::::::::::::::::::::::::::::::::::::::::::::::::::
  if (!is.null(ref.group)) {
    group.vals <- .select_vec(data, group)
    if (is.factor(group.vals)) {
      group.levs <- levels(group.vals)
    } else {
      group.levs <- unique(group.vals)
    }

    if (ref.group %in% group.levs) {
      data[[group]] <- stats::relevel(group.vals, ref.group)
    }

    if (ref.group == ".all.") {
      data <- data %>%
        mutate(
          .group. = as.character(group.vals),
          .all. = ".all."
        ) # Add 'all' column
      # Create a new grouping column gathering group and the .all. columns
      .group.name. <- NULL
      data <- data %>%
        df_gather(cols = c(".group.", ".all."), names_to = ".group.name.", values_to = ".group.") %>%
        dplyr::select(-.group.name.)
      data[[".group."]] <- factor(data[[".group."]], levels = c(".all.", group.levs))
      group <- ".group."
      formula <- .collapse(response.var, group, sep = " ~ ") %>% stats::as.formula()
    } else if (!(ref.group %in% group.levs)) {
      stop("Can't find specified reference group: ", ref.group, ". ",
        "Allowed values include one of: ", .collapse(group.levs, sep = ", "),
        call. = FALSE
      )
    }
  }

  # Peform the test
  # :::::::::::::::::::::::::::::::::::::::::::::::::::::
  test.func <- .test_pairwise
  if (method %in% c("anova", "kruskal.test")) {
    test.func <- .test_multigroups
  }

  if (is.null(group.by)) {
    res <- test.func(
      formula = formula, data = data, method = method,
      paired = paired, p.adjust.method = "none", ...
    )
  } else {
    grouped.d <- .group_by(data, group.by)
    res <- grouped.d %>%
      mutate(p = purrr::map(
        data,
        test.func,
        formula = formula,
        method = method, paired = paired, p.adjust.method = "none", ...
      )) %>%
      df_select(vars = c(group.by, "p")) %>%
      unnest(cols = "p")
  }

  # Add response variables to the result
  # :::::::::::::::::::::::::::::::::::::::::::::::::::::
  if (!c(".y." %in% colnames(res))) {
    res <- res %>%
      dplyr::mutate(.y. = variables) %>%
      dplyr::select(!!!syms(c(group.by, ".y.")), dplyr::everything())
  }

  # Select only reference groups if any
  # Skip for anova/kruskal.test which don't have group1/group2 columns (Issue #572)
  # ::::::::::::::::::::::::::::::::::::::::::::::::::::::::
  if (!is.null(ref.group) && !method %in% c("anova", "kruskal.test")) {
    group.levs <- .select_vec(data, group) %>% .levels()
    group1 <- NULL
    res <- res %>% dplyr::filter(group1 == ref.group | group2 == ref.group)
    # ref.group should be always in group1 column
    # swap group1 and group2 if group2 contains ref.group
    group2 <- res$group2
    res <- transform(res,
      group1 = ifelse(group2 == ref.group, group2, group1),
      group2 = ifelse(group2 == ref.group, group1, group2)
    )
  }
  # Formatting and adjusting pvalues, and adding significance symbols
  # :::::::::::::::::::::::::::::::::::::::::::::::::::::
  symnum.args$x <- res$p
  pvalue.signif <- do.call(stats::symnum, symnum.args) %>%
    as.character()

  pvalue.format <- format_p_value(res$p,
    style = p.format.style,
    digits = p.digits,
    leading.zero = p.leading.zero,
    min.threshold = p.min.threshold,
    decimal.mark = p.decimal.mark
  )

  .y. <- p.adj <- p <- NULL
  pvalue.adj <- res %>%
    group_by(.y.) %>%
    reframe(p.adj = stats::p.adjust(p, method = p.adjust.method))
  res <- res %>%
    dplyr::ungroup() %>%
    mutate(
      p.adj = pvalue.adj$p.adj, p.format = pvalue.format, p.signif = pvalue.signif,
      method = method.name
    )

  # Resolve p.digits for adjusted p-value rounding
  p_params <- resolve_p_format_params(
    p.format.style, p.digits, p.leading.zero, p.min.threshold, p.decimal.mark
  )
  res %>%
    mutate(p.adj = signif(p.adj, digits = p_params$digits)) %>%
    add_p_format_signif() %>%
    tibble::as_tibble()
}


# Check and get test method info
# :::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::
# return a list(method, name)
.method_info <- function(method) {
  if (is.null(method)) {
    method <- "wilcox.test"
  }

  allowed.methods <- list(
    t = "t.test", t.test = "t.test", student = "t.test",
    wiloxon = "wilcox.test", wilcox = "wilcox.test", wilcox.test = "wilcox.test",
    anova = "anova", aov = "anova",
    kruskal = "kruskal.test", kruskal.test = "kruskal.test"
  )

  method.names <- list(
    t.test = "T-test", wilcox.test = "Wilcoxon",
    anova = "Anova", kruskal.test = "Kruskal-Wallis"
  )

  if (!(method %in% names(allowed.methods))) {
    stop(
      "Non-supported method specified. Allowed methods are one of: ",
      .collapse(allowed.methods, sep = ", ")
    )
  }
  method <- allowed.methods[[method]]
  method.name <- method.names[[method]]

  list(method = method, name = method.name)
}

# Comparing two groups
# :::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::
.test <- function(data, formula, method = "t.test", ...) {
  test <- match.fun(method)

  x <- deparse(formula[[2]])
  group <- attr(stats::terms(formula), "term.labels")

  if (.is_empty(group)) { # Case of null model
    test.opts <- list(x = .select_vec(data, x), ...)
  } else {
    test.opts <- list(formula = formula, data = data, ...)
  }

  res <- data.frame(p = suppressWarnings(do.call(test, test.opts)$p.value))
  group1 <- group2 <- NULL

  if (!.is_empty(group)) {
    group.lev <- .select_vec(data, group) %>% levels()
    res <- res %>%
      dplyr::mutate(
        group1 = group.lev[1],
        group2 = group.lev[2]
      ) %>%
      dplyr::select(group1, group2, dplyr::everything())
  } else {
    res <- res %>%
      dplyr::mutate(group1 = 1, group2 = "null model") %>%
      dplyr::select(group1, group2, everything())
  }
  res
}


# pairwise test
# :::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::
.test_pairwise <- function(data, formula, method = "wilcox.test",
                           paired = FALSE, pool.sd = !paired,
                           ...) {
  x <- deparse(formula[[2]])
  group <- attr(stats::terms(formula), "term.labels")

  # One sample test
  if (.is_empty(group)) {
    res <- .test(data, formula, method = method, ...)
    return(res)
  }

  # Pairwise test
  method <- switch(method,
    t.test = "pairwise.t.test",
    wilcox.test = "pairwise.wilcox.test"
  )
  test <- match.fun(method)

  test.opts <- list(
    x = .select_vec(data, x),
    g = .select_vec(data, group),
    paired = paired,
    ...
  )
  if (method == "pairwise.t.test") {
    if (missing(pool.sd)) {
      if (!paired) pool.sd <- FALSE
    }
    test.opts$pool.sd <- pool.sd
  }

  pvalues <- suppressWarnings(do.call(test, test.opts)$p.value) %>%
    as.data.frame()
  p <- NULL
  pvalues$..group2.. <- rownames(pvalues)
  pvalues <- pvalues %>%
    tidyr::pivot_longer(
      cols = -"..group2..",
      names_to = "..group1..",
      values_to = "p"
    ) %>%
    dplyr::select(group1 = "..group1..", group2 = "..group2..", p) %>%
    dplyr::filter(!is.na(p))
  pvalues
}

# Compare multiple groups
# ::::::::::::::::::::::::::::::::::::::::::::::::::

.test_multigroups <- function(data, formula, method = c("anova", "kruskal.test"), ...) {
  method <- match.arg(method)
  . <- NULL

  if (method == "anova") {
    pvalue <- stats::lm(formula, data = data) %>%
      stats::anova(.) %>%
      .$`Pr(>F)` %>%
      .[1]
  } else if (method == "kruskal.test") {
    pvalue <- stats::kruskal.test(formula, data = data)$p.value
  }

  data.frame(p = pvalue)
}


# Formula with multiple response variables
# :::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::
# ex formula = c(GATA3, XBP1, DEPDC1) ~ group
.is_multi_formula <- function(formula) {
  x <- grep(",", formula)
  !.is_empty(x)
}

# Get formula variables
.formula_left_variables <- function(formula) {
  . <- NULL
  x <- deparse(formula[[2]]) %>%
    gsub("c\\(|\\)|\\s", "", .) %>%
    strsplit(",") %>%
    unlist()
  x
}
.formula_right_variables <- function(formula) {
  group <- attr(stats::terms(formula), "term.labels")
  if (.is_empty(group)) group <- "1"
  group
}

.update_test_arguments <- function(formula, data, group.by) {
  variables <- .formula_left_variables(formula)
  group <- .formula_right_variables(formula)

  data <- data %>%
    df_gather(cols = variables, names_to = ".y.", values_to = ".value.") %>%
    dplyr::mutate(.y. = factor(.data$.y., levels = unique(.data$.y.)))
  formula <- .collapse(".value.", group, sep = " ~ ") %>% stats::as.formula()
  group.by <- c(group.by, ".y.")
}
