#' @include utilities.R
NULL
#'Comparison of Means
#'@description Performs one or multiple mean comparisons.
#'@param formula a formula of the form \code{x ~ group} where \code{x} is a
#'  numeric variable giving the data values and \code{group} is a factor with
#'  one or multiple levels giving the corresponding groups. For example,
#'  \code{formula = TP53 ~ cancer_group}.
#'
#'  It's also possible to perform the test for multiple response variables at
#'  the same time. For example, \code{formula = c(TP53, PTEN) ~ cancer_group}.
#'@param data a data.frame containing the variables in the formula.
#'@param method the type of test. Default is \link[stats]{wilcox.test}. Allowed
#'  values include: \itemize{ \item \code{\link[stats]{t.test}} (parametric) and
#'  \code{\link[stats]{wilcox.test}} (non-parametric). Perform comparison
#'  between two groups of samples. If the grouping variable contains more than
#'  two levels, then a pairwise comparison is performed.
#'  \item \code{\link[stats]{anova}} (parametric) and
#'  \code{\link[stats]{kruskal.test}} (non-parametric). Perform one-way ANOVA test comparing multiple groups.
#'  }
#'@param group.by  a character vector containing the name of grouping variables.
#'@param ref.group a character string specifying the reference group. If
#'  specified, for a given grouping variable, each of the group levels will be
#'  compared to the reference group (i.e. control group).
#'
#'  \code{ref.group} can be also \code{".all."}. In this case, each of the
#'  grouping variable levels is compared to all (i.e. basemean).
#'@param symnum.args a list of arguments to pass to the function
#'  \code{\link[stats]{symnum}} for symbolic number coding of p-values. For
#'  example, \code{symnum.args <- list(list(cutpoints = c(0, 0.0001, 0.001,
#'  0.01, 0.05, 0.1, 1), symbols = c("****", "***", "**", "*", "+", "NS")))}.
#' @return return a data frame.
#'@param ... Other arguments to be passed to the test function.
#' @examples
#' # Load data
#' #:::::::::::::::::::::::::::::::::::::::
#' data("ToothGrowth")
#' df <- ToothGrowth
#'
#' # One-sample test
#' #:::::::::::::::::::::::::::::::::::::::::
#' compare_means(len ~ 1, df, mu = 0)
#'
#' # Two-samples unpaired test
#' #:::::::::::::::::::::::::::::::::::::::::
#' compare_means(len ~ supp, df)
#'
#' # Two-samples paired test
#' #:::::::::::::::::::::::::::::::::::::::::
#' compare_means(len ~ supp, df, paired = TRUE)
#'
#' # Compare supp levels after grouping the data by "dose"
#' #::::::::::::::::::::::::::::::::::::::::
#' compare_means(len ~ supp, df, group.by = "dose")
#'
#' # pairwise comparisons
#' #::::::::::::::::::::::::::::::::::::::::
#' # As dose contains more thant two levels ==>
#' # pairwise test is automatically performed.
#' compare_means(len ~ dose, df)
#'
#' # Comparison against reference group
#' #::::::::::::::::::::::::::::::::::::::::
#' compare_means(len ~ dose, df, ref.group = "0.5")
#'
#' # Comparison against all
#' #::::::::::::::::::::::::::::::::::::::::
#' compare_means(len ~ dose, df, ref.group = ".all.")
#'
#' # Anova and kruskal.test
#' #::::::::::::::::::::::::::::::::::::::::
#' compare_means(len ~ dose, df, method = "anova")
#' compare_means(len ~ dose, df, method = "kruskal.test")

#' @rdname compare_means
#' @export
compare_means <- function(formula, data, method = "wilcox.test",
                          group.by = NULL, ref.group = NULL,
                          symnum.args = list(),  ...)
{

  if(.is_empty(symnum.args))
    symnum.args <- list(cutpoints = c(0, 0.0001, 0.001, 0.01, 0.05, 0.1, 1),
                        symbols = c("****", "***", "**", "*", "+", "NS"))

  if(!inherits(data, "data.frame"))
    stop("data must be a data.frame.")

  variables <- response.var <- .formula_left_variables(formula)
  group <- .formula_right_variables(formula)

  if(!.is_empty(group)){
    group.vals <- .select_vec(data, group)
    if(!is.factor(group.vals)) data[, group] <- factor(group.vals, levels = unique(group.vals))
  }

  # Keep only variables of interest
  data <- data %>%
    dplyr::select_(.dots = c(group.by, group, variables))

  # Case of formula with multiple variables
  #   1. Gather the data
  #   2. group by variable
  #   3. Perform pairwise test between levels of each grouing variable
  #:::::::::::::::::::::::::::::::::::::::::::::::::::::
  # ex: formula = c(GATA3, XBP1, DEPDC1) ~ group
  if(.is_multi_formula(formula)){
    data <- tidyr::gather_(data, key_col = ".y.", value_col = ".value.",
                           gather_cols =  variables)
    data$.y. <- factor(data$.y., levels = unique(data$.y.))
    response.var <- ".value."
    group.by = c(group.by,  ".y.")
    formula <- .collapse(response.var, group, sep = " ~ ") %>% stats::as.formula()
  }

  # Check if comparisons should be done against a reference group
  #:::::::::::::::::::::::::::::::::::::::::::::::::::::
  if(!is.null(ref.group)){

    group.vals <- .select_vec(data, group)
    if(is.factor(group.vals)) group.levs <- levels(group.vals)
    else group.levs <- unique(group.vals)


    if(ref.group == ".all."){
      data <- data %>%
        mutate(.group. = as.character(group.vals),
               .all. = ".all.") # Add 'all' column
      # Create a new grouping column gathering group and the .all. columns
      .group.name. <- NULL
      data <- data %>%
        tidyr::gather_(key_col = ".group.name.", value_col = ".group.",
               gather_cols = c(".group.", ".all.")) %>%
        dplyr::select(-.group.name.)
      data$.group. <- factor(data$.group., levels = c(".all.", group.levs))
      group <- ".group."
      formula <- .collapse(response.var, group, sep = " ~ ") %>% stats::as.formula()

    }
    else if(!(ref.group %in% group.levs)){
      stop("Can't find specified reference group: ", ref.group, ". ",
           "Allowed values include one of: ", .collapse(group.levs, sep = ", "), call. = FALSE)
    }
  }

  # Peform the test
  #:::::::::::::::::::::::::::::::::::::::::::::::::::::
  test.func <- .test_pairwise
  if(method %in% c("anova", "kruskal.test"))
    test.func <- .test_multigroups

  if(is.null(group.by)){
    res <- test.func(formula = formula, data = data, method = method,  ...)
  }
  else{
    grouped.d <- .group_by(data, group.by)
    pvalues <- purrr::map(grouped.d$data, test.func, formula = formula, method = method, ...)
    res <- grouped.d %>% mutate(p.value = pvalues) %>%
      dplyr::select_(.dots = c(group.by, "p.value")) %>%
      tidyr::unnest()
  }

  # Add response variables to the result
  #:::::::::::::::::::::::::::::::::::::::::::::::::::::
  if(!c(".y." %in% colnames(res)))
    res <- res %>%
    dplyr::mutate(.y. = variables) %>%
    dplyr::select_(.dots = c(group.by, ".y.", "dplyr::everything()"))

  # Formatting pvalues and adding significance symbols
  #:::::::::::::::::::::::::::::::::::::::::::::::::::::
  symnum.args$x <- res$p.value
  pvalue.signif <- do.call(stats::symnum, symnum.args) %>%
    as.character()
  pvalue.format <- format.pval(res$p.value, digits = 2)
  res <- res %>%
    mutate(p.value.format = pvalue.format, p.value.signif = pvalue.signif,
           method = method)

  # Select only reference groups if any
  #::::::::::::::::::::::::::::::::::::::::::::::::::::::::
  if(!is.null(ref.group)){
    group.levs <- .select_vec(data, group) %>% .levels()
    group1 <- NULL
    res <- res %>% dplyr::filter(group1 == ref.group)
  }

  return(res)
}



.group_by <- function(data, grouping.vars){

  . <- NULL # used in pipes

  # Grouping the data ==> list of data sets
  #::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::
  grouped.d <- dplyr::group_by_(.data = data, .dots = grouping.vars) %>%
    tidyr::nest()

  # Defining names for the list of data sets.
  #::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::
  # names = combination of the levels of the grouping variables
  .names.df <- grouped.d[, grouping.vars, drop = FALSE]
  .names <- .paste_colnames(.names.df, sep = ":") %>%
    apply(1, paste, collapse = ", ")
  names(grouped.d$data) <- .names
  return(grouped.d)
}

# Comparing two groups
#:::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::
.test <- function(data, formula, method = "t.test",  ...)
{
  test <- match.fun(method)

  x <- deparse(formula[[2]])
  group <- attr(stats::terms(formula), "term.labels")

  if(.is_empty(group)) # Case of null model
    test.opts <- list(x = .select_vec(data, x), ...)
  else test.opts <- list(formula = formula, data = data, ...)
  if(method == "wilcox.test") test.opts$exact <- FALSE

  res <- data.frame(p.value = do.call(test, test.opts)$p.value)
  group1 <- group2 <- NULL

  if(!.is_empty(group)){
    group.lev <- .select_vec(data, group) %>% levels()
    res <- res %>%
      dplyr::mutate(
        group1 = group.lev[1],
        group2 = group.lev[2]
      ) %>%
      dplyr::select(group1,group2, dplyr::everything())
  }
  else res <- res %>% dplyr::mutate(group1 = 1, group2 = "null model") %>%
    dplyr::select(group1, group2, everything())
  res
}


# pairwise test
#:::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::
.test_pairwise <- function(data, formula, method = "wilcox.test", ...)
{

  x <- deparse(formula[[2]])
  group <- attr(stats::terms(formula), "term.labels")

  # One sample test
  if(.is_empty(group)){
    res <- .test(data, formula, method = method,  ...)
    return(res)
  }

  # Pairwise test
  method <- switch(method,
                   t.test = "pairwise.t.test",
                   wilcox.test = "pairwise.wilcox.test")
  test <- match.fun(method)

  test.opts <- list(x = .select_vec(data, x),
                    g = .select_vec(data, group),  ...)
  if(method == "pairwise.wilcox.test") test.opts$exact <- FALSE

  pvalues <- do.call(test, test.opts)$p.value %>%
    as.data.frame()
  group1 <- group2 <- p.value <- NULL
  pvalues$group2 <- rownames(pvalues)
  pvalues <- pvalues %>%
    tidyr::gather(key = "group1", value = "p.value", -group2) %>%
    dplyr::select(group1, group2, p.value) %>%
    dplyr::filter(!is.na(p.value))
  pvalues
}

# Compare multiple groups
#::::::::::::::::::::::::::::::::::::::::::::::::::

.test_multigroups <- function(data, formula, method = c("anova", "kruskal.test"), ...){

  method <- match.arg(method)
  . <- NULL

  if(method == "anova")
    pvalue <- stats::lm(formula, data = data) %>%
    stats::anova(.) %>%
    .$`Pr(>F)` %>%
    .[1]
  else if(method == "kruskal.test"){
    pvalue <- stats::kruskal.test(formula, data = data)$p.value
  }

  data.frame(p.value = pvalue)
}


# Pasting the column name to each value of a dataframe
#:::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::
.paste_colnames <- function(data, sep = "."){

  data <- as.data.frame(data)

  if(ncol(data) == 1){

    res <- paste0(colnames(data), ".", data[[1]])
    res <- data.frame(x = res, stringsAsFactors = FALSE)
    colnames(res) <- colnames(data)
    return(res)
  }

  res <- apply(data, 1,
               function(row, cname){paste(cname, row, sep = sep)},
               colnames(data)
  ) %>%
    t() %>%
    as.data.frame(stringsAsFactors = FALSE)
  colnames(res) <- colnames(data)
  res
}


# Formula with multiple response variables
#:::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::
# ex formula = c(GATA3, XBP1, DEPDC1) ~ group
.is_multi_formula <- function(formula){
  x <- grep(",", formula)
  !.is_empty(x)
}

# Get formula variables
.formula_left_variables <- function(formula){
  . <- NULL
  x <- deparse(formula[[2]]) %>%
    gsub("c\\(|\\)|\\s", "", .) %>%
    strsplit(",") %>%
    unlist()
  x
}
.formula_right_variables <- function(formula){
  group <- attr(stats::terms(formula), "term.labels")
  if(.is_empty(group)) group <- "1"
  group
}

.update_test_arguments <- function(formula, data, group.by){

  variables <- .formula_left_variables(formula)
  group <- .formula_right_variables(formula)

  data <- tidyr::gather_(data, key_col = ".y.", value_col = ".value.",
                         gather_cols =  variables)
  data$.y. <- factor(data$.y., levels = unique(data$.y.))
  formula <- .collapse(".value.", group, sep = " ~ ") %>% stats::as.formula()
  group.by = c(group.by, ".y.")

}







