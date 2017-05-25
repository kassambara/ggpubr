#' @include utilities.R
NULL
#'Comparison of Means
#'@description Performs one or multiple mean comparisons.
#'@param formula a formula of the form \code{x ~ group} where \code{x} is a numeric variable
#'  giving the data values and \code{group} is a factor with one or multiple levels giving the
#'  corresponding groups.
#'@param data a data.frame containing the variables in the formula.
#'@param method the type of test
#' @param group.by  a character vector containing the name of grouping
#'   variables.
#' @param ... Other arguments to be passed to the test function.
#' @examples
#' # Load data
#' #::::::::::::::::::::::::::::
#' data("ToothGrowth")
#' df <- ToothGrowth
#'
#' # Compare supp levels after grouping the data by "dose"
#' #:::::::::::::::::::::::::::::::::::
#' mc_test(len ~ supp, df, group.by = "dose")
#'
#'@rdname mean_comparison
#'@export
mc_test <- function(formula, data, method = "t.test", group.by = NULL, ...)
{
  if(!inherits(data, "data.frame"))
    stop("data must be a data.frame.")

  if(is.null(group.by)){
    res <- .test(formula = formula, data = data, method = method, ...)
  }
  else{
    grouped.d <- .group_by(data, group.by)
    data <- grouped.d$data
    pvalues <- purrr::map(data, .test, formula = formula, method = method, ...)
    res <- grouped.d %>% mutate(pvalue = pvalues)
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
  if(!inherits(data, "data.frame"))
    stop("data must be a data.frame.")

  test <- match.fun(method)

  x <- deparse(formula[[2]])
  group <- attr(stats::terms(formula), "term.labels")

  if(.is_empty(group))
    test.opts <- list(x = .select_vec(data, x), ...)
  else test.opts <- list(formula = formula, data = data, ...)
  if(method == "wilcox.test") test.opts$exact <- FALSE

  # Case of null model
  if(.is_empty(group)){
    res <- do.call(test, test.opts)
    return(res$p.value)
  }

  do.call(test, test.opts)$p.value
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


