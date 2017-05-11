#' Descriptive statistics by groups
#'
#' @description Computes descriptive statistics by groups for a measure variable.
#'
#' @param data a data frame.
#' @param measure.var the name of a column containing the variable to be summarized.
#' @param grps a character vector containing grouping variables; e.g.: grps = c("grp1", "grp2")
#' @param ci the percent range of the confidence interval (default is 0.95).
#'
#' @return
#' A data frame containing descriptive statistics, such as:
#' \itemize{
#' \item \strong{length}: the number of elements in each group
#' \item \strong{min}: minimum
#' \item \strong{max}: maximum
#' \item \strong{median}: median
#' \item \strong{mean}: mean
#' \item \strong{iqr}: interquartile range
#' \item \strong{mad}: median absolute deviation (see ?MAD)
#' \item \strong{sd}: standard deviation of the mean
#' \item \strong{se}: standard error of the mean
#' \item \strong{ci}: confidence interval of the mean
#' \item \strong{range}: the range = max - min
#' \item \strong{cv}: coefficient of variation, sd/mean
#' \item \strong{var}: variance, sd^2
#' }
#'
#' @examples
#' # Load data
#'data("ToothGrowth")
#'
#'# Descriptive statistics
#'res <- desc_statby(ToothGrowth, measure.var = "len",
#'    grps = c("dose", "supp"))
#' head(res[, 1:10])
#'
#' @export
desc_statby <- function(data, measure.var, grps, ci = 0.95){
  if(!inherits(data, "data.frame"))
    stop("data must be a data.frame.")

  . <- NULL
  data %>% as.data.frame() %>%
    group_by_(.dots = grps) %>%
    do(.summary(.[, measure.var], ci = ci)) %>%
    as.data.frame()
}

# Helper function to compute summary statistics
.summary <- function(x, ci = 0.95){

  if(is.data.frame(x)){
    if(ncol(x) == 1) x <- .select_vec(x, 1)
  }
  if(!is.numeric(x))
    stop("x should be a numeric vector or a data frame with one numeric column")

  data_sum <- data.frame(
    length = base::sum(!is.na(x)),
    min = base::min(x, na.rm=TRUE),
    max = base::max(x, na.rm=TRUE),
    median = stats::median(x, na.rm=TRUE),
    mean = base::mean(x, na.rm=TRUE),
    iqr = stats::IQR(x, na.rm=TRUE),
    mad = stats::mad(x, na.rm=TRUE),
    sd = stats::sd(x, na.rm=TRUE)
  )

  data_sum$se <- data_sum$sd / sqrt(data_sum$length) # standard error
  # Confidence interval from t-distribution
  # e.g., if conf.interval is .95, use .975 (above/below), and use df=N-1
  data_sum$ci <- stats::qt(ci/2 + .5, data_sum$length-1)*data_sum$se
  data_sum$range <- data_sum$max - data_sum$min
  data_sum$cv <- data_sum$sd/data_sum$mean
  data_sum$var <- data_sum$sd^2
  return(data_sum)
}



