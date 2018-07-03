#' @include utilities.R utilities_label.R
NULL
#' Add Correlation Coefficients with P-values to a Scatter Plot
#' @description Add correlation coefficients with p-values to a scatter plot.
#' @inheritParams ggplot2::layer
#' @param method a character string indicating which correlation coefficient (or
#'   covariance) is to be computed. One of "pearson" (default), "kendall", or
#'   "spearman".
#' @param show.p a logical value (T/F) indicating whether the p-value is 
#'   displayed. Default to T, where the p.value is displayed.
#' @param label.sep a character string to separate the terms. Default is ", ", to
#'   separate the correlation coefficient and the p.value.
#' @param label.x.npc,label.y.npc can be \code{numeric} or \code{character}
#'   vector of the same length as the number of groups and/or panels. If too
#'   short they will be recycled. \itemize{ \item If \code{numeric}, value
#'   should be between 0 and 1. Coordinates to be used for positioning the
#'   label, expressed in "normalized parent coordinates". \item If
#'   \code{character}, allowed values include: i) one of c('right', 'left',
#'   'center', 'centre', 'middle') for x-axis; ii) and one of c( 'bottom',
#'   'top', 'center', 'centre', 'middle') for y-axis.}
#'
#'   If too short they will be recycled.
#' @param label.x,label.y \code{numeric} Coordinates (in data units) to be used
#'   for absolute positioning of the label. If too short they will be recycled.
#'
#' @param ... other arguments to pass to \code{\link[ggplot2]{geom_text}} or
#'   \code{\link[ggplot2]{geom_label}}.
#' @param na.rm If FALSE (the default), removes missing values with a warning.
#'   If TRUE silently removes missing values.
#' @seealso \code{\link{ggscatter}}
#' @examples
#' # Load data
#' data("mtcars")
#' df <- mtcars
#' df$cyl <- as.factor(df$cyl)
#'
#' # Scatter plot with correlation coefficient
#' #:::::::::::::::::::::::::::::::::::::::::::::::::
#' sp <- ggscatter(df, x = "wt", y = "mpg",
#'    add = "reg.line",  # Add regressin line
#'    add.params = list(color = "blue", fill = "lightgray"), # Customize reg. line
#'    conf.int = TRUE # Add confidence interval
#'    )
#' # Add correlation coefficient
#' sp + stat_cor(method = "pearson", label.x = 3, label.y = 30)
#'
#' # Color by groups and facet
#' #::::::::::::::::::::::::::::::::::::::::::::::::::::
#' sp <- ggscatter(df, x = "wt", y = "mpg",
#'    color = "cyl", palette = "jco",
#'    add = "reg.line", conf.int = TRUE)
#' sp + stat_cor(aes(color = cyl), label.x = 3)
#'
#' @export
stat_cor <- function(mapping = NULL, data = NULL,
                     method = "pearson", show.p = T, label.sep = ", ",
                     label.x.npc = "left", label.y.npc = "top",
                     label.x = NULL, label.y = NULL,
                     geom = "text", position = "identity",  na.rm = FALSE, show.legend = NA,
                    inherit.aes = TRUE, ...) {
  layer(
    stat = StatCor, data = data, mapping = mapping, geom = geom,
    position = position, show.legend = show.legend, inherit.aes = inherit.aes,
    params = list(label.x.npc  = label.x.npc , label.y.npc  = label.y.npc,
                  label.x = label.x, label.y = label.y, show.p = show.p, label.sep = label.sep,
                  method = method, na.rm = na.rm, ...)
  )
}


StatCor<- ggproto("StatCor", Stat,
                  required_aes = c("x", "y"),
                  default_aes = aes(hjust = ..hjust.., vjust = ..vjust..),

                  compute_group = function(data, scales, method, label.x.npc, label.y.npc,
                                           label.x, label.y, show.p, label.sep)
                    {
                    if (length(unique(data$x)) < 2) {
                      # Not enough data to perform test
                      return(data.frame())
                    }
                    # Returns a data frame with estimate, p.value, label, method
                    .test <- .cor_test(data$x, data$y, method = method, show.p = show.p, label.sep = label.sep)
                    # Returns a data frame with label: x, y, hjust, vjust
                    .label.pms <- .label_params(data = data, scales = scales,
                                                label.x.npc = label.x.npc, label.y.npc = label.y.npc,
                                                label.x = label.x, label.y = label.y )
                    cbind(.test, .label.pms)
                  }
)





# Correlation test
#::::::::::::::::::::::::::::::::::::::::
# Returns a data frame: estimatel|p.value|method|label
.cor_test <- function(x, y, method = "pearson", show.p, label.sep = ", "){
  .cor <- stats::cor.test(x, y, method = method, exact = FALSE)
  z <- data.frame(estimate = .cor$estimate, p.value = .cor$p.value, method = method)
  pval <- .cor$p.value

  if(show.p == T){
  
   pvaltxt <- ifelse(pval < 2.2e-16, "p < 2.2e-16",
                    
                      paste("p =", signif(pval, 2)))
  
    cortxt <- paste0("r = ", signif(.cor$estimate, 2),
                   
                    label.sep,  pvaltxt)
  }
  else {
    pvaltxt <- ifelse(pval < 2.2e-16, "p < 2.2e-16",
                      
                      paste("p =", signif(pval, 2)))
    
    cortxt <- paste0("r = ", signif(.cor$estimate, 2))
  }
  z$label <- cortxt
  z
}
