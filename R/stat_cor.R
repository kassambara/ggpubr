#' @include utilities.R utilities_label.R
NULL
#'Add Correlation Coefficients with P-values to a Scatter Plot
#'@description Add correlation coefficients with p-values to a scatter plot. Can
#'  be also used to add `R2`.
#'@inheritParams ggplot2::layer
#'@param method a character string indicating which correlation coefficient (or
#'  covariance) is to be computed. One of "pearson" (default), "kendall", or
#'  "spearman".
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
#'@param output.type character One of "expression", "latex" or "text".
#'@param ... other arguments to pass to \code{\link[ggplot2]{geom_text}} or
#'  \code{\link[ggplot2]{geom_label}}.
#'@param na.rm If FALSE (the default), removes missing values with a warning. If
#'  TRUE silently removes missing values.
#'@seealso \code{\link{ggscatter}}
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
#'# Use R2 instead of R
#'ggscatter(df, x = "wt", y = "mpg", add = "reg.line") +
#'  stat_cor(
#'    aes(label = paste(..rr.label.., ..p.label.., sep = "~`,`~")),
#'   label.x = 3
#' )
#'
#' # Color by groups and facet
#' #::::::::::::::::::::::::::::::::::::::::::::::::::::
#' sp <- ggscatter(df, x = "wt", y = "mpg",
#'    color = "cyl", palette = "jco",
#'    add = "reg.line", conf.int = TRUE)
#' sp + stat_cor(aes(color = cyl), label.x = 3)
#'
#'@export
stat_cor <- function(mapping = NULL, data = NULL,
                     method = "pearson", label.sep = ", ",
                     label.x.npc = "left", label.y.npc = "top",
                     label.x = NULL, label.y = NULL, output.type = "expression",
                     geom = "text", position = "identity",  na.rm = FALSE, show.legend = NA,
                    inherit.aes = TRUE, ...) {
  parse <- ifelse(output.type == "expression", TRUE, FALSE)
  layer(
    stat = StatCor, data = data, mapping = mapping, geom = geom,
    position = position, show.legend = show.legend, inherit.aes = inherit.aes,
    params = list(label.x.npc  = label.x.npc , label.y.npc  = label.y.npc,
                  label.x = label.x, label.y = label.y, label.sep = label.sep,
                  method = method, output.type = output.type, parse = parse, na.rm = na.rm, ...)
  )
}


StatCor<- ggproto("StatCor", Stat,
                  required_aes = c("x", "y"),
                  default_aes = aes(hjust = ..hjust.., vjust = ..vjust..),

                  compute_group = function(data, scales, method, label.x.npc, label.y.npc,
                                           label.x, label.y, label.sep, output.type)
                    {
                    if (length(unique(data$x)) < 2) {
                      # Not enough data to perform test
                      return(data.frame())
                    }
                    # Returns a data frame with estimate, p.value, label, method
                    .test <- .cor_test(
                      data$x, data$y, method = method, label.sep = label.sep,
                      output.type = output.type
                      )
                    # Returns a data frame with label: x, y, hjust, vjust
                    .label.pms <- .label_params(data = data, scales = scales,
                                                label.x.npc = label.x.npc, label.y.npc = label.y.npc,
                                                label.x = label.x, label.y = label.y ) %>%
                      mutate(hjust = 0)
                    cbind(.test, .label.pms)
                  }
)





# Correlation test
#::::::::::::::::::::::::::::::::::::::::
# Returns a data frame: estimatel|p.value|method|label
.cor_test <- function(x, y, method = "pearson", label.sep = ", ", output.type = "expression"){

  .cor <- stats::cor.test(x, y, method = method, exact = FALSE, use = "complete.obs")
  estimate <- p.value <- p <- r <- rr <-  NULL
  z <- data.frame(estimate = .cor$estimate, p.value = .cor$p.value, method = method) %>%
    mutate(
      r = signif(estimate, 2),
      rr = signif(estimate^2, 2),
      p = signif(p.value, 2)
    )


  # Defining labels
  pval <- .cor$p.value

  if(output.type == "expression"){

    z <- z %>% dplyr::mutate(
      r.label = paste("italic(R)", r, sep = "~`=`~"),
      rr.label = paste("italic(R)^2", rr, sep = "~`=`~"),
      p.label = paste("italic(p)", p, sep = "~`=`~")
    )
    # Default label
    pvaltxt <- ifelse(pval < 2.2e-16, "italic(p)~`<`~2.2e-16",
                      paste("italic(p)~`=`~", signif(pval, 2)))
    if(label.sep == "\n"){
      # Line break at each comma
      cortxt <- paste0("atop(italic(R)~`=`~", signif(.cor$estimate, 2),
                       ",",  pvaltxt, ")")
    }
    else{
      label.sep <- trimws(label.sep)
      if(label.sep == "") label.sep <- "~"
      else label.sep <- paste0("~`", label.sep, "`~")
      cortxt <- paste0("italic(R)~`=`~", signif(.cor$estimate, 2),
                       label.sep,  pvaltxt)
    }

    z$label <- cortxt

  }
  else if (output.type %in% c("latex", "tex", "text")){

    z <- z %>% dplyr::mutate(
      r.label = paste("R", r, sep = " = "),
      rr.label = paste("R2", rr, sep = " = "),
      p.label = paste("p", p, sep = "=")
    )

    # Default label
    pvaltxt <- ifelse(pval < 2.2e-16, "p < 2.2e-16",
                      paste("p =", signif(pval, 2)))
    cortxt <- paste0("R = ", signif(.cor$estimate, 2),
                     label.sep,  pvaltxt)
    z$label <- cortxt
  }

  z
}


