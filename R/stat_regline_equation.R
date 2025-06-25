#' @include utilities.R utilities_label.R
#' @importFrom dplyr everything
#' @importFrom dplyr select
NULL
#'Add Regression Line Equation and R-Square to a GGPLOT.
#'@description Add regression line equation and R^2 to a ggplot. Regression
#'  model is fitted using the function \code{\link[stats]{lm}}.
#'@inheritParams ggpubr-common-params
#'@inheritParams ggplot2::layer
#'@param formula a formula object
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
#'  \code{\link[ggplot2:geom_text]{geom_label}}.
#'@param na.rm If FALSE (the default), removes missing values with a warning. If
#'  TRUE silently removes missing values.
#'@seealso \code{\link{ggscatter}}
#'@references the source code of the function \code{stat_regline_equation()} is
#'  inspired from the code of the function \code{stat_poly_eq()} (in ggpmisc
#'  package).
#'
#' @section Computed variables:
#'   \describe{ \item{x}{x position for left edge}
#'   \item{y}{y position near upper edge}
#'   \item{eq.label}{equation for the
#'   fitted polynomial as a character string to be parsed}
#'   \item{rr.label}{\eqn{R^2} of the fitted model as a character string to be parsed}
#'   \item{adj.rr.label}{Adjusted \eqn{R^2} of the fitted model as a character string
#'   to be parsed}
#'   \item{AIC.label}{AIC for the fitted model.}
#'   \item{BIC.label}{BIC for the fitted model.}
#'   \item{hjust}{Set to zero to override the default of the "text" geom.}}
#' @examples
#'
#' # Simple scatter plot with correlation coefficient and
#' # regression line
#' #::::::::::::::::::::::::::::::::::::::::::::::::::::
#' ggscatter(mtcars, x = "wt", y = "mpg", add = "reg.line") +
#'   stat_cor(label.x = 3, label.y = 34) +
#'   stat_regline_equation(label.x = 3, label.y = 32)
#'
#'
#' # Groupped scatter plot
#' #::::::::::::::::::::::::::::::::::::::::::::::::::::
#' ggscatter(
#'   iris, x = "Sepal.Length", y = "Sepal.Width",
#'   color = "Species", palette = "jco",
#'   add = "reg.line"
#'   ) +
#'   facet_wrap(~Species) +
#'   stat_cor(label.y = 4.4) +
#'   stat_regline_equation(label.y = 4.2)
#'
#' # Polynomial equation
#' #::::::::::::::::::::::::::::::::::::::::::::::::::::
#'
#' # Demo data
#' set.seed(4321)
#' x <- 1:100
#' y <- (x + x^2 + x^3) + rnorm(length(x), mean = 0, sd = mean(x^3) / 4)
#' my.data <- data.frame(x, y, group = c("A", "B"),
#'                       y2 = y * c(0.5,2), block = c("a", "a", "b", "b"))
#'
#' # Fit polynomial regression line and add labels
#' formula <- y ~ poly(x, 3, raw = TRUE)
#' p <- ggplot(my.data, aes(x, y2, color = group)) +
#'   geom_point() +
#'   stat_smooth(aes(fill = group, color = group), method = "lm", formula = formula) +
#'   stat_regline_equation(
#'     aes(label =  paste(..eq.label.., ..adj.rr.label.., sep = "~~~~")),
#'     formula = formula
#'   ) +
#'   theme_bw()
#' ggpar(p, palette = "jco")
#'
#'@export
stat_regline_equation <- function(
  mapping = NULL, data = NULL, formula = y~x,
  label.x.npc = "left", label.y.npc = "top",
  label.x = NULL, label.y = NULL, output.type = "expression",
  geom = "text", position = "identity",  na.rm = FALSE, show.legend = NA,
  inherit.aes = TRUE, ...
  )
  {

  parse <- ifelse(output.type == "expression", TRUE, FALSE)

  layer(
    stat = StatReglineEquation, data = data, mapping = mapping, geom = geom,
    position = position, show.legend = show.legend, inherit.aes = inherit.aes,
    params = list(formula = formula, label.x.npc  = label.x.npc , label.y.npc  = label.y.npc,
                  label.x = label.x, label.y = label.y,
                  output.type = output.type, parse = parse, na.rm = na.rm, ...)
  )
}


StatReglineEquation<- ggproto("StatReglineEquation", Stat,
                  required_aes = c("x", "y"),
                  default_aes = aes(label = ..eq.label.., hjust = ..hjust.., vjust = ..vjust..),

                  compute_group = function(data, scales, formula, label.x.npc, label.y.npc,
                                           label.x, label.y, output.type)
                    {

                    force(data)

                    if (length(unique(data$x)) < 2) {
                      return(data.frame()) # Not enough data to perform test
                    }

                    .test <- .stat_lm(formula, data, output.type = output.type)
                    # Returns a data frame with label: x, y, hjust, vjust
                    .label.pms <- .label_params(data = data, scales = scales,
                                                label.x.npc = label.x.npc, label.y.npc = label.y.npc,
                                                label.x = label.x, label.y = label.y ) %>%
                      mutate(hjust = 0)
                    cbind(.test, .label.pms)
                  }
)



# Compute regression line equation
.stat_lm <- function(formula, data, output.type = "expression"){

  res.lm <- stats::lm(formula, data)
  coefs <- stats::coef(res.lm)

  formula.rhs.chr <- as.character(formula)[3]
  if (grepl("-1", formula.rhs.chr) || grepl("- 1", formula.rhs.chr)) {
    coefs <- c(0, coefs)
  }

  rr <- summary(res.lm)$r.squared %>% signif(2)
  adj.rr <- summary(res.lm)$adj.r.squared %>% signif(2)
  AIC <- stats::AIC(res.lm) %>% signif(2)
  BIC <- stats::BIC(res.lm) %>% signif(2)

  # Build model equation
  eq.char <- as.character(signif(polynom::as.polynomial(coefs), 2))
  eq.char <- gsub("e([+-]?[0-9]*)", "%*%10^\\1", eq.char)
  if (output.type %in% c("latex", "tex", "tikz")) {
    eq.char <- gsub("*", " ", eq.char, fixed = TRUE)
  }
  # Add y
  if (output.type == "expression") {
    lhs <- "italic(y)~`=`~"
  } else if (output.type %in% c("latex", "tex", "tikz", "text")) {
    lhs <- "y = "
  }
  eq.char <- paste(lhs, eq.char, sep = "")

  # Build data frame with the output
  if (output.type == "expression") {
    eq.x.rhs = "~italic(x)"
  } else {
    eq.x.rhs = " x"
  }

  if (output.type == "expression") {
    z <- data.frame(eq.label = gsub("x", eq.x.rhs, eq.char, fixed = TRUE),
                    rr.label = paste("italic(R)^2", rr, sep = "~`=`~"),
                    adj.rr.label = paste("italic(R)[adj]^2",
                                         adj.rr, sep = "~`=`~"),
                    AIC.label = paste("AIC", AIC, sep = "~`=`~"),
                    BIC.label = paste("BIC", BIC, sep = "~`=`~"))
  } else if (output.type %in% c("latex", "tex", "text")) {
    z <- data.frame(eq.label = gsub("x", eq.x.rhs, eq.char, fixed = TRUE),
                    rr.label = paste("R^2", rr, sep = " = "),
                    adj.rr.label = paste("R_{adj}^2",adj.rr, sep = " = "),
                    AIC.label = paste("AIC", AIC, sep = " = "),
                    BIC.label = paste("BIC", BIC, sep = " = "))
  }

  z <- z %>%
    mutate(rr = rr, adj.rr = adj.rr, AIC = AIC, BIC = BIC) %>%
    dplyr::select(rr, adj.rr, AIC, BIC, everything())

  z
}

