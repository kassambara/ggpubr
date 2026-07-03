#' @include utilities.R utilities_label.R utils_stat_test_label.R
#' @importFrom dplyr everything
#' @importFrom dplyr select
NULL

# stat_regline_equation() was adapted from the source code of ggpmisc::stat_poly_eq()
# by Pedro J. Aphalo. For an actively maintained version with additional features and
# bug fixes, see the 'ggpmisc' package.

#' Add Regression Line Equation and R-Square to a GGPlot.
#' @description Add regression line equation and R^2 to a ggplot. Regression
#'  model is fitted using the function \code{\link[stats]{lm}}.
#' @inheritParams ggpubr-common-params
#' @inheritParams ggplot2::layer
#' @param formula a formula object
#' @param label.x.npc,label.y.npc can be \code{numeric} or \code{character}
#'  vector of the same length as the number of groups and/or panels. If too
#'  short they will be recycled. \itemize{ \item If \code{numeric}, value should
#'  be between 0 and 1. Coordinates to be used for positioning the label,
#'  expressed in "normalized parent coordinates". \item If \code{character},
#'  allowed values include: i) one of c('right', 'left', 'center', 'centre',
#'  'middle') for x-axis; ii) and one of c( 'bottom', 'top', 'center', 'centre',
#'  'middle') for y-axis.}
#'
#'  If too short they will be recycled.
#' @param label.x,label.y \code{numeric} Coordinates (in data units) to be used
#'  for absolute positioning of the label. If too short they will be recycled.
#' @param label.y.step numeric value giving the vertical spacing (in text-line
#'  units) between the labels of successive groups. Default is 1.4 (unchanged
#'  behavior). Set \code{label.y.step = 0} to stop the per-group vertical shift so
#'  labels align across facet panels.
#' @param output.type character One of "expression", "latex" or "text".
#' @param decreasing logical. If \code{TRUE} (the default), the equation is
#'   formatted in standard mathematical convention with terms in decreasing
#'   order of powers (e.g., "y = 2*x + 1"). If \code{FALSE}, terms are in
#'   increasing order (e.g., "y = 1 + 2*x").
#' @param coef.digits integer indicating the number of significant digits to use
#'   for the regression equation coefficients. Default is 2.
#' @param rr.digits integer indicating the number of significant digits to use
#'   for R2 and adjusted R2. Default is 2.
#' @param ... other arguments to pass to \code{\link[ggplot2]{geom_text}} or
#'  \code{\link[ggplot2:geom_text]{geom_label}}.
#' @param na.rm If FALSE (the default), removes missing values with a warning. If
#'  TRUE silently removes missing values.
#' @seealso \code{\link{ggscatter}}
#' @references \code{stat_regline_equation()} was adapted from the source code of
#'  \code{stat_poly_eq()} in the 'ggpmisc' package by Pedro J. Aphalo. For an actively
#'  maintained version of this statistic, with additional features and bug fixes, see
#'  \code{ggpmisc::stat_poly_eq()}.
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
#' # ::::::::::::::::::::::::::::::::::::::::::::::::::::
#' ggscatter(mtcars, x = "wt", y = "mpg", add = "reg.line") +
#'   stat_cor(label.x = 3, label.y = 34) +
#'   stat_regline_equation(label.x = 3, label.y = 32)
#'
#'
#' # Groupped scatter plot
#' # ::::::::::::::::::::::::::::::::::::::::::::::::::::
#' ggscatter(
#'   iris,
#'   x = "Sepal.Length", y = "Sepal.Width",
#'   color = "Species", palette = "jco",
#'   add = "reg.line"
#' ) +
#'   facet_wrap(~Species) +
#'   stat_cor(label.y = 4.4) +
#'   stat_regline_equation(label.y = 4.2)
#'
#' # Polynomial equation
#' # ::::::::::::::::::::::::::::::::::::::::::::::::::::
#'
#' # Demo data
#' set.seed(4321)
#' x <- 1:100
#' y <- (x + x^2 + x^3) + rnorm(length(x), mean = 0, sd = mean(x^3) / 4)
#' my.data <- data.frame(x, y,
#'   group = c("A", "B"),
#'   y2 = y * c(0.5, 2), block = c("a", "a", "b", "b")
#' )
#'
#' # Fit polynomial regression line and add labels
#' formula <- y ~ poly(x, 3, raw = TRUE)
#' p <- ggplot(my.data, aes(x, y2, color = group)) +
#'   geom_point() +
#'   stat_smooth(aes(fill = group, color = group), method = "lm", formula = formula) +
#'   stat_regline_equation(
#'     aes(label = paste(..eq.label.., ..adj.rr.label.., sep = "~~~~")),
#'     formula = formula
#'   ) +
#'   theme_bw()
#' ggpar(p, palette = "jco")
#'
#' @export
stat_regline_equation <- function(
  mapping = NULL, data = NULL, formula = y ~ x,
  label.x.npc = "left", label.y.npc = "top",
  label.x = NULL, label.y = NULL, label.y.step = 1.4,
  output.type = "expression", decreasing = TRUE,
  coef.digits = 2, rr.digits = 2,
  geom = "text", position = "identity", na.rm = FALSE, show.legend = NA,
  inherit.aes = TRUE, ...
) {
  parse <- ifelse(output.type == "expression", TRUE, FALSE)
  # Convert any dot-dot notation in user-provided mapping
  mapping <- convert_label_dotdot_notation_to_after_stat(mapping)

  layer(
    stat = StatReglineEquation, data = data, mapping = mapping, geom = geom,
    position = position, show.legend = show.legend, inherit.aes = inherit.aes,
    params = list(
      formula = formula, label.x.npc = label.x.npc, label.y.npc = label.y.npc,
      label.x = label.x, label.y = label.y, label.y.step = label.y.step,
      output.type = output.type, decreasing = decreasing,
      coef.digits = coef.digits, rr.digits = rr.digits,
      parse = parse, na.rm = na.rm, ...
    )
  )
}


StatReglineEquation <- ggproto("StatReglineEquation", Stat,
  required_aes = c("x", "y"),
  default_aes = aes(label = after_stat(eq.label), hjust = after_stat(hjust), vjust = after_stat(vjust)),
  compute_group = function(data, scales, formula, label.x.npc, label.y.npc,
                           label.x, label.y, label.y.step = 1.4, output.type, decreasing,
                           coef.digits = 2, rr.digits = 2) {
    force(data)

    if (length(unique(data$x)) < 2) {
      return(data.frame()) # Not enough data to perform test
    }

    .test <- .stat_lm(formula, data, output.type = output.type, decreasing = decreasing,
                      coef.digits = coef.digits, rr.digits = rr.digits)
    # Returns a data frame with label: x, y, hjust, vjust
    .label.pms <- .label_params(
      data = data, scales = scales,
      label.x.npc = label.x.npc, label.y.npc = label.y.npc,
      label.x = label.x, label.y = label.y, label.y.step = label.y.step
    ) %>%
      mutate(hjust = 0)
    cbind(.test, .label.pms)
  }
)


# Compute regression line equation
.stat_lm <- function(formula, data, output.type = "expression", decreasing = TRUE,
                     coef.digits = 2, rr.digits = 2) {
  res.lm <- stats::lm(formula, data)
  coefs <- stats::coef(res.lm)

  # With an orthogonal poly() fit (the default, raw = FALSE), coef() returns
  # coefficients in the orthogonal basis, but the displayed equation treats them
  # as raw polynomial coefficients (c0 + c1 x + c2 x^2 + ...), giving a wrong
  # equation (#653). For a simple poly(x, k) term with an intercept, refit with
  # raw = TRUE - an identical fit - to obtain the correct raw coefficients for
  # display. R^2/AIC/BIC are unchanged (same fit) and stay from the original
  # model. .polynomial_raw_formula() returns NULL for cases that are not safe to
  # rewrite (no intercept, transformed poly argument, ...), leaving the original
  # coefficients untouched.
  raw.formula <- .polynomial_raw_formula(formula)
  if (!is.null(raw.formula)) {
    coefs <- tryCatch(
      stats::coef(stats::lm(raw.formula, data)),
      error = function(e) coefs
    )
  }

  formula.rhs.chr <- as.character(formula)[3]
  if (grepl("-1", formula.rhs.chr) || grepl("- 1", formula.rhs.chr)) {
    coefs <- c(0, coefs)
  }

  rr <- summary(res.lm)$r.squared %>% signif(rr.digits)
  adj.rr <- summary(res.lm)$adj.r.squared %>% signif(rr.digits)
  AIC <- stats::AIC(res.lm) %>% signif(2)
  BIC <- stats::BIC(res.lm) %>% signif(2)

  # Build model equation
  eq.char <- as.character(signif(polynom::as.polynomial(coefs), coef.digits), decreasing = decreasing)
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
    eq.x.rhs <- "~italic(x)"
  } else {
    eq.x.rhs <- " x"
  }

  if (output.type == "expression") {
    z <- data.frame(
      eq.label = gsub("x", eq.x.rhs, eq.char, fixed = TRUE),
      rr.label = paste("italic(R)^2", rr, sep = "~`=`~"),
      adj.rr.label = paste("italic(R)[adj]^2",
        adj.rr,
        sep = "~`=`~"
      ),
      AIC.label = paste("AIC", AIC, sep = "~`=`~"),
      BIC.label = paste("BIC", BIC, sep = "~`=`~")
    )
  } else if (output.type %in% c("latex", "tex", "text")) {
    z <- data.frame(
      eq.label = gsub("x", eq.x.rhs, eq.char, fixed = TRUE),
      rr.label = paste("R^2", rr, sep = " = "),
      adj.rr.label = paste("R_{adj}^2", adj.rr, sep = " = "),
      AIC.label = paste("AIC", AIC, sep = " = "),
      BIC.label = paste("BIC", BIC, sep = " = ")
    )
  }

  z <- z %>%
    mutate(rr = rr, adj.rr = adj.rr, AIC = AIC, BIC = BIC) %>%
    dplyr::select(rr, adj.rr, AIC, BIC, everything())

  z
}

# If a formula fits an ORTHOGONAL polynomial - poly(x, k) or poly(x, k, raw =
# FALSE) - return an equivalent formula using raw = TRUE, whose coefficients are
# the raw polynomial coefficients suitable for display. Returns NULL when no
# rewrite is needed or cannot be done safely, in which case the original
# coefficients are used (unchanged behavior):
#   * no poly(), or poly() already raw = TRUE, or a raw form such as y ~ x /
#     y ~ x + I(x^2);
#   * a model without an intercept (- 1): an orthogonal poly() without an
#     intercept does NOT fit the same curve as its raw = TRUE counterpart, so
#     rewriting would not be an identical fit;
#   * a poly() whose first argument is itself a call, e.g. poly(log(x), 2): the
#     simple textual rewrite can't be applied reliably.
# Only a single, simple poly(<var>, ...) term is rewritten (#653).
.polynomial_raw_formula <- function(formula) {
  rhs <- paste(deparse(formula[[3]]), collapse = "")
  # Match one simple poly() call with no nested parentheses in its arguments.
  poly.call <- regmatches(rhs, regexpr("poly\\s*\\([^()]*\\)", rhs))
  if (length(poly.call) != 1) {
    return(NULL)
  }
  if (grepl("raw\\s*=\\s*TRUE", poly.call)) {
    return(NULL)
  }
  # No-intercept models: orthogonal vs raw poly are not the same fit.
  if (grepl("-\\s*1(\\b|$)", rhs)) {
    return(NULL)
  }
  if (grepl("raw\\s*=", poly.call)) {
    # explicit raw = FALSE (or other) -> force raw = TRUE
    new.call <- sub("raw\\s*=\\s*[^,)]+", "raw = TRUE", poly.call)
  } else {
    # no raw argument -> add it inside the poly() call
    new.call <- sub("\\)\\s*$", ", raw = TRUE)", poly.call)
  }
  rhs <- sub(poly.call, new.call, rhs, fixed = TRUE)
  tryCatch(
    stats::as.formula(
      paste(deparse(formula[[2]]), "~", rhs),
      env = environment(formula)
    ),
    error = function(e) NULL
  )
}
