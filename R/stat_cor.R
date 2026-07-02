#' @include utilities.R utilities_label.R p_format_utils.R
NULL

# stat_cor()'s label-positioning logic was adapted from ggpmisc::stat_correlation() /
# stat_poly_eq() by Pedro J. Aphalo. For an actively maintained correlation statistic
# with more features, see the 'ggpmisc' package.

#' Add Correlation Coefficients with P-values to a Scatter Plot
#' @description Add correlation coefficients with p-values to a scatter plot. Can
#'  be also used to add `R2`.
#' @inheritParams ggpubr-common-params
#' @inheritParams ggplot2::layer
#' @param method a character string indicating which correlation coefficient (or
#'  covariance) is to be computed. One of "pearson" (default), "kendall", or
#'  "spearman".
#' @param alternative a character string specifying the alternative hypothesis,
#'  must be one of "two.sided" (default), "greater" or "less". You can specify
#'  just the initial letter.
#' @param cor.coef.name character. Can be one of \code{"R"} (pearson coef),
#'  \code{"rho"} (spearman coef) and \code{"tau"} (kendall coef). Uppercase and
#'  lowercase are allowed.
#' @param label.sep a character string to separate the terms. Default is ", ", to
#'  separate the correlation coefficient and the p.value.
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
#'  units) between the labels of successive groups when several groups are present.
#'  Default is 1.4 (unchanged behavior). Set \code{label.y.step = 0} to stop the
#'  per-group vertical shift, so that labels align across facet panels when a
#'  factor is mapped to an aesthetic that also defines the facets. This places the
#'  labels flush with the top of each panel; it is similar in spirit to the
#'  \code{aes(group = 1)} workaround, which instead leaves them one text line lower.
#' @param output.type character One of "expression", "latex", "tex" or "text".
#' @param digits,r.digits,p.digits integer indicating the number of decimal
#'  places (round) or significant digits (signif) to be used for the correlation
#'  coefficient and the p-value, respectively..
#' @param r.accuracy a real value specifying the number of decimal places of
#'  precision for the correlation coefficient. Default is NULL. Use (e.g.) 0.01
#'  to show 2 decimal places of precision. If specified, then \code{r.digits} is
#'  ignored.
#' @param p.accuracy a real value specifying the number of decimal places of
#'  precision for the p-value. Default is NULL. Use (e.g.) 0.0001 to show 4
#'  decimal places of precision. If specified, then \code{p.digits} is ignored.
#' @param r.leading.zero logical. Whether to include the leading zero before the
#'  decimal point in the correlation coefficient (e.g., \code{"0.73"} vs
#'  \code{".73"}). Default (NULL) keeps the leading zero; set to \code{FALSE}
#'  for APA-style reporting.
#' @param p.coef.name character. Symbol used for the p-value label. Default is
#'  \code{"p"}; use \code{"P"} for an uppercase p-value label. For
#'  \code{output.type = "expression"} this should be a single valid plotmath
#'  symbol (e.g. \code{"P"}), since it is parsed as an expression.
#' @param p.format.style character specifying the p-value formatting style.
#'  One of "default", "apa", "nejm", "lancet", "ama", "graphpad", "scientific".
#'  Default is "default" for backward compatibility.
#' @param p.leading.zero logical. Whether to include leading zero before decimal
#'  point (e.g., "0.05" vs ".05"). If NULL, uses the style's default setting.
#' @param p.decimal.mark character string to use as the decimal mark. If NULL,
#'  uses \code{getOption("OutDec")}.
#' @param ... other arguments to pass to \code{\link[ggplot2]{geom_text}} or
#'  \code{\link[ggplot2:geom_text]{geom_label}}.
#' @param na.rm If FALSE (the default), removes missing values with a warning. If
#'  TRUE silently removes missing values.
#' @references \code{stat_cor()}'s label-positioning logic was adapted from
#'  \code{stat_correlation()} / \code{stat_poly_eq()} in the 'ggpmisc' package by
#'  Pedro J. Aphalo. For an actively maintained correlation statistic with more features,
#'  see \code{ggpmisc::stat_correlation()}.
#' @seealso \code{\link{ggscatter}}. For an alternative implementation with more
#'  control over label positioning (native NPC coordinates, per-group vertical and
#'  horizontal steps), see \code{ggpmisc::stat_correlation()}, from which some of
#'  the label-positioning logic in \code{stat_cor()} was originally adapted.
#' @section Computed variables: \describe{ \item{r}{correlation coefficient}
#'  \item{rr}{correlation coefficient squared} \item{r.label}{formatted label
#'  for the correlation coefficient} \item{rr.label}{formatted label for the
#'  squared correlation coefficient} \item{p.label}{label for the p-value}
#'  \item{label}{default labeldisplayed by \code{stat_cor()}} }
#'
#' @examples
#' # Load data
#' data("mtcars")
#' df <- mtcars
#' df$cyl <- as.factor(df$cyl)
#'
#' # Scatter plot with correlation coefficient
#' # :::::::::::::::::::::::::::::::::::::::::::::::::
#' sp <- ggscatter(df,
#'   x = "wt", y = "mpg",
#'   add = "reg.line", # Add regressin line
#'   add.params = list(color = "blue", fill = "lightgray"), # Customize reg. line
#'   conf.int = TRUE # Add confidence interval
#' )
#' # Add correlation coefficient
#' sp + stat_cor(method = "pearson", label.x = 3, label.y = 30)
#'
#' # Specify the number of decimal places of precision for p and r
#' # Using 3 decimal places for the p-value and
#' # 2 decimal places for the correlation coefficient (r)
#' sp + stat_cor(p.accuracy = 0.001, r.accuracy = 0.01)
#'
#' # Show only the r.label but not the p.label
#' sp + stat_cor(aes(label = after_stat(r.label)), label.x = 3)
#'
#' # Use R2 instead of R
#' ggscatter(df, x = "wt", y = "mpg", add = "reg.line") +
#'   stat_cor(
#'     aes(label = paste(after_stat(rr.label), after_stat(p.label), sep = "~`,`~")),
#'     label.x = 3
#'   )
#'
#' # Color by groups and facet
#' # ::::::::::::::::::::::::::::::::::::::::::::::::::::
#' sp <- ggscatter(df,
#'   x = "wt", y = "mpg",
#'   color = "cyl", palette = "jco",
#'   add = "reg.line", conf.int = TRUE
#' )
#' sp + stat_cor(aes(color = cyl), label.x = 3)
#'
#' @export
stat_cor <- function(mapping = NULL, data = NULL,
                     method = "pearson", alternative = "two.sided",
                     cor.coef.name = c("R", "rho", "tau"), label.sep = ", ",
                     label.x.npc = "left", label.y.npc = "top",
                     label.x = NULL, label.y = NULL, label.y.step = 1.4,
                     output.type = "expression",
                     digits = 2, r.digits = digits, p.digits = digits,
                     r.accuracy = NULL, p.accuracy = NULL,
                     r.leading.zero = NULL,
                     p.format.style = "default", p.leading.zero = NULL,
                     p.decimal.mark = NULL, p.coef.name = "p",
                     geom = "text", position = "identity", na.rm = FALSE, show.legend = NA,
                     inherit.aes = TRUE, ...) {
  parse <- ifelse(output.type == "expression", TRUE, FALSE)
  cor.coef.name <- cor.coef.name[1]
  layer(
    stat = StatCor, data = data, mapping = mapping, geom = geom,
    position = position, show.legend = show.legend, inherit.aes = inherit.aes,
    params = list(
      label.x.npc = label.x.npc, label.y.npc = label.y.npc,
      label.x = label.x, label.y = label.y, label.y.step = label.y.step,
      label.sep = label.sep,
      method = method, alternative = alternative, output.type = output.type, digits = digits,
      r.digits = r.digits, p.digits = p.digits, r.accuracy = r.accuracy,
      p.accuracy = p.accuracy, r.leading.zero = r.leading.zero,
      p.format.style = p.format.style,
      p.leading.zero = p.leading.zero, p.decimal.mark = p.decimal.mark,
      cor.coef.name = cor.coef.name, p.coef.name = p.coef.name,
      parse = parse, na.rm = na.rm, ...
    )
  )
}


StatCor <- ggproto("StatCor", Stat,
  required_aes = c("x", "y"),
  default_aes = aes(hjust = after_stat(hjust), vjust = after_stat(vjust)),
  compute_group = function(data, scales, method, alternative, label.x.npc, label.y.npc,
                           label.x, label.y, label.y.step = 1.4, label.sep, output.type, digits,
                           r.digits, p.digits, r.accuracy, p.accuracy, r.leading.zero,
                           p.format.style, p.leading.zero, p.decimal.mark, cor.coef.name,
                           p.coef.name) {
    if (length(unique(data$x)) < 2) {
      # Not enough data to perform test
      return(data.frame())
    }
    # Returns a data frame with estimate, p.value, label, method
    .test <- .cor_test(
      data$x, data$y,
      method = method, alternative = alternative,
      label.sep = label.sep, output.type = output.type, digits = digits,
      r.digits = r.digits, p.digits = p.digits,
      r.accuracy = r.accuracy, p.accuracy = p.accuracy,
      r.leading.zero = r.leading.zero,
      p.format.style = p.format.style, p.leading.zero = p.leading.zero,
      p.decimal.mark = p.decimal.mark,
      cor.coef.name = cor.coef.name, p.coef.name = p.coef.name
    )
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


# Correlation test
# ::::::::::::::::::::::::::::::::::::::::
# Returns a data frame: estimatel|p.value|method|label
.cor_test <- function(x, y, method = "pearson", alternative = "two.sided",
                      label.sep = ", ", output.type = "expression",
                      digits = 2, r.digits = digits, p.digits = digits,
                      r.accuracy = NULL, p.accuracy = NULL, r.leading.zero = NULL,
                      p.format.style = "default", p.leading.zero = NULL,
                      p.decimal.mark = NULL,
                      cor.coef.name = "R", p.coef.name = "p") {
  # Overwritting digits by accuracy, if specified
  if (!is.null(p.accuracy)) {
    nb_decimal_places <- round(abs(log10(p.accuracy)))
    p.digits <- nb_decimal_places
  }
  if (!is.null(r.accuracy)) {
    nb_decimal_places <- round(abs(log10(r.accuracy)))
    r.digits <- nb_decimal_places
  }

  # Correlation analyses
  .cor <- suppressWarnings(stats::cor.test(
    x, y,
    method = method, alternative = alternative,
    use = "complete.obs"
  ))
  estimate <- p.value <- p <- r <- rr <- NULL
  z <- data.frame(estimate = .cor$estimate, p.value = .cor$p.value, method = method) %>%
    mutate(
      r = signif(estimate, r.digits),
      rr = signif(estimate^2, r.digits),
      p = signif(p.value, p.digits)
    )

  # Defining p and r labels
  pval <- .cor$p.value
  z <- z %>%
    dplyr::mutate(
      r.label = get_corcoef_label(
        r,
        accuracy = r.accuracy, prefix = "R",
        cor.coef.name = cor.coef.name, type = output.type,
        leading.zero = r.leading.zero
      ),
      rr.label = get_corcoef_label(
        rr,
        accuracy = r.accuracy, prefix = "R2",
        cor.coef.name = cor.coef.name, type = output.type,
        leading.zero = r.leading.zero
      ),
      p.label = get_p_label(
        pval,
        p.digits = p.digits, accuracy = p.accuracy, type = output.type,
        p.format.style = p.format.style, p.leading.zero = p.leading.zero,
        p.decimal.mark = p.decimal.mark, p.coef.name = p.coef.name
      )
    )

  # Defining correlation labels
  if (output.type == "expression") {
    if (label.sep == "\n") {
      # Line break at each comma
      cortxt <- paste0("atop(", z$r.label, ",", z$p.label, ")")
    } else {
      label.sep <- trimws(label.sep)
      if (label.sep == "") {
        label.sep <- "~"
      } #  Using "*" to avoid the space between the R2 value and comma
      else {
        label.sep <- paste0("*`", label.sep, "`~")
      }
      cortxt <- paste0(z$r.label, label.sep, z$p.label)
    }
  } else if (output.type %in% c("latex", "tex", "text")) {
    cortxt <- paste0(z$r.label, label.sep, z$p.label)
  }
  z$label <- cortxt
  z
}


# Formatting R and P ----------------------
get_p_label <- function(x, p.digits = 2, accuracy = 0.0001, type = "expression",
                        p.format.style = "default", p.leading.zero = NULL,
                        p.decimal.mark = NULL, p.coef.name = "p") {
  if (is.null(p.decimal.mark)) {
    p.decimal.mark <- getOption("OutDec")
  }

  if (!is.null(p.format.style) && p.format.style != "default") {
    p_digits <- p.digits
    if (!is.null(accuracy)) {
      p_digits <- round(abs(log10(accuracy)))
    }
    digits_by_value <- .signif_to_decimal_digits(x, p_digits)
    p_formatted <- vapply(seq_along(x), function(i) {
      format_p_value(
        x[i],
        style = p.format.style,
        digits = digits_by_value[i],
        leading.zero = p.leading.zero,
        min.threshold = NULL,
        decimal.mark = p.decimal.mark
      )
    }, character(1))
    label <- create_p_label(p_formatted)
  } else {
    # Backward compatible behavior (scales::pvalue + accuracy)
    if (is.null(accuracy)) {
      label <- ifelse(x < 2.2e-16, "p < 2.2e-16", paste0("p = ", x))
    } else if (!(accuracy < 1)) {
      stop(
        "Accuracy should be < 1; For example use 0.01, 0.001, 0.0001, etc.",
        call. = FALSE
      )
    } else {
      label <- scales::pvalue(x, accuracy = accuracy, add_p = TRUE)
      # Add space before and after: = or <
      label <- gsub(pattern = "(=|<)", replacement = " \\1 ", x = label)
    }

    if (!is.null(p.leading.zero) && !p.leading.zero) {
      label <- sub("^((?:p\\s*[=<]\\s*))0\\.", "\\1.", label, perl = TRUE)
      label <- sub("^((?:p\\s*[=<]\\s*))-0\\.", "\\1-.", label, perl = TRUE)
    }
    if (!is.null(p.decimal.mark) && p.decimal.mark != ".") {
      label <- sub("^((?:p\\s*[=<]\\s*)-?[0-9]+)\\.", paste0("\\1", p.decimal.mark), label, perl = TRUE)
    }
  }

  if (type == "expression") {
    label <- gsub(pattern = "p = ", replacement = "italic(p)~`=`~", x = label, fixed = TRUE)
    label <- gsub(pattern = "p < ", replacement = "italic(p)~`<`~", x = label, fixed = TRUE)
    # Quote a value whose leading zero was explicitly dropped (p.leading.zero =
    # FALSE) so plotmath renders it literally instead of re-adding the zero
    # (e.g. .001 -> "0.001"). Scoped to the explicit flag so p.format.style
    # presets keep their existing numeric rendering (#540).
    if (!is.null(p.leading.zero) && !p.leading.zero) {
      label <- .quote_leading_zero_dropped(label)
    }
  }
  # Optionally use a custom symbol for the p-value label, e.g. uppercase "P" (#541).
  if (!identical(p.coef.name, "p")) {
    if (type == "expression") {
      label <- gsub("italic(p)", paste0("italic(", p.coef.name, ")"), label, fixed = TRUE)
    } else {
      label <- sub("^p", p.coef.name, label)
    }
  }
  label
}

.signif_to_decimal_digits <- function(x, sig_digits) {
  if (is.null(sig_digits)) {
    sig_digits <- 2L
  }
  x_abs <- abs(x)
  out <- ifelse(
    is.na(x_abs) | x_abs == 0,
    sig_digits,
    pmax(0, sig_digits - 1L - floor(log10(x_abs)))
  )
  as.integer(out)
}

# Prefix can be R or R^2.
# cor.coef.name: R, rho, tau
get_corcoef_label <- function(x, accuracy = 0.01, prefix = "R", cor.coef.name = "R", type = "expression",
                              leading.zero = NULL) {
  if (is.null(accuracy)) {
    label <- paste0(prefix, " = ", x)
  } else if (!(accuracy < 1)) {
    stop(
      "Accuracy should be < 1; For example use 0.01, 0.001, 0.0001, etc.",
      call. = FALSE
    )
  } else {
    nb_decimal_places <- round(abs(log10(accuracy)))
    label <- formatC(x, digits = nb_decimal_places, format = "f", decimal.mark = ".")
    label <- paste0(prefix, " = ", label)
  }
  if (!is.null(leading.zero) && !leading.zero) {
    # Drop the leading zero of the coefficient value (e.g. "R = 0.20" -> "R = .20",
    # "R = -0.87" -> "R = -.87") for APA-style reporting (#540).
    label <- sub("= (-?)0\\.", "= \\1.", label)
  }
  if (type == "expression") {
    label <- gsub(pattern = "R2 = ", replacement = "italic(R)^2~`=`~", x = label, fixed = TRUE)
    label <- gsub(pattern = "R = ", replacement = "italic(R)~`=`~", x = label, fixed = TRUE)
    if (!is.null(leading.zero) && !leading.zero) {
      label <- .quote_leading_zero_dropped(label)
    }
  }
  label <- gsub(pattern = "R", cor.coef.name, x = label, fixed = TRUE)
  label
}

# In plotmath, a bare value whose leading zero was dropped (e.g. .87) is parsed
# as a number and rendered back WITH the zero (0.87). To make APA-style
# leading-zero suppression survive rendering, quote such a value so plotmath
# treats it as a literal string, keeping any minus sign outside the quotes
# (e.g. italic(R)~`=`~-.87  ->  italic(R)~`=`~-".87"). Only values that start
# with "." (i.e. already stripped) are affected; "0.87", "1.00", scientific
# notation are left untouched (#540).
.quote_leading_zero_dropped <- function(label) {
  sub("~(-?)(\\.[0-9]+)$", '~\\1"\\2"', label)
}
