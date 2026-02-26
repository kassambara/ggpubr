#' @include utilities.R
NULL

#' P-Value Formatting Styles
#'
#' @description Predefined p-value formatting styles based on major scientific
#'   publication standards and citation styles.
#'
#' @details The following styles are available:
#' \itemize{
#'   \item \code{default}: Current behavior with scientific notation (backward compatible)
#'   \item \code{apa}: APA Style - no leading zero, 3 decimals, threshold at 0.001
#'   \item \code{nejm}: New England Journal of Medicine - leading zero, 3 decimals, threshold at 0.001
#'   \item \code{lancet}: The Lancet - leading zero, 4 decimals, threshold at 0.0001
#'   \item \code{ama}: American Medical Association - no leading zero, 3 decimals, threshold at 0.001
#'   \item \code{graphpad}: GraphPad Prism - leading zero, 4 decimals, threshold at 0.0001
#'   \item \code{scientific}: Scientific notation for genomics/GWAS studies
#' }
#'
#' @name p_format_styles
#' @rdname p_format_styles
#' @keywords internal
.p_format_styles <- list(
  default = list(
    digits = 2,
    leading.zero = TRUE,
    min.threshold = NULL,
    use.scientific = TRUE,
    description = "Current behavior (backward compatible)"
  ),
  apa = list(
    digits = 3,
    leading.zero = FALSE,
    min.threshold = 0.001,
    use.scientific = FALSE,
    description = "APA Style (Psychology, Social Sciences)"
  ),
  nejm = list(
    digits = 3,
    leading.zero = TRUE,
    min.threshold = 0.001,
    use.scientific = FALSE,
    description = "New England Journal of Medicine"
  ),
  lancet = list(
    digits = 4,
    leading.zero = TRUE,
    min.threshold = 0.0001,
    use.scientific = FALSE,
    description = "The Lancet"
  ),
  ama = list(
    digits = 3,
    leading.zero = FALSE,
    min.threshold = 0.001,
    use.scientific = FALSE,
    description = "American Medical Association"
  ),
  graphpad = list(
    digits = 4,
    leading.zero = TRUE,
    min.threshold = 0.0001,
    use.scientific = FALSE,
    description = "GraphPad Prism Style"
  ),
  scientific = list(
    digits = 2,
    leading.zero = TRUE,
    min.threshold = NULL,
    use.scientific = TRUE,
    description = "Scientific notation (for GWAS, genomics)"
  )
)


#' Get P-Value Format Style Settings
#'
#' @description Returns the settings for a given p-value format style.
#'
#' @param style Character string specifying the style. One of: "default", "apa",
#'   "nejm", "lancet", "ama", "graphpad", "scientific".
#'
#' @return A list containing: digits, leading.zero, min.threshold, use.scientific, description.
#'
#' @examples
#' get_p_format_style("apa")
#' get_p_format_style("nejm")
#'
#' @export
get_p_format_style <- function(style = "default") {
  style <- match.arg(style, names(.p_format_styles))
  .p_format_styles[[style]]
}


#' List Available P-Value Format Styles
#'
#' @description Returns a data frame describing all available p-value formatting styles.
#'
#' @return A data frame with columns: style, digits, leading.zero, min.threshold,
#'   use.scientific, description.
#'
#' @examples
#' list_p_format_styles()
#'
#' @export
list_p_format_styles <- function() {
  styles_df <- do.call(rbind, lapply(names(.p_format_styles), function(name) {
    s <- .p_format_styles[[name]]
    data.frame(
      style = name,
      digits = s$digits,
      leading.zero = s$leading.zero,
      min.threshold = ifelse(is.null(s$min.threshold), NA, s$min.threshold),
      use.scientific = s$use.scientific,
      description = s$description,
      stringsAsFactors = FALSE
    )
  }))
  rownames(styles_df) <- NULL
  styles_df
}


#' Format P-Values According to Style or Custom Settings
#'
#' @description Formats p-values according to major scientific publication standards
#'   (APA, AMA, NEJM, Lancet, etc.) or custom user-defined settings. This function
#'   provides flexible control over decimal places, leading zeros, and threshold
#'   notation for very small p-values.
#'
#' @param p Numeric vector of p-values to format.
#' @param style Character string specifying the formatting style. One of:
#'   \itemize{
#'     \item \code{"default"}: Current behavior with scientific notation (backward compatible)
#'     \item \code{"apa"}: APA Style - no leading zero, 3 decimals, "< .001" threshold
#'     \item \code{"nejm"}: NEJM Style - leading zero, 3 decimals, "< 0.001" threshold
#'     \item \code{"lancet"}: Lancet Style - leading zero, 4 decimals, "< 0.0001" threshold
#'     \item \code{"ama"}: AMA Style - no leading zero, 3 decimals, "< .001" threshold
#'     \item \code{"graphpad"}: GraphPad Style - leading zero, 4 decimals, "< 0.0001" threshold
#'     \item \code{"scientific"}: Scientific notation for genomics/GWAS studies
#'   }
#' @param digits Integer specifying the number of decimal places. If provided,
#'   overrides the style default.
#' @param leading.zero Logical indicating whether to include leading zero before
#'   decimal point (e.g., "0.05" vs ".05"). If provided, overrides the style default.
#' @param min.threshold Numeric specifying the minimum p-value to display exactly.
#' @param decimal.mark Character string to use as the decimal mark. If NULL,
#'   uses \code{getOption("OutDec")}.
#'   Values below this threshold are shown as "< threshold" (e.g., "< 0.001").
#'   Set to NULL to always show exact values. If provided, overrides the style default.
#'
#' @return Character vector of formatted p-values.
#'
#' @details
#' P-value formatting conventions vary across scientific disciplines and journals:
#'
#' \strong{APA Style} (Psychology, Social Sciences):
#' \itemize{
#'   \item No leading zero (write ".05" not "0.05")
#'   \item 2-3 decimal places
#'   \item Report as "p < .001" for very small values
#' }
#'
#' \strong{NEJM/Medical Journals}:
#' \itemize{
#'   \item P > 0.01: 2 decimal places
#'   \item P between 0.001 and 0.01: 3 decimal places
#'   \item P < 0.001: report as "< 0.001"
#' }
#'
#' \strong{Scientific Notation} (GWAS, Genomics):
#' \itemize{
#'   \item Used when very small p-values are meaningful (e.g., 5e-8 threshold)
#'   \item Appropriate for high-dimensional data analyses
#' }
#'
#' @examples
#' # Test p-values
#' p_vals <- c(0.76404, 0.0432, 0.0043, 0.00018, 1.7e-11)
#'
#' # Different styles
#' format_p_value(p_vals, style = "default")
#' format_p_value(p_vals, style = "apa")
#' format_p_value(p_vals, style = "nejm")
#' format_p_value(p_vals, style = "lancet")
#'
#' # Custom formatting
#' format_p_value(p_vals, digits = 2, leading.zero = FALSE, min.threshold = 0.01)
#'
#' # Override style defaults
#' format_p_value(p_vals, style = "nejm", digits = 4)
#'
#' @seealso \code{\link{get_p_format_style}}, \code{\link{list_p_format_styles}}
#'
#' @export
format_p_value <- function(p,
                           style = "default",
                           digits = NULL,
                           leading.zero = NULL,
                           min.threshold = NULL,
                           decimal.mark = NULL) {
  # Validate style

  style <- match.arg(style, names(.p_format_styles))


  # Get style defaults

  style_settings <- .p_format_styles[[style]]


  # Apply overrides if provided

  if (is.null(digits)) digits <- style_settings$digits

  if (is.null(leading.zero)) leading.zero <- style_settings$leading.zero

  if (is.null(min.threshold)) min.threshold <- style_settings$min.threshold

  if (is.null(decimal.mark)) decimal.mark <- getOption("OutDec")

  use.scientific <- style_settings$use.scientific


  # Handle NA values
  result <- rep(NA_character_, length(p))
  valid_idx <- !is.na(p)

  if (!any(valid_idx)) {
    return(result)
  }

  p_valid <- p[valid_idx]

  if (use.scientific) {
    # Use vectorized format.pval for consistent formatting across values
    # This preserves the original behavior where format.pval formats all values
    # together to maintain consistent decimal places
    formatted <- format.pval(p_valid, digits = digits, decimal.mark = decimal.mark)

    # Apply threshold if specified
    if (!is.null(min.threshold)) {
      below_threshold <- p_valid < min.threshold
      if (any(below_threshold)) {
        threshold_digits <- max(digits, -floor(log10(min.threshold)))
        threshold_str <- format_single_p(min.threshold, threshold_digits, leading.zero, FALSE, decimal.mark)
        formatted[below_threshold] <- paste0("< ", threshold_str)
      }
    }

    # Apply leading zero removal if requested
    if (!leading.zero) {
      formatted <- sub("^0\\.", ".", formatted)
      formatted <- sub("^-0\\.", "-.", formatted)
    }
  } else {
    # Non-scientific styles: format individually
    formatted <- character(length(p_valid))
    for (i in seq_along(p_valid)) {
      pval <- p_valid[i]

      # Check if below threshold
      if (!is.null(min.threshold) && pval < min.threshold) {
        # Format threshold value - use enough digits to show the threshold
        threshold_digits <- max(digits, -floor(log10(min.threshold)))
        threshold_str <- format_single_p(min.threshold, threshold_digits, leading.zero, FALSE, decimal.mark)
        formatted[i] <- paste0("< ", threshold_str)
      } else {
        # Format exact value
        formatted[i] <- format_single_p(pval, digits, leading.zero, FALSE, decimal.mark)
      }
    }
  }

  result[valid_idx] <- formatted
  result
}


#' Format a Single P-Value
#'
#' @description Internal helper function to format a single p-value.
#'
#' @param p Single numeric p-value.
#' @param digits Number of decimal places.
#' @param leading.zero Whether to include leading zero.
#' @param scientific Whether to use scientific notation.
#' @param decimal.mark Character string to use as the decimal mark.
#'
#' @return Formatted character string.
#'
#' @keywords internal
format_single_p <- function(p, digits, leading.zero, scientific, decimal.mark) {
  if (is.na(p)) {
    return(NA_character_)
  }


  if (scientific) {
    return(format.pval(p, digits = digits))
  }


  # Round to specified decimal places
  formatted <- formatC(p, digits = digits, format = "f", decimal.mark = decimal.mark)

  # Remove leading zero if requested

  if (!leading.zero) {
    formatted <- sub("^0\\.", ".", formatted)
    formatted <- sub("^-0\\.", "-.", formatted)
  }

  formatted
}


#' Resolve P-Format Parameters
#'
#' @description Internal helper function to resolve p-value formatting parameters
#'   from style and individual overrides. Used by other functions to maintain
#'   consistent parameter handling.
#'
#' @param style Character string specifying the style.
#' @param digits Override for digits parameter.
#' @param leading.zero Override for leading.zero parameter.
#' @param min.threshold Override for min.threshold parameter.
#' @param decimal.mark Override for decimal.mark parameter.
#'
#' @return A list with resolved parameters: digits, leading.zero, min.threshold, use.scientific.
#'
#' @keywords internal
resolve_p_format_params <- function(style = "default",
                                    digits = NULL,
                                    leading.zero = NULL,
                                    min.threshold = NULL,
                                    decimal.mark = NULL) {
  style <- match.arg(style, names(.p_format_styles))
  style_settings <- .p_format_styles[[style]]

  list(
    digits = if (is.null(digits)) style_settings$digits else digits,
    leading.zero = if (is.null(leading.zero)) style_settings$leading.zero else leading.zero,
    min.threshold = if (is.null(min.threshold)) style_settings$min.threshold else min.threshold,
    decimal.mark = if (is.null(decimal.mark)) getOption("OutDec") else decimal.mark,
    use.scientific = style_settings$use.scientific
  )
}


#' Create P-Value Label with Proper Prefix
#'
#' @description Creates a p-value label string with proper handling of inequality
#'   symbols. When the formatted p-value starts with "<" or ">", uses "p <value"
#'   or "p >value" format. Otherwise uses "p = value" format.
#'
#' @param p.format Character string of the formatted p-value (e.g., "0.05", "< 0.001").
#' @param p.signif Optional character string of significance symbol (e.g., "*", "**", "ns").
#'   If provided, it will be appended after the p-value.
#'
#' @return A character string with the properly formatted p-value label.
#'
#' @examples
#' \dontrun{
#' create_p_label("0.05")
#' # Returns: "p = 0.05"
#'
#' create_p_label("< 0.001")
#' # Returns: "p < 0.001"
#'
#' create_p_label("0.01", "**")
#' # Returns: "p = 0.01 **"
#'
#' create_p_label("< 0.001", "****")
#' # Returns: "p < 0.001 ****"
#' }
#'
#' @export
create_p_label <- function(p.format, p.signif = NULL) {
  # Check if p.format starts with < or >
  has_inequality <- grepl("^[<>]", p.format)

  # Create base label
  label <- ifelse(has_inequality,
    paste("p", p.format),
    paste("p =", p.format)
  )

  # Normalize spacing around inequality symbols
  label <- gsub("([<>])\\s*", "\\1 ", label)

  # Append significance if provided
  if (!is.null(p.signif)) {
    label <- paste(label, p.signif)
    label <- trimws(label)
  }

  label
}
