# check symnum.args --------------------------------
fortify_signif_symbols_encoding <- function(symnum.args = list()) {
  if (.is_empty(symnum.args)) {
    symnum.args <- list(
      cutpoints = c(0, 1e-04, 0.001, 0.01, 0.05, Inf),
      symbols = c("****", "***", "**", "*", "ns")
    )
  } else {
    symnum.args.isok <- (length(symnum.args$symbols) == length(symnum.args$cutpoints) - 1)
    if (!symnum.args.isok) {
      stop(
        "Incorrect format detected in 'symnum.args'. ",
        "Check the documentation. ",
        "length(symbols) should be length(cutpoints)-1",
        call. = FALSE
      )
    }
  }
  symnum.args
}


#' Build symnum.args from Significance Parameters
#'
#' @description Internal helper function to build a symnum.args list from
#'   user-friendly significance parameters. This allows users to specify
#'   custom significance cutoffs and symbols without using the complex
#'   symnum.args format directly.
#'
#' @param signif.cutoffs Numeric vector of p-value cutoffs in descending order
#'   (e.g., \code{c(0.10, 0.05, 0.01)} or \code{c(0.10, 0.05, 0.01, 0.001)}).
#'   Values smaller than each cutoff receive the corresponding symbol.
#' @param signif.symbols Character vector of symbols matching signif.cutoffs.
#'   If NULL, auto-generated based on length: 3 cutoffs -> c("*", "**", "***"),
#'   4 cutoffs -> c("*", "**", "***", "****").
#' @param ns.symbol Character string for non-significant results. Default is "ns".
#'   Use "" (empty string) to show nothing for non-significant results.
#' @param use.four.stars Logical. If TRUE and signif.symbols is NULL, include
#'   four stars (****) for the most significant level. Default is FALSE.
#' @param symnum.args Existing symnum.args list. If provided and non-empty,
#'   it takes precedence over other parameters (for backward compatibility).
#'
#' @return A list suitable for use as symnum.args parameter.
#'
#' @details
#' Priority order:
#' 1. If symnum.args is provided (non-empty), use it directly
#' 2. If signif.cutoffs is provided, build symnum.args from it
#' 3. Otherwise, use package defaults
#'
#' @keywords internal
build_symnum_args <- function(signif.cutoffs = NULL,
                              signif.symbols = NULL,
                              ns.symbol = "ns",
                              use.four.stars = FALSE,
                              symnum.args = list()) {
  # Priority 1: If symnum.args is provided, use it directly (backward compatibility)
  if (!.is_empty(symnum.args)) {
    return(fortify_signif_symbols_encoding(symnum.args))
  }

  # Priority 2: If signif.cutoffs is provided, build from it
  if (!is.null(signif.cutoffs)) {
    # Validate cutoffs
    if (!is.numeric(signif.cutoffs) || any(signif.cutoffs <= 0) || any(signif.cutoffs >= 1)) {
      stop("signif.cutoffs must be numeric values between 0 and 1 (exclusive)", call. = FALSE)
    }

    # Sort cutoffs in descending order (largest to smallest)
    signif.cutoffs <- sort(signif.cutoffs, decreasing = TRUE)
    n_cutoffs <- length(signif.cutoffs)

    # Auto-generate symbols if not provided
    if (is.null(signif.symbols)) {
      # Generate symbols: *, **, ***, (****)
      if (use.four.stars && n_cutoffs >= 4) {
        signif.symbols <- vapply(
          seq_len(n_cutoffs),
          function(i) paste(rep("*", i), collapse = ""),
          FUN.VALUE = character(1)
        )
      } else if (n_cutoffs > 3 && !use.four.stars) {
        stop("signif.cutoffs has more than 3 levels but use.four.stars = FALSE. ",
          "Either set use.four.stars = TRUE or provide signif.symbols explicitly.",
          call. = FALSE
        )
      } else {
        signif.symbols <- vapply(
          seq_len(n_cutoffs),
          function(i) paste(rep("*", i), collapse = ""),
          FUN.VALUE = character(1)
        )
      }
    }

    # Validate symbols length
    if (length(signif.symbols) != n_cutoffs) {
      stop("signif.symbols must have the same length as signif.cutoffs (", n_cutoffs, ")",
        call. = FALSE
      )
    }

    # Build cutpoints: 0, cutoffs (ascending), Inf
    cutpoints <- c(0, rev(signif.cutoffs), Inf)

    # Build symbols: most significant first, then less significant, then ns
    symbols <- c(rev(signif.symbols), ns.symbol)

    return(list(cutpoints = cutpoints, symbols = symbols))
  }

  # Priority 3: Use package defaults, but respect ns.symbol if customized
  default_args <- fortify_signif_symbols_encoding(list())

  # If ns.symbol is different from default, update it
  if (ns.symbol != "ns") {
    # Replace the last symbol (ns) with the custom one
    default_args$symbols[length(default_args$symbols)] <- ns.symbol
  }

  default_args
}

# Check user specified label -----------------------------

# Check if is glue package expression
is_glue_expression <- function(label) {
  grepl("\\{|\\}", label, perl = TRUE)
}

# Check if label is a plotmath expression
contains_plotmath_symbols <- function(label) {
  grepl("==|italic\\s?\\(|bold\\s?\\(|bolditalic\\s?\\(", label)
}
starts_with_list <- function(label) {
  grepl("^list\\s?\\(", label)
}
is_plotmath_expression <- function(label) {
  starts_with_list(label) | contains_plotmath_symbols(label)
}

# Fortify label --------------------
# if label is plotmath expression, then
# fortify it in case users miss something
contains_p_signif <- function(label) {
  any(grepl("p*\\.signif", label))
}
contains_twoequal_signs <- function(label) {
  any(grepl("==", label))
}
replace_simple_by_double_equals <- function(label) {
  if (!contains_twoequal_signs(label)) {
    label <- gsub("=", "==", label)
  }
  label
}

escape_psignif_asteriks <- function(label) {
  # Escaping asteriks (special plotmath symbols) in p.signif or p.adj.signif by adding quote
  label <- gsub(pattern = "\\}\\{(p.*.signif)\\}", replacement = "}*`{\\1}`", x = label)
  # p signif preceded with space
  label <- gsub(pattern = "~\\{(p.*.signif)\\}", replacement = "~`{\\1}`", x = label)
  label <- gsub(pattern = "=+?\\s+?\\{(p.*.signif)}", replacement = "== `{\\1}`", x = label)
  label
}


# Get statistical test label to be displayed -------------------
# stat.test: statistical test output
# description: the description of the stat test, for example: "Anova"
# label: can be p, p.signif, p.adj or glue expression
add_stat_label <- function(stat.test, label = NULL) {
  is_plotmath <- FALSE
  stat.test <- add_p_format_signif(stat.test)
  if (is.null(label)) {
    stat.test$label <- add_p(stat.test$p.format)
  } else {
    is_plotmath <- is_plotmath_expression(label)
    if (is_plotmath) {
      label <- fortify_plotmath(label)
    }
    if (is_glue_expression(label)) {
      stat.test <- stat.test %>% mutate(label = glue(label))
    } else {
      if (!(label %in% colnames(stat.test))) {
        stop(
          "Can't find the value of the argument label ('", label, "') in the computed ",
          "statistical test table.",
          call. = FALSE
        )
      }
      stat.test$label <- as.character(stat.test[[label]])
    }
  }
  label <- gsub(pattern = "=+(\\s+)?<", replacement = "<\\1", stat.test$label)
  if (is_plotmath) {
    label <- replace_simple_by_double_equals(label)
    label <- gsub(pattern = "\\s", replacement = "~", label)
    label <- gsub(pattern = "~==~", replacement = "~`=`~", label)
    label <- gsub(pattern = "~<~", replacement = "~`<`~", label)
    # Make sure that decimal values will be displayed asis in character when parsed by ggplot
    # Add quote around numeric values
    label <- gsub("([0-9.-]+)", "'\\1'", label)
    # Escape minus in text
    label <- gsub("Kruskal'-'Wallis", "'Kruskal-Wallis'", label)
  }
  stat.test$label <- label
  stat.test
}

fortify_plotmath <- function(label) {
  label <- gsub(pattern = "~", replacement = " ", x = label, fixed = TRUE)
  if (!starts_with_list(label)) label <- paste0("list(", label, ")")
  if (contains_p_signif(label)) {
    # Escape p signif stars
    label <- gsub(pattern = "\\}\\{p.signif\\}", replacement = "}*`{p.signif}`", x = label)
    label <- gsub(pattern = "\\}\\{p.adj.signif\\}", replacement = "}*`{p.adj.signif}`", x = label)
    label <- gsub(pattern = "\\}\\{p.format.signif\\}", replacement = "}*`{p.format.signif}`", x = label)
    # Escape p signif stars preceded by space
    label <- gsub(pattern = "\\s\\{p.signif\\}", replacement = " `{p.signif}`", x = label)
    label <- gsub(pattern = "\\s\\{p.adj.signif\\}", replacement = " `{p.adj.signif}`", x = label)
    label <- gsub(pattern = "\\s\\{p.format.signif\\}", replacement = " `{p.format.signif}`", x = label)
    # Escape p signif stars preceded by equal signs
    label <- gsub(pattern = "=(\\s+)?\\{p.signif}", replacement = "=\\1`{p.signif}`", x = label)
    label <- gsub(pattern = "=(\\s+)?\\{p.adj.signif}", replacement = "=\\1`{p.adj.signif}`", x = label)
    label <- gsub(pattern = "=(\\s+)?\\{p.format.signif}", replacement = "=\\1`{p.format.signif}`", x = label)
  }
  label <- gsub(pattern = "eta2[g]", replacement = "eta[g]^2", x = label, fixed = TRUE)
  label <- gsub(pattern = "eta2[p]", replacement = "eta[p]^2", x = label, fixed = TRUE)
  label
}

# Add p prefix
# add_p(0.05) --> p = 0.05
# add_p("<0.05") --> p < 0.05
add_p <- function(label) {
  create_p_label(label)
}

# Add combined p.format + p.signif column when available
# Falls back to adjusted values when raw p-values are missing.
add_p_format_signif <- function(stat.test) {
  if ("p.format.signif" %in% colnames(stat.test)) {
    return(stat.test)
  }

  has_p <- all(c("p.format", "p.signif") %in% colnames(stat.test))
  has_adj <- all(c("p.adj.format", "p.adj.signif") %in% colnames(stat.test))
  if (!has_p && !has_adj) {
    return(stat.test)
  }

  if (has_p) {
    p_format <- stat.test$p.format
    p_signif <- stat.test$p.signif
  } else {
    p_format <- rep(NA_character_, nrow(stat.test))
    p_signif <- rep(NA_character_, nrow(stat.test))
  }

  if (has_adj) {
    use_adj <- is.na(p_format) | p_format == ""
    p_format[use_adj] <- stat.test$p.adj.format[use_adj]
    p_signif[use_adj] <- stat.test$p.adj.signif[use_adj]
  }

  if (all(is.na(p_format))) {
    return(stat.test)
  }

  p_signif[is.na(p_signif)] <- ""
  combined <- rep(NA_character_, length(p_format))
  ok <- !is.na(p_format)
  combined[ok] <- create_p_label(p_format[ok], p_signif[ok])
  stat.test[["p.format.signif"]] <- combined
  stat.test
}

# Add statistical test number of samples
add_stat_n <- function(stat.test) {
  stat.test$n <- rstatix::get_n(stat.test)
  stat.test
}


# gg layer data checking --------------------------
# Check whether there is multiple grouping variables
# This is the case for grouped plots
contains_multiple_grouping_vars <- function(data) {
  !all(data$x == data$group)
}

# Check if group variable is specified in aes
is_group_aes_specified <- function(mapping) {
  answer <- FALSE
  if (is.null(mapping)) {
    answer <- FALSE
  } else if (!is.null(mapping$group)) {
    answer <- TRUE
  }
  answer
}


# Manipulating statistical test outputs -----------------
keep_only_tbl_df_classes <- function(x) {
  toremove <- setdiff(class(x), c("tbl_df", "tbl", "data.frame"))
  if (length(toremove) > 0) {
    class(x) <- setdiff(class(x), toremove)
  }
  x
}


# The dot-dot notation (`..p.signif..`) was deprecated in ggplot2 3.4.0.
# after_stat(p.signif) should be used. This function makes automatic
# conversion if user specified ..p.signif..
convert_label_dotdot_notation_to_after_stat <- function(mapping) {
  if (!is.null(mapping)) {
    label <- mapping$label
    if (!is.null(mapping$label)) {
      label <- rlang::as_label(mapping$label)
      dot_dot_labels <- c(
        "p.signif", "p.adj.signif", "p.format", "p.format.signif", "p", "p.adj",
        "eq.label", "adj.rr.label", "rr.label", "AIC.label", "BIC.label"
      )
      for (dot_dot_label in dot_dot_labels) {
        # Escape dots in the label name for regex matching
        escaped_label <- gsub("\\.", "\\\\.", dot_dot_label)
        # Use word boundaries that ensure the pattern is not part of a larger identifier
        # (?<![a-zA-Z0-9_.]) ensures no alphanumeric, underscore, or dot before
        # (?![a-zA-Z0-9_.]) ensures no alphanumeric, underscore, or dot after
        label <- gsub(
          pattern = paste0("(?<![a-zA-Z0-9_.])\\.\\.", escaped_label, "\\.\\.", "(?![a-zA-Z0-9_.])"),
          replacement = paste0("ggplot2::after_stat(", dot_dot_label, ")"),
          x = label,
          perl = TRUE
        )
      }
      # Only qualify unqualified after_stat() calls using negative lookbehind
      # This correctly handles mixed qualified and unqualified cases
      # replace after_stat() with ggplot2::after_stat()
      label <- gsub(
        pattern = "(?<!ggplot2::)after_stat\\s*\\(",
        replacement = "ggplot2::after_stat(",
        x = label,
        perl = TRUE
      )

      mapping$label <- parse(text = label)[[1]]
    }
  }
  mapping
}
