#' @include utilities.R ggpar.R
NULL
#' ROC Curve
#' @description Draw a publication-ready Receiver Operating Characteristic (ROC)
#'   curve, with the area under the curve (AUC) and its confidence interval
#'   computed and annotated on the plot. One or several predictors can be drawn
#'   on the same axes to compare models.
#'
#'   Everything is computed in base R -- the empirical ROC, the AUC (the
#'   Mann-Whitney statistic), and a deterministic large-sample confidence
#'   interval (Hanley and McNeil, 1982) -- so \code{ggrocplot()} has no modeling
#'   dependency and runs in lightweight environments (e.g. WebR). The result is a
#'   standard \code{ggplot} you can further customize with \code{\link{ggpar}}.
#'
#' @inheritParams ggboxplot
#' @param data a data frame.
#' @param response the name of the outcome column. It must have exactly two
#'   classes. The \strong{positive (event) class} is the second level of a factor
#'   or, otherwise, the larger of the two sorted values; the other class is the
#'   negative one. Set the factor levels explicitly to control this.
#' @param predictor the name of one or more numeric predictor columns (a marker
#'   or a fitted probability). A \strong{higher} predictor value is taken to
#'   indicate the positive class. Passing several columns overlays one curve per
#'   predictor for model comparison.
#' @param color line color, used when a single \code{predictor} is drawn. Ignored
#'   when several predictors are compared (they are colored by predictor).
#' @param palette the color palette to be used when several predictors are
#'   compared. Allowed values include "grey" for grey color palettes; brewer
#'   palettes e.g. "RdBu", "Blues"; or custom color palette e.g. c("blue",
#'   "red"); and scientific journal palettes from ggsci R package, e.g. "npg",
#'   "aaas", "lancet", "jco", "ucscgb", "uchicago", "simpsons" and "rickandmorty".
#' @param linetype line type.
#' @param size line size.
#' @param print.auc logical. If \code{TRUE} (default) the AUC is annotated on the
#'   plot (single predictor) or appended to the legend labels (several predictors).
#' @param ci logical. If \code{TRUE} (default) a confidence interval for the AUC
#'   is computed (Hanley-McNeil) and shown next to the AUC.
#' @param conf.level the confidence level of the AUC interval. Default is 0.95.
#' @param diag logical. If \code{TRUE} (default) the diagonal no-discrimination
#'   reference line is drawn.
#' @param diag.color,diag.linetype color and line type of the diagonal reference.
#' @param legend the legend position. Allowed values: "top", "bottom", "left",
#'   "right" or "none". Passed to \code{\link{ggpar}}.
#' @param legend.title the legend title used when several predictors are compared.
#' @param ... other arguments to be passed to \code{\link{ggpar}}.
#' @details The AUC is the Mann-Whitney U statistic scaled to \[0, 1\]: the
#'   probability that a randomly chosen positive case scores higher than a
#'   randomly chosen negative one (ties count as 1/2). Its confidence interval
#'   uses the closed-form Hanley-McNeil (1982) standard error, so the output is
#'   deterministic (no bootstrap) and byte-stable across runs. This large-sample
#'   interval is close to, but not identical with, the DeLong interval reported
#'   by \code{pROC::ci.auc()}; for DeLong or bootstrap intervals, or for ROC
#'   smoothing and statistical tests between curves, use the \code{pROC} package.
#' @references Hanley JA, McNeil BJ (1982). The meaning and use of the area under
#'   a receiver operating characteristic (ROC) curve. Radiology, 143(1), 29-36.
#' @seealso \code{\link{ggpar}}, \code{\link{ggline}}.
#'
#' @return a ggplot.
#' @examples
#' # Example data: two markers of a binary outcome
#' set.seed(123)
#' n <- 60
#' df <- data.frame(
#'   outcome = factor(
#'     rep(c("control", "case"), each = n),
#'     levels = c("control", "case") # "case" = positive class
#'   ),
#'   marker1 = c(rnorm(n, 0), rnorm(n, 1.2)),
#'   marker2 = c(rnorm(n, 0), rnorm(n, 0.6))
#' )
#'
#' # Single ROC curve with AUC + 95% CI annotated
#' ggrocplot(df, response = "outcome", predictor = "marker1")
#'
#' # Compare two markers on the same axes
#' ggrocplot(
#'   df,
#'   response = "outcome",
#'   predictor = c("marker1", "marker2"),
#'   palette = "jco"
#' )
#'
#' @export
ggrocplot <- function(data, response, predictor,
                      color = NULL, palette = NULL,
                      linetype = "solid", size = NULL,
                      print.auc = TRUE, ci = TRUE, conf.level = 0.95,
                      diag = TRUE, diag.color = "grey60", diag.linetype = "dashed",
                      title = NULL, xlab = "1 - Specificity", ylab = "Sensitivity",
                      legend = "right", legend.title = "Model",
                      ggtheme = theme_pubr(),
                      ...) {
  if (missing(data)) stop("'data' is required.")
  if (missing(response)) stop("'response' is required.")
  if (missing(predictor)) stop("'predictor' is required.")
  .check_roc_columns(data, response, predictor)

  roc <- .compute_roc(data, response, predictor, conf.level = conf.level)
  curve <- roc$curve
  summ <- roc$summary
  multi <- nrow(summ) > 1

  # Legend labels: append the AUC (+ CI) to each model when several are compared.
  if (multi) {
    labels <- .auc_label(summ$auc, summ$lo, summ$hi, ci = ci, prefix = paste0(summ$model, "  "))
    if (!print.auc) labels <- summ$model
    curve$.model <- factor(curve$.model, levels = summ$model, labels = labels)
  }

  if (multi) {
    p <- ggplot(curve, aes(x = .data[["fpr"]], y = .data[["tpr"]], color = .data[[".model"]]))
  } else {
    p <- ggplot(curve, aes(x = .data[["fpr"]], y = .data[["tpr"]]))
  }

  # Diagonal no-discrimination reference (drawn first, under the curve).
  if (diag) {
    p <- p + geom_abline(
      slope = 1, intercept = 0,
      color = diag.color, linetype = diag.linetype
    )
  }

  line.args <- list(linewidth = size %||% 0.8, linetype = linetype)
  if (!multi) line.args$color <- color %||% "#2E9FDF"
  p <- p + do.call(geom_line, line.args)

  # AUC annotation for a single curve (multi-curve carries it in the legend).
  if (print.auc && !multi) {
    p <- p + annotate(
      "text", x = 0.98, y = 0.02, hjust = 1, vjust = 0,
      label = .auc_label(summ$auc, summ$lo, summ$hi, ci = ci)
    )
  }

  p <- p +
    scale_x_continuous(limits = c(0, 1)) +
    scale_y_continuous(limits = c(0, 1))

  # The color aesthetic (and its legend title) only exists when several
  # predictors are compared; setting it for a single curve warns.
  if (multi) {
    p <- p + labs(color = legend.title)
    p <- ggpar(
      p, palette = palette, ggtheme = ggtheme,
      title = title, xlab = xlab, ylab = ylab,
      legend = legend, legend.title = legend.title, ...
    )
  } else {
    p <- ggpar(
      p, palette = palette, ggtheme = ggtheme,
      title = title, xlab = xlab, ylab = ylab,
      legend = legend, ...
    )
  }

  # Keep the ROC panel square via the theme rather than coord_fixed(), so a user
  # xlim/ylim passed through ggpar() still zooms instead of colliding with the
  # coordinate system. Applied after ggpar() so a complete ggtheme cannot drop it.
  p <- p + theme(aspect.ratio = 1)
  p
}


# Helpers
# :::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::

# Validate the requested columns exist and are usable.
.check_roc_columns <- function(data, response, predictor) {
  miss <- setdiff(c(response, predictor), colnames(data))
  if (length(miss) > 0) {
    stop("Column(s) not found in the data: ", paste(miss, collapse = ", "), call. = FALSE)
  }
  resp <- data[[response]]
  n.class <- length(unique(stats::na.omit(resp)))
  if (n.class != 2) {
    stop("'response' must have exactly two classes; found ", n.class, ".", call. = FALSE)
  }
  invisible(TRUE)
}

# Coerce the response to 0/1 with the positive (event) class = 1.
# Positive = 2nd factor level, else the larger of the two sorted unique values.
.roc_binarize <- function(response) {
  if (is.factor(response) || is.character(response)) {
    lev <- if (is.factor(response)) levels(response) else sort(unique(response))
    lev <- lev[lev %in% unique(stats::na.omit(response))]
  } else {
    lev <- sort(unique(stats::na.omit(response)))
  }
  positive <- lev[length(lev)]
  as.integer(response == positive)
}

# AUC = Mann-Whitney U / (n_pos * n_neg); ties contribute 1/2 (rank method).
.auc_mannwhitney <- function(d01, score) {
  pos <- score[d01 == 1]
  neg <- score[d01 == 0]
  n1 <- length(pos)
  n0 <- length(neg)
  r <- rank(c(pos, neg))
  U <- sum(r[seq_len(n1)]) - n1 * (n1 + 1) / 2
  U / (n1 * n0)
}

# Hanley-McNeil (1982) closed-form CI for the AUC.
.auc_ci_hanley <- function(auc, n1, n0, conf.level = 0.95) {
  q1 <- auc / (2 - auc)
  q2 <- 2 * auc^2 / (1 + auc)
  se <- sqrt((auc * (1 - auc) +
    (n1 - 1) * (q1 - auc^2) +
    (n0 - 1) * (q2 - auc^2)) / (n1 * n0))
  z <- stats::qnorm(1 - (1 - conf.level) / 2)
  c(lo = max(0, auc - z * se), hi = min(1, auc + z * se))
}

# Empirical ROC operating points (fpr, tpr), tie-collapsed, with (0,0) prepended.
.roc_points <- function(d01, score) {
  o <- order(score, decreasing = TRUE)
  d <- d01[o]
  s <- score[o]
  n1 <- sum(d01 == 1)
  n0 <- sum(d01 == 0)
  tp <- cumsum(d == 1)
  fp <- cumsum(d == 0)
  keep <- c(s[-length(s)] != s[-1], TRUE) # last index of each tied score run
  data.frame(
    fpr = c(0, fp[keep] / n0),
    tpr = c(0, tp[keep] / n1)
  )
}

# Build the curve (long) + per-predictor AUC summary.
.compute_roc <- function(data, response, predictor, conf.level = 0.95) {
  curve.list <- list()
  summary.list <- list()
  for (pv in predictor) {
    ok <- !is.na(data[[response]]) & !is.na(data[[pv]])
    d01 <- .roc_binarize(data[[response]][ok])
    score <- as.numeric(data[[pv]][ok])
    n1 <- sum(d01 == 1)
    n0 <- sum(d01 == 0)
    auc <- .auc_mannwhitney(d01, score)
    ci <- .auc_ci_hanley(auc, n1, n0, conf.level)
    pts <- .roc_points(d01, score)
    pts$.model <- pv
    curve.list[[pv]] <- pts
    summary.list[[pv]] <- data.frame(
      model = pv, auc = auc, lo = ci[["lo"]], hi = ci[["hi"]],
      stringsAsFactors = FALSE
    )
  }
  list(
    curve = do.call(rbind, curve.list),
    summary = do.call(rbind, summary.list)
  )
}

# Format an "AUC = 0.84 [0.79, 0.89]" style label.
.auc_label <- function(auc, lo, hi, ci = TRUE, prefix = "") {
  if (ci) {
    sprintf("%sAUC = %.2f [%.2f, %.2f]", prefix, auc, lo, hi)
  } else {
    sprintf("%sAUC = %.2f", prefix, auc)
  }
}
