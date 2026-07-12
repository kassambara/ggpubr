#' @include utilities.R
NULL
#' Volcano Plot of Differential Expression Results
#' @description Draws a publication-ready volcano plot from differential
#'   expression results: the log2 fold change on the x axis versus the
#'   \eqn{-\log_{10}} of the (adjusted) p-value on the y axis. Points are colored
#'   as up-regulated, down-regulated or non-significant using fold-change and
#'   significance thresholds, and the top hits are labeled. It mirrors the
#'   surface of \code{\link{ggmaplot}()}.
#' @param data a data frame (or matrix / \code{DESeqResults} etc.) of
#'   differential expression results. Must contain the fold-change column
#'   \code{x} and the p-value column \code{y}.
#' @param x the name of the log2 fold-change column. Default
#'   \code{"log2FoldChange"}.
#' @param y the name of the p-value column plotted (as \eqn{-\log_{10}}) on the y
#'   axis and used for significance. Default \code{"padj"}.
#' @param fdr the significance threshold applied to \code{y}. Default 0.05.
#' @param fc the fold-change threshold (on the natural scale; the dashed vertical
#'   lines are drawn at \eqn{\pm \log_2(fc)}). Default 1.5.
#' @param genenames a character vector of point labels of length
#'   \code{nrow(data)}. If \code{NULL}, the row names of \code{data} are used.
#' @param size the point size.
#' @param alpha the point transparency.
#' @param seed passed to the label repel functions for reproducible label
#'   placement.
#' @param font.label a vector specifying label font: size, style and color, e.g.
#'   \code{c(14, "bold", "red")}.
#' @param label.rectangle logical. If \code{TRUE}, labels are drawn inside a
#'   rectangle (\code{\link[ggrepel]{geom_label_repel}()}).
#' @param palette colors for up-regulated, down-regulated and non-significant
#'   points, in that order. Default \code{c("#B31B21", "#1465AC", "darkgray")}.
#' @param top the number of top hits to label. Default 15. Use \code{top = 0}
#'   with \code{label.select} to label only specific genes.
#' @param select.top.method the ranking used to pick the top hits, one of
#'   \code{"padj"} (default) or \code{"fc"}.
#' @param label.select a character vector of specific gene names to label.
#' @param facet.by character vector of grouping variable(s) for faceting; the top
#'   hits are then selected within each panel.
#' @param main the plot title.
#' @param xlab,ylab axis labels. If \code{ylab} is \code{NULL} (default), it is
#'   built from \code{y} (e.g. \code{"-Log10 padj"}).
#' @param line.color the color of the threshold lines. Default \code{"black"}.
#' @param ggtheme a ggplot theme. Default \code{\link[ggplot2]{theme_classic}()}.
#' @param ... other arguments passed to \code{\link{ggpar}()}.
#' @return a ggplot.
#' @seealso \code{\link{ggmaplot}}.
#' @examples
#' data(diff_express)
#'
#' ggvolcano(diff_express,
#'   fdr = 0.05, fc = 2, size = 0.6,
#'   genenames = as.vector(diff_express$name),
#'   top = 20
#' )
#'
#' # Label only specific genes
#' ggvolcano(diff_express,
#'   fdr = 0.05, fc = 2, size = 0.6,
#'   genenames = as.vector(diff_express$name),
#'   top = 0, label.select = c("BUB1", "CD83")
#' )
#'
#' @export
ggvolcano <- function(data, x = "log2FoldChange", y = "padj",
                      fdr = 0.05, fc = 1.5, genenames = NULL,
                      size = NULL, alpha = 1, seed = 42,
                      font.label = c(12, "plain", "black"), label.rectangle = FALSE,
                      palette = c("#B31B21", "#1465AC", "darkgray"),
                      top = 15, select.top.method = c("padj", "fc"),
                      label.select = NULL, facet.by = NULL,
                      main = NULL, xlab = "Log2 fold change", ylab = NULL,
                      line.color = "black",
                      ggtheme = theme_classic(), ...) {
  if (!base::inherits(data, c("matrix", "data.frame", "DataFrame", "DE_Results", "DESeqResults"))) {
    stop("data must be an object of class matrix, data.frame, DataFrame, DE_Results or DESeqResults")
  }
  data <- as.data.frame(data)
  ss <- base::setdiff(c(x, y), colnames(data))
  if (length(ss) > 0) {
    stop("The colnames of data must contain: ", paste(ss, collapse = ", "))
  }
  if (is.null(ylab)) ylab <- paste0("-Log10 ", y)

  if (is.null(genenames)) {
    genenames <- rownames(data)
  } else if (length(genenames) != nrow(data)) {
    stop("genenames should be of length nrow(data).")
  }

  # Capture faceting column(s) before rebuilding the data frame, so the top hits
  # can be selected per panel (mirrors ggmaplot()).
  if (!is.null(facet.by)) {
    facet.by <- as.character(facet.by)
    miss <- base::setdiff(facet.by, colnames(data))
    if (length(miss) > 0) {
      stop("facet.by column(s) not found in data: ", paste(miss, collapse = ", "))
    }
    facet_data <- data[, facet.by, drop = FALSE]
  } else {
    facet_data <- NULL
  }

  lfc <- data[[x]]
  pval <- data[[y]]

  # Drop rows that cannot be plotted (missing fold change or p-value), keeping
  # genenames and the faceting column(s) aligned, so the Up/Down/NS counts
  # reflect the points actually drawn.
  keep <- !is.na(lfc) & !is.na(pval)
  if (!all(keep)) {
    data <- data[keep, , drop = FALSE]
    genenames <- genenames[keep]
    if (!is.null(facet_data)) facet_data <- facet_data[keep, , drop = FALSE]
    lfc <- lfc[keep]
    pval <- pval[keep]
  }

  sig <- rep(3, nrow(data))
  sig[which(pval <= fdr & lfc < 0 & abs(lfc) >= log2(fc))] <- 2
  sig[which(pval <= fdr & lfc > 0 & abs(lfc) >= log2(fc))] <- 1

  # y coordinate: -log10(p). A p-value of exactly 0 gives Inf, which ggplot2
  # would drop (losing the most significant hits), so cap it at the largest
  # finite value.
  logp <- -log10(pval)
  finite.logp <- logp[is.finite(logp)]
  if (length(finite.logp) > 0) {
    logp[is.infinite(logp)] <- max(finite.logp)
  }

  data <- data.frame(
    name = genenames, lfc = lfc, padj = pval, logp = logp, sig = sig
  )
  if (!is.null(facet_data)) data <- cbind(data, facet_data)

  # Level labels (Up/Down/NS), coloured by palette.
  . <- NULL
  data$sig <- as.factor(data$sig)
  .lev <- .levels(data$sig) %>% as.numeric()
  palette <- palette[.lev]
  if (is.null(facet.by)) {
    new.levels <- c(
      paste0("Up: ", sum(sig == 1)),
      paste0("Down: ", sum(sig == 2)),
      "NS"
    ) %>% .[.lev]
  } else {
    new.levels <- c("Up", "Down", "NS") %>% .[.lev]
  }
  data$sig <- factor(data$sig, labels = new.levels)

  # Order for selecting the top hits.
  select.top.method <- match.arg(select.top.method)
  if (select.top.method == "padj") {
    data <- data[order(data$padj), ]
  } else if (select.top.method == "fc") {
    data <- data[order(abs(data$lfc), decreasing = TRUE), ]
  }
  complete_data <- stats::na.omit(data)
  labs_data <- subset(complete_data, padj <= fdr & name != "" & abs(lfc) >= log2(fc))
  if (is.null(facet.by)) {
    labs_data <- utils::head(labs_data, top)
  } else {
    labs_data <- labs_data %>%
      dplyr::group_by(dplyr::across(dplyr::all_of(facet.by))) %>%
      dplyr::slice_head(n = top) %>%
      dplyr::ungroup() %>%
      as.data.frame()
  }
  if (!is.null(label.select)) {
    selected_labels <- complete_data %>%
      subset(complete_data$name %in% label.select, drop = FALSE)
    labs_data <- dplyr::bind_rows(labs_data, selected_labels) %>%
      dplyr::distinct(.data$name, .keep_all = TRUE)
  }

  font.label <- .parse_font(font.label)
  font.label$size <- ifelse(is.null(font.label$size), 12, font.label$size)
  font.label$color <- ifelse(is.null(font.label$color), "black", font.label$color)
  font.label$face <- ifelse(is.null(font.label$face), "plain", font.label$face)

  # Draw NS points first (behind) so significant hits stay visible (#365).
  is.ns <- as.character(data$sig) == "NS"
  data <- data[order(!is.ns), ]

  lfc <- logp <- sig <- name <- padj <- NULL
  p <- ggplot(data, aes(x = lfc, y = logp))
  if (is.null(size)) {
    p <- p + geom_point(aes(color = sig), alpha = alpha)
  } else {
    p <- p + geom_point(aes(color = sig), size = size, alpha = alpha)
  }

  max.overlaps <- getOption("ggrepel.max.overlaps", default = Inf)
  repel.fun <- if (label.rectangle) ggrepel::geom_label_repel else ggrepel::geom_text_repel
  p <- p + repel.fun(
    data = labs_data, mapping = aes(label = name),
    box.padding = unit(0.35, "lines"),
    point.padding = unit(0.3, "lines"),
    force = 1, seed = seed, fontface = font.label$face,
    size = font.label$size / 3, color = font.label$color,
    max.overlaps = max.overlaps
  )

  p <- p +
    labs(x = xlab, y = ylab, title = main, color = "") +
    geom_vline(xintercept = c(-log2(fc), log2(fc)), linetype = 2, color = line.color) +
    geom_hline(yintercept = -log10(fdr), linetype = 2, color = line.color)

  p <- ggpar(p, palette = palette, ggtheme = ggtheme, ...)
  if (!is.null(facet.by)) p <- facet(p, facet.by = facet.by)
  p
}
