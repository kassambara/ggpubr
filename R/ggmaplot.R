#' @include utilities.R ggpar.R
NULL
#' MA-plot from means and log fold changes
#' @description Make MA-plot which is a scatter plot of log2 fold changes (on
#'   the y-axis) versus the mean expression signal (on the x-axis).
#' @inheritParams ggboxplot
#' @inheritParams ggpar
#' @param data an object of class DESeqResults, get_diff, DE_Results, matrix or
#'   data frame containing the columns baseMean, log2FoldChange, and padj. Rows
#'   are genes. \itemize{ \item baseMean: the mean expression of genes in the
#'   two groups. \item log2FoldChange: the log2 fold changes of group 2 compared
#'   to group 1 \item padj: the adjusted p-value of the used statiscal test. }
#' @param fdr Accepted false discovery rate for considering genes as
#'   differentially expressed.
#' @param fc the fold change threshold. Only genes with a fold change >= fc and
#'   padj <= fdr are considered as significantly differentially expressed.
#' @param genenames a character vector of length nrow(data) specifying gene
#'   names corresponding to each row. Used for point labels.
#' @param detection_call a numeric vector with length = nrow(data), specifying
#'   if the genes is expressed (value = 1) or not (value = 0). For example
#'   detection_call = c(1, 1, 0, 1, 0, 1). Default is NULL. If detection_call
#'   column is available in data, it will be used.
#' @param size points size.
#' @param font.label a vector of length 3 indicating respectively the size
#'   (e.g.: 14), the style (e.g.: "plain", "bold", "italic", "bold.italic") and
#'   the color (e.g.: "red") of point labels. For example \emph{font.label =
#'   c(14, "bold", "red")}.
#' @param label.rectangle logical value. If TRUE, add rectangle underneath the
#'   text, making it easier to read.
#' @param top the number of top genes to be shown on the plot.
#' @param select.top.method methods to be used for selecting top genes. Allowed
#'   values include "padj" and "fc" for selecting by adjusted p values or fold
#'   changes, respectively.
#' @param ... other arguments to be passed to \code{\link{ggpar}}.
#' @return returns a ggplot.
#' @examples
#' data(diff_express)
#'
#' # Default plot
#'ggmaplot(diff_express, main = expression("Group 1" %->% "Group 2"),
#'    fdr = 0.05, fc = 2, size = 0.4,
#'    palette = c("#B31B21", "#1465AC", "darkgray"),
#'    genenames = as.vector(diff_express$name),
#'    legend = "top", top = 20,
#'    font.label = c("bold", 11),
#'    font.legend = "bold",
#'    font.main = "bold",
#'    ggtheme = ggplot2::theme_minimal())
#'
#' # Add rectangle around labels
#'ggmaplot(diff_express, main = expression("Group 1" %->% "Group 2"),
#'    fdr = 0.05, fc = 2, size = 0.4,
#'    palette = c("#B31B21", "#1465AC", "darkgray"),
#'    genenames = as.vector(diff_express$name),
#'    legend = "top", top = 20,
#'    font.label = c("bold", 11), label.rectangle = TRUE,
#'    font.legend = "bold",
#'    font.main = "bold",
#'    ggtheme = ggplot2::theme_minimal())
#' @export
ggmaplot <- function (data, fdr = 0.05, fc = 1.5, genenames = NULL,
                     detection_call = NULL, size = 0.9,
                     font.label = c(12, "plain", "black"), label.rectangle = FALSE,
                     palette = c("#B31B21", "#1465AC", "darkgray"),
                     top = 15, select.top.method = c("padj", "fc"),
                     main = NULL, xlab = "Log2 mean expression",  ylab = "Log2 fold change",
                     ggtheme = theme_pubr(),...)
{

  if(!base::inherits(data, c("matrix", "data.frame", "DataFrame", "DE_Results", "DESeqResults")))
    stop("data must be an object of class matrix, data.frame, DataFrame, DE_Results or DESeqResults")
  if(!is.null(detection_call)){
    if(nrow(data)!=length(detection_call))
      stop("detection_call must be a numeric vector of length = nrow(data)")
  }
  else if("detection_call" %in% colnames(data)){
    detection_call <- as.vector(data$detection_call)
  }
  else detection_call = rep(1, nrow(data))

  # Legend position
  if(is.null(list(...)$legend)) legend <- c(0.12, 0.9)

  # Check data format
  ss <- base::setdiff(c("baseMean", "log2FoldChange", "padj"), colnames(data))
  if(length(ss)>0) stop("The colnames of data must contain: ",
                        paste(ss, collapse = ", "))

  if(is.null(genenames)) genenames <- rownames(data)
  else if(length(genenames)!=nrow(data))
  stop("genenames should be of length nrow(data).")

  sig <- rep(3, nrow(data))
  sig[which(data$padj <= fdr & data$log2FoldChange < 0 & abs(data$log2FoldChange) >= log2(fc) & detection_call ==1)] = 2
  sig[which(data$padj <= fdr & data$log2FoldChange > 0 & abs(data$log2FoldChange) >= log2(fc) & detection_call ==1)] = 1
  data <- data.frame(name = genenames, mean = data$baseMean, lfc = data$log2FoldChange,
                  padj = data$padj, sig = sig)

  data$sig <- factor(sig,
                   labels = c( paste0("Up: ", sum(sig ==1)),
                               paste0("Down: ", sum(sig==2)),
                               "NS"
                   ))

  # Ordering for selecting top gene
  select.top.method <- match.arg(select.top.method)
  if(select.top.method == "padj") data <- data[order(data$padj), ]
  else if(select.top.method == "fc") data <- data[order(abs(data$lfc), decreasing = TRUE), ]
  # select data for top genes
  labs_data <- stats::na.omit(data)
  labs_data <- subset(labs_data, padj <= 0.05 & name!="")
  labs_data <- head(labs_data, top)

  font.label <- .parse_font(font.label)
  font.label$size <- ifelse(is.null(font.label$size), 12, font.label$size)
  font.label$color <- ifelse(is.null(font.label$color), "black", font.label$color)
  font.label$face <- ifelse(is.null(font.label$face), "plain", font.label$face)

  # Plot
  set.seed(42)
  mean <- lfc <- sig <- name <- padj <-  NULL
  p <- ggplot(data, aes(x = log2(mean+1), y = lfc)) +
    geom_point(aes(color = sig), size = size)

  if(label.rectangle){
    p <- p + ggrepel::geom_label_repel(data = labs_data, mapping = aes(label = name),
                                      box.padding = unit(0.35, "lines"),
                                      point.padding = unit(0.3, "lines"),
                                      force = 1, fontface = font.label$face,
                                      size = font.label$size/3, color = font.label$color)
  }
  else{
     p <- p + ggrepel::geom_text_repel(data = labs_data, mapping = aes(label = name),
                             box.padding = unit(0.35, "lines"),
                             point.padding = unit(0.3, "lines"),
                             force = 1, fontface = font.label$face,
                             size = font.label$size/3, color = font.label$color)
  }

  p <- p + scale_x_continuous(breaks=seq(0, max(log2(data$mean+1)), 2))+
    labs(x = xlab, y = ylab, title = main, color = "")+ # to remove legend title use color = ""
    geom_hline(yintercept = c(0, -log2(fc), log2(fc)), linetype = c(1, 2, 2),
               color = c("black", "black", "black"))

  p <- ggpar(p, palette = palette, ggtheme = ggtheme, ...)
  p
}
