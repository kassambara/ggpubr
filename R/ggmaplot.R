#' @include utilities.R ggpar.R
NULL
#'MA-plot from means and log fold changes
#'@description Make MA-plot which is a scatter plot of log2 fold changes (M, on
#'  the y-axis) versus the average expression signal (A, on the x-axis). \code{M
#'  = log2(x/y)} and \code{A = (log2(x) + log2(y))/2 = log2(xy)*1/2}, where x
#'  and y are respectively the mean of the two groups being compared.
#'@inheritParams ggboxplot
#'@inheritParams ggpar
#'@param data an object of class DESeqResults, get_diff, DE_Results, matrix or
#'  data frame containing the columns baseMean (or baseMeanLog2),
#'  log2FoldChange, and padj. Rows are genes.
#'
#'  Two possible formats are accepted for the input data: \itemize{ \item 1/
#'  \code{baseMean | log2FoldChange | padj}. This is a typical output from
#'  DESeq2 pipeline. Here, we'll use log2(baseMean) as the x-axis variable.
#'  \item 2/ \code{baseMeanLog2 | log2FoldChange | padj}. Here, baseMeanLog2 is
#'  assumed to be the mean of logged values; so we'll use it as the x-axis
#'  variable without any transformation. This is the real A in MA plot. In other
#'  words, it is the average of two log-scales values: \code{A = (log2(x) +
#'  log2(y))/2 = log2(xy)*1/2} }
#'
#'  Terminology:
#'
#'  \itemize{ \item baseMean: the mean expression of genes in the two groups.
#'  \item log2FoldChange: the log2 fold changes of group 2 compared to group 1
#'  \item padj: the adjusted p-value of the used statiscal test. }
#'@param fdr Accepted false discovery rate for considering genes as
#'  differentially expressed.
#'@param fc the fold change threshold. Only genes with a fold change >= fc and
#'  padj <= fdr are considered as significantly differentially expressed.
#'@param genenames a character vector of length nrow(data) specifying gene names
#'  corresponding to each row. Used for point labels.
#'@param detection_call a numeric vector with length = nrow(data), specifying if
#'  the genes is expressed (value = 1) or not (value = 0). For example
#'  detection_call = c(1, 1, 0, 1, 0, 1). Default is NULL. If detection_call
#'  column is available in data, it will be used.
#'@param size points size.
#'@param alpha numeric value betwenn 0 an 1 specifying point alpha for
#'  controlling transparency. For example, use alpha = 0.5.
#'@param seed Random seed passed to \code{set.seed}. if
#'  \code{NA}, set.seed will not be called. Default is 42 for reproducibility.
#'@param font.label a vector of length 3 indicating respectively the size (e.g.:
#'  14), the style (e.g.: "plain", "bold", "italic", "bold.italic") and the
#'  color (e.g.: "red") of point labels. For example \emph{font.label = c(14,
#'  "bold", "red")}.
#'@param label.rectangle logical value. If TRUE, add rectangle underneath the
#'  text, making it easier to read.
#'@param top the number of top genes to be shown on the plot. Use top = 0 to
#'  hide to gene labels.
#'@param select.top.method methods to be used for selecting top genes. Allowed
#'  values include "padj" and "fc" for selecting by adjusted p values or fold
#'  changes, respectively.
#'@param label.select character vector specifying some labels to show.
#'@param ... other arguments to be passed to \code{\link{ggpar}}.
#'@return returns a ggplot.
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
#'
#' # Select specific genes to show
#' # set top = 0, then specify genes using label.select argument
#' ggmaplot(diff_express, main = expression("Group 1" %->% "Group 2"),
#'          fdr = 0.05, fc = 2, size = 0.4,
#'          genenames = as.vector(diff_express$name),
#'          ggtheme = ggplot2::theme_minimal(),
#'          top = 0, label.select = c("BUB1", "CD83")
#' )
#'
#'@export
ggmaplot <- function (data, fdr = 0.05, fc = 1.5, genenames = NULL,
                     detection_call = NULL, size = NULL, alpha = 1,
                     seed = 42,
                     font.label = c(12, "plain", "black"), label.rectangle = FALSE,
                     palette = c("#B31B21", "#1465AC", "darkgray"),
                     top = 15, select.top.method = c("padj", "fc"),
                     label.select = NULL,
                     main = NULL, xlab = "Log2 mean expression",  ylab = "Log2 fold change",
                     ggtheme = theme_classic(),...)
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
  # If basemean logged, we'll leave it as is, otherwise log2 transform
  is.basemean.logged <- "baseMeanLog2" %in% colnames(data)
  if(is.basemean.logged){
    data$baseMean <- data$baseMeanLog2
  }
  else if("baseMean" %in% colnames(data)){
    data$baseMean <- log2(data$baseMean +1)
  }

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

  # Change level labels
  . <- NULL
  data$sig <- as.factor(data$sig)
  .lev <- .levels(data$sig) %>% as.numeric()
   palette <- palette[.lev]
  new.levels <- c(
    paste0("Up: ", sum(sig == 1)),
    paste0("Down: ", sum(sig == 2)),
    "NS"
  ) %>% .[.lev]

  data$sig <- factor(data$sig, labels = new.levels)


  # Ordering for selecting top gene
  select.top.method <- match.arg(select.top.method)
  if(select.top.method == "padj") data <- data[order(data$padj), ]
  else if(select.top.method == "fc") data <- data[order(abs(data$lfc), decreasing = TRUE), ]
  # select data for top genes
  complete_data <- stats::na.omit(data)
  labs_data <- subset(complete_data, padj <= fdr & name!="" & abs(lfc) >= log2(fc))
  labs_data <- utils::head(labs_data, top)
  # Select some specific labels to show
  if(!is.null(label.select)){
    selected_labels  <- complete_data %>%
      subset(complete_data$name  %in% label.select, drop = FALSE)
    labs_data <- dplyr::bind_rows(labs_data, selected_labels) %>%
      dplyr::distinct(.data$name, .keep_all = TRUE)
  }


  font.label <- .parse_font(font.label)
  font.label$size <- ifelse(is.null(font.label$size), 12, font.label$size)
  font.label$color <- ifelse(is.null(font.label$color), "black", font.label$color)
  font.label$face <- ifelse(is.null(font.label$face), "plain", font.label$face)

  # Plot
  mean <- lfc <- sig <- name <- padj <-  NULL
  p <- ggplot(data, aes(x = mean, y = lfc)) +
    geom_point(aes(color = sig), size = size, alpha = alpha)

  if(label.rectangle){
    p <- p + ggrepel::geom_label_repel(data = labs_data, mapping = aes(label = name),
                                      box.padding = unit(0.35, "lines"),
                                      point.padding = unit(0.3, "lines"),
                                      force = 1, seed = seed, fontface = font.label$face,
                                      size = font.label$size/3, color = font.label$color)
  }
  else{
     p <- p + ggrepel::geom_text_repel(data = labs_data, mapping = aes(label = name),
                             box.padding = unit(0.35, "lines"),
                             point.padding = unit(0.3, "lines"),
                             force = 1, seed = seed, fontface = font.label$face,
                             size = font.label$size/3, color = font.label$color)
  }

  p <- p + scale_x_continuous(breaks=seq(0, max(data$mean), 2))+
    labs(x = xlab, y = ylab, title = main, color = "")+ # to remove legend title use color = ""
    geom_hline(yintercept = c(0, -log2(fc), log2(fc)), linetype = c(1, 2, 2),
               color = c("black", "black", "black"))

  p <- ggpar(p, palette = palette, ggtheme = ggtheme, ...)
  p
}
