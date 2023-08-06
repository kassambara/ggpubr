#'Gene Expression Data
#'
#'@description Gene expression data extracted from TCGA using the `RTCGA` and
#'  `RTCGA.mRNA` R packages. It contains the mRNA expression for 3 genes -
#'  GATA3, PTEN and XBP1- from 3 different datasets: Breast invasive carcinoma
#'  (BRCA), Ovarian serous cystadenocarcinoma (OV) and Lung squamous cell
#'  carcinoma (LUSC)
#'@name gene_expression
#'@docType data
#'@usage data("gene_expression")
#'@format A data frame with 1305 rows and 5 columns. \describe{
#'  \item{\code{bcr_patient_barcode}}{sample ID} \item{\code{dataset}}{cance type} \item{\code{GATA3}}{GATA3 gene expression}
#'  \item{\code{PTEN}}{PTEN gene expression}\item{\code{XBP1}}{XBP1 gene expression.}}
#'
#' @examples
#' data(gene_expression)
#'
#' ggboxplot(gene_expression, x = "dataset",
#' y = c("GATA3", "PTEN", "XBP1"),
#' combine = TRUE,
#' ylab = "Expression",
#' color = "dataset", palette = "jco")
#'
NULL
