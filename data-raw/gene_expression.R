library(RTCGA)
library(RTCGA.mRNA)
gene_expression <- expressionsTCGA(BRCA.mRNA, OV.mRNA, LUSC.mRNA,
                        extract.cols = c("GATA3", "PTEN", "XBP1"))

# Number of samples
nb_samples <- table(expr$dataset)
nb_samples

# simplify data set names by removing the “mRNA” tag.
gene_expression$dataset <- gsub(pattern = ".mRNA", replacement = "",  expr$dataset)
# simplify also the patients’ barcode column.
gene_expression$bcr_patient_barcode <- paste0(gene_expression$dataset, c(1:590, 1:561, 1:154))

usethis::use_data(gene_expression, overwrite = TRUE)
