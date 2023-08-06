data("gene_expression", package = "ggpubr")

test_that(".get_label_data works when label.select specified as a list", {
  results <- .get_label_data(
    data = gene_expression, x = "dataset", y = "XBP1", label ="bcr_patient_barcode",
    label.select = list(criteria = "`y` > 4.4 & `x` %in% c('BRCA', 'OV')"),
  )
  expected <- tibble::tribble(
     ~bcr_patient_barcode, ~dataset,   ~GATA3,             ~PTEN,            ~XBP1,
                "BRCA149",   "BRCA", 2.345125,  1.13357142857143, 4.48758333333333,
                "BRCA536",   "BRCA",   3.1135, 0.846142857142857,          4.73675,
                "BRCA581",   "BRCA",  3.38675, 0.152928571428571,           4.5725
     ) %>%
    data.frame(stringsAsFactors = FALSE)
  expect_equal(results, expected, tolerance = 1e-4)
})
