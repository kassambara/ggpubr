
# Gene expression data
df_expr <- tibble::tribble(
  ~sample,    ~dataset, ~GATA3, ~PTEN, ~XBP1,
  "ID.1", "BRCA.mRNA",   2.23,  0.77,  2.91,
  "ID.2", "BRCA.mRNA",   1.24,  1.32,  1.68,
  "ID.3",   "OV.mRNA",  -3.44,  1.65, -1.29,
  "ID.4",   "OV.mRNA",  -5.21,  1.11, -0.62,
  "ID.5", "LUSC.mRNA",  -4.01, -0.05, -0.57,
  "ID.6", "LUSC.mRNA",  -3.29,   0.3, -0.02
)


test_that("check_data works when combine=TRUE and length(y) > 1", {

  observed <- .check_data(df_expr, x = "dataset", y = c("GATA3", "PTEN", "XBP1"), combine = TRUE)
  expected_y <- c(".value." = ".value.")
  expected_x <- c("dataset" = "dataset")
  expected_data <- tibble::tribble(
    ~sample,    ~dataset,    ~.y., ~.value.,
    "ID.1", "BRCA.mRNA", "GATA3",     2.23,
    "ID.2", "BRCA.mRNA", "GATA3",     1.24,
    "ID.3",   "OV.mRNA", "GATA3",    -3.44,
    "ID.4",   "OV.mRNA", "GATA3",    -5.21,
    "ID.5", "LUSC.mRNA", "GATA3",    -4.01,
    "ID.6", "LUSC.mRNA", "GATA3",    -3.29,
    "ID.1", "BRCA.mRNA",  "PTEN",     0.77,
    "ID.2", "BRCA.mRNA",  "PTEN",     1.32,
    "ID.3",   "OV.mRNA",  "PTEN",     1.65,
    "ID.4",   "OV.mRNA",  "PTEN",     1.11,
    "ID.5", "LUSC.mRNA",  "PTEN",    -0.05,
    "ID.6", "LUSC.mRNA",  "PTEN",      0.3,
    "ID.1", "BRCA.mRNA",  "XBP1",     2.91,
    "ID.2", "BRCA.mRNA",  "XBP1",     1.68,
    "ID.3",   "OV.mRNA",  "XBP1",    -1.29,
    "ID.4",   "OV.mRNA",  "XBP1",    -0.62,
    "ID.5", "LUSC.mRNA",  "XBP1",    -0.57,
    "ID.6", "LUSC.mRNA",  "XBP1",    -0.02
  ) %>%
    dplyr::mutate(
      .y. = factor(.y., levels = c("GATA3", "PTEN", "XBP1")),
      dataset = factor(dataset, levels = c("BRCA.mRNA", "OV.mRNA", "LUSC.mRNA"))
    )

  expect_equal(observed$y, expected_y)
  expect_equal(observed$x, expected_x)
  expect_equal(observed$data, expected_data)
})



test_that("check_data works when combine=TRUE and length(x) > 1", {

  observed <- .check_data(df_expr, x = c("GATA3", "PTEN", "XBP1"), y = "..density..", combine = TRUE)
  expected_y <- c("..density.." = "..density..")
  expected_x <- c(".value." = ".value.")
  expected_data <- tibble::tribble(
    ~sample,    ~dataset,    ~.y., ~.value.,
    "ID.1", "BRCA.mRNA", "GATA3",     2.23,
    "ID.2", "BRCA.mRNA", "GATA3",     1.24,
    "ID.3",   "OV.mRNA", "GATA3",    -3.44,
    "ID.4",   "OV.mRNA", "GATA3",    -5.21,
    "ID.5", "LUSC.mRNA", "GATA3",    -4.01,
    "ID.6", "LUSC.mRNA", "GATA3",    -3.29,
    "ID.1", "BRCA.mRNA",  "PTEN",     0.77,
    "ID.2", "BRCA.mRNA",  "PTEN",     1.32,
    "ID.3",   "OV.mRNA",  "PTEN",     1.65,
    "ID.4",   "OV.mRNA",  "PTEN",     1.11,
    "ID.5", "LUSC.mRNA",  "PTEN",    -0.05,
    "ID.6", "LUSC.mRNA",  "PTEN",      0.3,
    "ID.1", "BRCA.mRNA",  "XBP1",     2.91,
    "ID.2", "BRCA.mRNA",  "XBP1",     1.68,
    "ID.3",   "OV.mRNA",  "XBP1",    -1.29,
    "ID.4",   "OV.mRNA",  "XBP1",    -0.62,
    "ID.5", "LUSC.mRNA",  "XBP1",    -0.57,
    "ID.6", "LUSC.mRNA",  "XBP1",    -0.02
  ) %>%
    dplyr::mutate(.y. = factor(.y., levels = c("GATA3", "PTEN", "XBP1")))

  expect_equal(observed$y, expected_y)
  expect_equal(observed$x, expected_x)
  expect_equal(observed$data, expected_data)
})
