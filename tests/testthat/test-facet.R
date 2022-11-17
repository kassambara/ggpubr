
# Data preparation
data("ToothGrowth")
df <- ToothGrowth
# Adding column name with space
df[['spa ced']] <- df[['supp']]


test_that("facet works", {
  p <- ggpubr::ggboxplot(df, "dose", "len", facet.by = "supp")
  panels <- ggplot2::layer_data(p)[["PANEL"]] %>%
    unique() %>%
    as.numeric()
  expect_equal(panels, c(1, 2))
})


test_that("facet works when column names contain space", {
  p <- ggpubr::ggboxplot(df, "dose", "len", facet.by = "spa ced")
  panels <- ggplot2::layer_data(p)[["PANEL"]] %>%
    unique() %>%
    as.numeric()
  expect_equal(panels, c(1, 2))
})



