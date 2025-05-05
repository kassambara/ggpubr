context("test-stat_pvalue_manual")
data("ToothGrowth")
df <- ToothGrowth

test_with_fill <- function(inherit.aes = NULL) {
    stat.test <- df %>%
        group_by(dose) %>%
        rstatix::t_test(len ~ supp) %>%
        rstatix::add_xy_position(x = "dose")

    if (is.null(inherit.aes)) {
        layer <- stat_pvalue_manual(stat.test)
    } else {
        layer <- stat_pvalue_manual(stat.test, inherit.aes = inherit.aes)
    }

    p <- ggplot(df, aes(x = dose, y = len, color = supp, fill = supp)) +
        geom_boxplot() +
        layer
    ggplot2::ggplot_build(p)
}

test_that("fill aes works well by default", {
    expect_error(test_with_fill(inherit.aes = TRUE))
    expect_no_error(test_with_fill(inherit.aes = FALSE))
    expect_no_error(test_with_fill())
})
