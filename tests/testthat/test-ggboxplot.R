test_that("The position of jittered points is reproducible in ggboxplot", {
  p <- ggboxplot(PlantGrowth, x = "group", y = "weight", add = "jitter")
  jitter_data <- layer_data(p, 2) %>%
    dplyr::select(x, y) %>%
    dplyr::mutate(x = as.numeric(round(x, 7)), y = as.numeric(round(y, 7)))
  expected_data <- tibble::tribble(
    ~x, ~y,
    0.915031, 4.1737042,
    1.1153221, 5.5832184,
    0.9635908, 5.1815256,
    1.153207, 6.1123637,
    1.1761869, 4.4961969,
    0.8182226, 4.6098224,
    1.0112422, 5.1720677,
    1.1569676, 4.5277313,
    1.020574, 5.3285454,
    0.9826459, 5.137853,
    2.1827333, 4.8071424,
    1.9813337, 4.1693164,
    2.0710283, 4.4093098,
    2.0290534, 3.5889508,
    1.8411699, 5.8672196,
    2.15993, 3.8271104,
    1.8984351, 6.0278643,
    1.8168238, 4.8897277,
    1.9311683, 4.3181278,
    2.1818015, 4.6928626,
    3.1558157, 6.3063666,
    3.0771214, 5.1195376,
    3.0562027, 5.5423914,
    3.1977079, 5.4969752,
    3.0622823, 5.3704876,
    3.0834122, 5.2876523,
    3.0176264, 4.9170203,
    3.0376568, 6.1520265,
    2.9156639, 5.8031604,
    2.8588455, 5.2589957
  ) %>%
    as.data.frame()
  expect_equal(jitter_data, expected_data)
})

# #615: ggboxplot() crashed when a `position` was supplied (the internal
# position_dodge(0.8) was hardcoded and collided in geom_exec). `position` is
# now an argument, so the dodge width is controllable and defaults are unchanged.
test_that("ggboxplot accepts a position argument without error (#615)", {
  tg <- ToothGrowth
  tg$dose <- factor(tg$dose)
  expect_no_error(
    ggplot2::ggplot_build(
      ggboxplot(tg, "dose", "len", color = "supp", position = ggplot2::position_dodge(0.5))
    )
  )
  # bxp.errorbar path too
  expect_no_error(
    ggplot2::ggplot_build(
      ggboxplot(tg, "dose", "len", color = "supp", bxp.errorbar = TRUE,
                position = ggplot2::position_dodge(0.9))
    )
  )
})

test_that("ggboxplot default dodge is unchanged and width is controllable (#615)", {
  tg <- ToothGrowth
  tg$dose <- factor(tg$dose)
  box_x <- function(p) ggplot2::ggplot_build(p)$data[[1]]$xmin
  default <- box_x(ggboxplot(tg, "dose", "len", color = "supp"))
  explicit08 <- box_x(ggboxplot(tg, "dose", "len", color = "supp", position = ggplot2::position_dodge(0.8)))
  narrow <- box_x(ggboxplot(tg, "dose", "len", color = "supp", position = ggplot2::position_dodge(0.4)))
  expect_equal(default, explicit08)          # default == dodge(0.8), no regression
  expect_false(isTRUE(all.equal(default, narrow)))  # width actually changes spacing
})
