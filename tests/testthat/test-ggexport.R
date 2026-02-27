test_that("ggexport suppresses filename output when verbose is FALSE", {
  p <- ggplot2::ggplot(ToothGrowth, ggplot2::aes(dose, len)) +
    ggplot2::geom_point()
  filename <- tempfile(fileext = ".png")
  exported_pattern <- paste0(tools::file_path_sans_ext(filename), "*.png")
  on.exit(unlink(Sys.glob(exported_pattern)), add = TRUE)

  expect_silent(
    ggexport(
      p, p,
      filename = filename,
      width = 320, height = 320, res = 72,
      verbose = FALSE
    )
  )
  expect_gte(length(Sys.glob(exported_pattern)), 1)
})

test_that("ggexport reports filename when verbose is TRUE", {
  p <- ggplot2::ggplot(ToothGrowth, ggplot2::aes(dose, len)) +
    ggplot2::geom_point()
  filename <- tempfile(fileext = ".png")
  exported_pattern <- paste0(tools::file_path_sans_ext(filename), "*.png")
  on.exit(unlink(Sys.glob(exported_pattern)), add = TRUE)

  expect_output(
    expect_message(
      ggexport(
        p, p,
        filename = filename,
        width = 320, height = 320, res = 72,
        verbose = TRUE
      ),
      "file saved to"
    ),
    "%03d\\.png"
  )
})
