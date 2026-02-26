test_that(".parse_font parses decimal sizes with dot or comma", {
  res <- ggpubr:::.parse_font(c("2.4", "italic", "black"))
  expect_equal(res$size, 2.4)
  expect_equal(res$face, "italic")
  expect_equal(res$color, "black")

  old <- options(OutDec = ",")
  on.exit(options(old), add = TRUE)

  res_locale <- ggpubr:::.parse_font(c(2.4, "bold", "red"))
  expect_equal(res_locale$size, 2.4)
  expect_equal(res_locale$face, "bold")
  expect_equal(res_locale$color, "red")

  res_comma <- ggpubr:::.parse_font(c("2,4", "plain", "blue"))
  expect_equal(res_comma$size, 2.4)
  expect_equal(res_comma$face, "plain")
  expect_equal(res_comma$color, "blue")
})
