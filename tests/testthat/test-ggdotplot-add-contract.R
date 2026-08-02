test_that("ggdotplot honors its documented jitter add value", {
  d <- data.frame(group = rep(c("a", "b"), each = 4), value = 1:8)
  geom_names <- function(add) {
    unname(vapply(
      ggdotplot(d, "group", "value", add = add)$layers,
      function(layer) class(layer$geom)[1],
      character(1)
    ))
  }
  expect_identical(
    list(none = geom_names("none"), jitter = geom_names("jitter")),
    list(none = "GeomDotplot", jitter = c("GeomDotplot", "GeomPoint"))
  )
})
