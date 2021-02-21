context("test-get_coord")

# Basic plots: one group -----------------------------------
test_that("No transformation is performed when coord specified", {
  res_10 <- get_coord(coord = 10)
  res_text <- get_coord(coord = "text")
  expect_equal(res_10, 10)
  expect_equal(res_text, "text")
})

test_that("get_coord works for one group", {
  # x- axis
  res_left <- get_coord(
    data.ranges = c(1, 3), group = 1,
    npc = "left", step = 0.1, margin.npc = 0
  )
  # Y -axis
  res_top<- get_coord(
    data.ranges = c(4.2, 36.4), group = 1,
    npc = "top", step = -0.1, margin.npc = 0
  )
  expect_equal(res_top, 36.4)
})


# Grouped plots --------------------------
test_that("get_coord works for grouped plots", {
  # x- axis
  res_left <- get_coord(
    data.ranges = c(1, 3), group = 1:3,
    npc = "left", step = 0.1, margin.npc = 0
  )
  # no transformation when coord specified
  res_coord <- get_coord(
    data.ranges = c(1, 3), group = 1:3,
    coord = 1:3, step = 0.1, margin.npc = 0
  )
  # Y-axis
  res_top <- get_coord(
    data.ranges = c(4.2, 36.4), group = c(1, 2, 3),
    npc = "top", step = -0.1, margin.npc = 0
  )
  expect_equal(res_left, c(1.0, 1.2, 1.4))
  expect_equal(res_coord, 1:3)
  expect_equal(res_top, c(36.40, 39.62, 42.84))
})

