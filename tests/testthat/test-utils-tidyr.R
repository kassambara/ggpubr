
# Wide data
get_wide_data <- function(){
  tibble::tibble(
    before = c(200.1, 190.9, 192.7),
    after = c(392.9, 393.2, 345.1)
  )
}


test_that("df_gather is sorting rows as done by gather_()", {
  df <- get_wide_data()
  expected_output <- tibble::tribble(
     ~condition,  ~val,
       "before", 200.1,
       "before", 190.9,
       "before", 192.7,
        "after", 392.9,
        "after", 393.2,
        "after", 345.1
     )
  observed_output <- df_gather(
    df, cols = c("before", "after"),
    names_to = "condition", values_to = "val"
  )
  expect_equal(observed_output, expected_output)
})
