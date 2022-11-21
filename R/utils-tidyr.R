# Transform data into long format--------------------
# Replacement of deprecated tidyr::gather_()
# wrapper around tidyr::pivot_longer() but using `cols_vary = "slowest"` as default
# instead of `cols_vary = "fastest"`
df_gather <- function(data, cols, cols_vary = "slowest",
                      names_to = "name", values_to = "value", ...){
  data %>%
    tidyr::pivot_longer(
      cols = dplyr::all_of(cols), names_to = names_to, values_to = values_to,
      cols_vary = cols_vary, ...
    )
}
