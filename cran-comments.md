## Test environments
- macOS Tahoe 26.4, R 4.5.3 (aarch64-apple-darwin25.3.0)

## R CMD check results
- 0 errors | 0 warnings | 0 notes

## Additional comments
- Refreshed package metadata and generated documentation, including README assets and package help files.
- Stabilized example output for `compare_means()`, `ggsummarystats()`, and `stat_pvalue_manual()` by printing base data frames instead of tibbles in the examples.
- Verified `devtools::run_examples(run_donttest = TRUE, document = FALSE)` completes successfully in addition to clean `R CMD check` and `R CMD check --as-cran`.
