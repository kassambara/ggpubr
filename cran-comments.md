## Test environments
* local macOS 15.6.1 (aarch64-apple-darwin20), R 4.5.1
* Github Action, set up using `usethis::use_github_action("check-standard")`

## R CMD check results
There were no ERRORs or WARNINGs.

## Downstream dependencies
I have also run R CMD check on downstream dependencies of ggpubr.
All packages that I could install passed.

## Update

This is an updated version 0.6.3 (see NEWS.md). Key changes:

- Fixed ggplot2 deprecation warnings (linewidth vs size, after_stat() syntax)
- Fixed dplyr/tidyr deprecation warnings (do() → reframe(), gather() → pivot_longer())
- Raised minimum R version to R >= 4.1.0 and minimum dplyr version to >= 1.1.0
- Multiple bug fixes (stat_cor with European decimals, compare_means with ref.group, .parse_font with decimal sizes)
