## Test environments
- macOS Tahoe 26.5.2, R 4.6.0 (aarch64-apple-darwin25.4.0)

## R CMD check results
- 0 errors | 0 warnings | 1 note
- NOTE: The Date field is over a month old.

## Additional comments
- The package metadata requires ggplot2 >= 3.5.2. This check was run with
  ggplot2 4.0.3.
- The previous cran-comments note incorrectly stated that this release requires
  ggplot2 >= 4.0.0; DESCRIPTION and NEWS.md keep the ggplot2 floor at >= 3.5.2.
- Added paired-id comparison tests and refreshed p-value formatting documentation.
