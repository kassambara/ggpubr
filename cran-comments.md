## Test environments
- macOS Tahoe 26.2, R 4.5.3 (aarch64-apple-darwin25.0.0)

## R CMD check results
- 0 errors | 0 warnings | 1 note
- NOTE: New submission.

## Additional comments
- This release intentionally raises the dependency floors to
  `R >= 4.5.0`, `ggplot2 >= 4.0.0`, `ggrepel >= 0.9.7`, and
  `scales >= 1.4.0`.
- The previous Ubuntu `oldrel-1` CI workaround that pinned `ggrepel` to
  `0.9.5` has been removed because it is outside the supported dependency
  matrix for this development version.
- Refreshed package documentation to reflect the updated support policy.
