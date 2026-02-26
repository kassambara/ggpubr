# ggpubr 0.6.3.999

## Summary

- Synced the codebase to the functionality delivered in `ggpubrplus` v0.9.0.3.
- Preserved upstream package identity (`ggpubr`) and original authorship.
- Added contributor credit for Laszlo Erdey in package metadata and documentation.

## Compatibility updates

- Added compatibility updates for modern `ggplot2`, `dplyr`, and `tidyr`.
- Updated legacy `size` usage to `linewidth` where required by recent `ggplot2`.
- Replaced deprecated tidyverse APIs in affected helper functions.
- Added package startup lock-file checks and `clean_lock_files()` helper.
- Relaxed minimum `ggrepel` dependency to `>= 0.9.2` to keep Ubuntu oldrel
  (`R 4.4.x`) CI dependency resolution working.

## Robustness fixes

- Hardened sparse-group handling in `stat_compare_means()` and `geom_pwc()`.
- Non-comparable grouped subsets (insufficient levels/observations) are now skipped
  while preserving valid inferential comparisons in the same layer.
- Added regression tests for mixed comparable/non-comparable subsets and fully sparse
  subsets.

## P-value formatting

- Added `format_p_value()`, `get_p_format_style()`, and `list_p_format_styles()`.
- Added style presets (`default`, `apa`, `nejm`, `lancet`, `ama`, `graphpad`, `scientific`).
- Added formatting parameters to statistical helpers:
  - `p.format.style`
  - `p.digits`
  - `p.leading.zero`
  - `p.min.threshold`
  - `p.decimal.mark`
- Added `p.format.signif` support and related label handling paths.

## Tests and docs

- Expanded test coverage for formatting helpers and compatibility paths.
- Regenerated manuals and package documentation after source sync.

## Issue closure targets for the PR

- Closes #663 (`stat_compare_means()` fatal warning/error in sparse grouped subsets with modern `tidyr`).
- Closes #334 (`stat_compare_means(label = "p.format")` rounding and formatting control).
- Related (not auto-closing): #540, #626.

## Credits

- Original author and maintainer: Alboukadel Kassambara.
- Contributor: Laszlo Erdey (Faculty of Economics and Business, University of Debrecen, Hungary).
