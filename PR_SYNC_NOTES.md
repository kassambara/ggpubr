# ggpubr Sync Notes (ggpubrplus parity)

## Baselines

- Upstream base (`kassambara/ggpubr`): `8ebb039ea819818a7e568bce247171636c1cdd28` (`origin/master`)
- Source parity target (`erdeyl/ggpubrplus`): `efca9bde033241b83101677da23ac8909e764a6f` (`origin/main`)

## What was done

- Synced package sources, tests, manuals, and generated site assets to match the functionality present in `ggpubrplus`.
- Kept upstream package identity:
  - `Package: ggpubr`
  - Original author and maintainer preserved as Alboukadel Kassambara.
  - Upstream project URLs and bug tracker restored to `kassambara/ggpubr`.
- Kept contributor credit for Laszlo Erdey in:
  - `DESCRIPTION` (`Authors@R`, `Author`, `Maintainer` context)
  - package manuals and generated authors page
  - `README.Rmd` / `README.md`
  - `NEWS.md`
  - `inst/CITATION`

## Functional additions synced from ggpubrplus

- p-value formatting subsystem:
  - `format_p_value()`
  - `get_p_format_style()`
  - `list_p_format_styles()`
  - `create_p_label()`
- Extended statistical-layer p-value formatting parameters:
  - `p.format.style`, `p.digits`, `p.leading.zero`, `p.min.threshold`, `p.decimal.mark`
- Compatibility and maintenance updates:
  - `size`/`linewidth` compatibility paths
  - deprecated tidyverse API replacements
  - lock-file utility (`clean_lock_files()`)
- Additional tests for the synchronized behavior.

## Open issue mapping

- `ggpubrplus` open issues: checked all pages, total open issues = `0`.
- `ggpubr` open issues: checked all pages, total open issues = `220`.
- PR auto-closing target:
  - `Closes #334`
- Related but not auto-closing in this PR text:
  - `#540`, `#626`

## Validation run

- `devtools::document('.')`
- `rmarkdown::render('README.Rmd', ...)`
- `pkgdown::build_site('.')`
- `devtools::test()`:
  - `PASS 342`, `WARN 2`, `FAIL 0`
  - warnings are orientation warnings in `test-stat_pvalue_manual.R`
- `R CMD check --no-manual --as-cran .`:
  - completed with `1 WARNING, 5 NOTEs`
  - notes/warning are from checking the working directory directly instead of a built source tarball (expected in this local run mode).
