## Release summary
- ggpubr 1.1.0: a feature release adding publication-ready figures for common
  analysis workflows — `ggrocplot()` (ROC curves with the AUC and its confidence
  interval, an optional optimal cut-point marker and multi-marker overlays),
  `ggcompare()` (one- and two-way group-comparison figures with adjusted-p
  brackets and an omnibus test label, including simple main effects),
  `ggvolcano()`, `ggestimates()` (forest and estimation plots),
  `ggraincloud()`, `stat_cld()` (compact letter display) and
  `add_test_label()` — plus automatic bracket packing in
  `geom_pwc(pack = "auto")`, several bug fixes and five vignettes (the previous
  CRAN version shipped none).
  See NEWS.md.
- Last CRAN version: 1.0.0.

## Test environments
- macOS 15 (aarch64-apple-darwin), R 4.5.1, local
- GitHub Actions: Ubuntu (devel/release/oldrel-1), macOS (release), Windows (release)

## R CMD check results
- 0 errors | 0 warnings | 1 note
- The only NOTE is "checking HTML version of manual ... Skipping checking HTML
  validation: 'tidy' doesn't look like recent enough HTML Tidy", which reports the
  HTML Tidy version installed on the check host; it is not related to the package.

## Reverse dependencies
- ggpubr has 318 reverse dependencies (230 Depends/Imports/LinkingTo, 88 Suggests).
- Seven changes in this release alter default output. All are corrections of
  output that was previously drawn against the wrong group or silently dropped,
  and all but the last are confined to `ggbarplot()` and `ggsummarystats()`:
  - `ggsummarystats()` now draws its summary table on the categories the plot was
    actually drawn on; previously the table could be transposed relative to the
    plot for a character x axis in non-alphabetical order.
  - `ggsummarystats(free.panels = TRUE)` now titles every panel with the group
    whose data it draws; previously the titles could be permuted across panels
    when the grouping column was not already in sorted order.
  - `ggbarplot()` keeps each error bar on its own bar when a variable is mapped to
    `alpha` and the bars are dodged with `position_dodge()`.
  - The same, for `position_dodge2()`. Most such calls previously failed at draw;
    the configurations that did draw were drawn with the error bars permuted, so
    their appearance changes too.
  - `ggbarplot(position = position_dodge2(reverse = TRUE))` pairs each error bar
    with its own bar; previously each carried the neighbouring bar's statistic.
  - `ggsummarystats(comparisons = )` now draws the comparison brackets and the
    test label that were requested; previously `comparisons` was silently
    dropped and the plot was drawn without them.
  - `select =` and `remove =` used together now filter to the groups asked for.
    The row mask was built before `select` subset the data and then reused after
    it, so the removed group could survive, `NA` rows were introduced, and the
    summaries were computed over the wrong rows. Affects the eight builders
    taking both arguments; either argument on its own is unchanged.
- Two previously accepted inputs are now refused:
  - `ggsummarystats(free.panels = TRUE)` rejects a `labeller` outside the two
    documented values `"label_value"` and `"label_both"`. A number, `TRUE` or a
    factor used to be accepted and selected a labeller by position rather than by
    name, so which one you got depended on the argument's integer value rather
    than on what was asked for.
  - Naming the same item in both `select =` and `remove =` is an error, since the
    call asks to both keep and drop it. Combining the two arguments on different
    items is still supported.
- We downloaded and scanned the sources of all 230 strong reverse dependencies for
  calls to the changed surface. No reverse dependency maps `alpha` in
  `ggbarplot()`, none passes `reverse =` to `position_dodge2()`, none calls
  `ggsummarystats()`, and no package uses `ggbarplot()` and `position_dodge2()`
  together. One package (jsmodule) calls `ggbarplot()` with a summary and
  `position_dodge()`; we reproduced its call shape against 1.0.0 and against this
  release and the built layer data is identical.
- For the `select`/`remove` change specifically: 42 files across the 230 packages
  call one of the eight affected builders, and none of those calls passes
  `select =` or `remove =` at all, so neither the corrected filtering nor the new
  error can be reached. (Matches for those names in reverse-dependency sources
  are base R's `subset(df, select = )` and `tidyr::separate(..., remove = )`.)
- One further case moves output that was already wrong and remains wrong:
  `ggbarplot(top = )` combined with a discrete `alpha` under `position_dodge()`
  still draws more error bars than there are bars, but which stray interval
  lands where now differs. It is documented in NEWS.md.
- All other changes are additive or opt-in with unchanged defaults.
- Conclusion: no new problems expected in reverse dependencies.

## Additional comments
- The package metadata requires ggplot2 >= 3.5.2; this check was also run against
  ggplot2 4.0.3.
- The incoming checks may flag words in the Description as possibly misspelled:
  ggplot, ggpubr, ggrocplot, ggcompare, ggvolcano, ggestimates, ggraincloud, cld,
  pwc and raincloud. These are package and function names, and the statistical
  term "raincloud plot", not misspellings.
