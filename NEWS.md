# ggpubr 1.0.0.9000

## New features

- New `ggvolcano()` draws a publication-ready volcano plot of differential
  expression results: log2 fold change versus `-log10` of the (adjusted)
  p-value, with points colored as up/down/non-significant by fold-change and
  significance thresholds, the top hits labeled, and threshold lines. It mirrors
  the `ggmaplot()` surface (`fdr`, `fc`, `top`, `select.top.method`,
  `label.select`, `facet.by`) and accepts custom fold-change/p-value column
  names via `x`/`y` (#761).

- New `ggraincloud()` draws a raincloud plot: a half violin (the "cloud"), a
  narrow box plot, and the raw jittered data points (the "rain") offset to the
  opposite side. Supports coloring by group, journal palettes, horizontal
  (default) or vertical orientation, and faceting. The half violin is provided
  by the new `geom_violin_half()` / `GeomViolinHalf`, a one-sided
  `geom_violin()` usable on its own (#760).

- New `ggcompare()` draws a publication-ready group comparison figure in one
  call: a box plot (or violin/stripchart) with jittered points and means,
  pairwise comparison brackets with adjusted p-values (packed compactly), and an
  omnibus test subtitle. It composes existing ggpubr layers (`geom_pwc()`,
  `add_test_label()`) into one customizable `ggplot`; every piece can be turned on
  or off. It follows the `ggfunc` contract, so `ggsummarystats(ggfunc =
  ggcompare)` places a summary table under the comparison figure. As a
  shortcut, `ggsummarystats(comparisons = ...)` now routes to `ggcompare()`
  automatically (previously `comparisons` was silently dropped) (#759).

- New `add_test_label()` adds, in one call, the omnibus test result (one-way
  ANOVA or Kruskal-Wallis) as a plot subtitle, and optionally a pairwise
  comparison description as a caption (`caption = TRUE`). It reproduces the
  common `+ labs(subtitle = get_test_label(...), caption = get_pwc_label(...))`
  idiom, reusing the re-exported rstatix label helpers (#758).

- `geom_pwc()` and `stat_pwc()` gain a `pack` argument. With `pack = "auto"`,
  pairwise comparison brackets are packed onto the fewest possible levels so
  that comparisons whose x-spans do not overlap share a level, keeping the
  annotation compact while guaranteeing that no two brackets on the same level
  overlap within a panel. The default `pack = "none"` keeps the previous
  one-bracket-per-level stacking, so existing plots are unchanged (#757).

- `geom_pwc()` and `stat_pwc()` gain a `{effsize}` label token to display the
  pairwise effect size on each bracket, e.g.
  `label = "{p.adj.signif}, d={effsize}"`. The effect size is the one returned
  by the chosen `method` (Cohen's d for `"t_test"`/`"games_howell_test"`,
  Cliff's delta for `"wilcox_test"`, r for `"dunn_test"`), rounded to two
  decimals. Labels without `{effsize}` are unchanged (#756).

- New `stat_cld()` adds a compact letter display (CLD) of all-pairwise
  comparisons to a plot: one letter per group, placed above each box/violin,
  where groups that do not share a letter are significantly different. Letters
  are derived with `rstatix::add_cld()` from an all-pairwise post-hoc test
  (`method`: `"tukey_hsd"` (default), `"games_howell_test"`, `"dunn_test"`,
  `"wilcox_test"` or `"t_test"`). This is a compact alternative to drawing many
  significance brackets when comparing several groups.

- Re-exported the rstatix annotation helpers `get_test_label()`,
  `get_pwc_label()`, `create_test_label()`, `add_cld()`, and
  `add_xy_position()`, so they can be used directly from ggpubr without
  attaching rstatix (e.g. `ggpubr::add_xy_position()`). These track rstatix's
  signatures.

## Bug fixes

- `stat_compare_means(label = "p.format")` (and `label = "p"`) no longer fail
  with `could not find function "create_p_label"` when ggpubr is called via
  `ggpubr::` without being attached by `library(ggpubr)`. The label helper is now
  namespace-qualified inside `after_stat()` so it resolves off the search path.
  Reported by @pwwang (#751).


# ggpubr 1.0.0

## New features

### New functions

- Added `format_p_value()`, `get_p_format_style()`, and `list_p_format_styles()`.
- Added style presets (`default`, `apa`, `nejm`, `lancet`, `ama`, `graphpad`, `scientific`).

### New arguments, computed variables, and input support

- `ggmaplot()` gains an optional `facet.by` argument to split the MA plot into
  multiple panels (e.g. one per contrast or species). Top genes are selected per
  panel, while point colors use the same significance thresholds in every panel.
  Default output (no `facet.by`) is unchanged (#498).

- `stat_cor()` and `stat_regline_equation()` gain a `label.anchor` argument.
  With `label.anchor = "panel"` the label is placed at the true panel-relative
  position (npc), so labels stay aligned across panels or facets whose axis
  ranges differ - e.g. `facet_wrap(scales = "free_y")`, `geom_smooth()`
  extending each panel by a different amount, or separate plots combined with
  `ggarrange()`. The default `label.anchor = "data"` keeps the previous
  data-range placement, so existing plots are unchanged (#248).

- `geom_bracket()` gains an `orientation` argument. `orientation = "vertical"`
  draws a native vertical bracket (the bar spans the y axis, the tips point left
  and the label is rotated), specified with `ymin`/`ymax`/`x.position` — useful
  to annotate plots where the comparison runs along the y axis, e.g.
  Kaplan-Meier curves (it is designed for a continuous x axis). The default
  `orientation = "horizontal"` is unchanged, and it cannot be combined with
  `coord.flip = TRUE` (#456).

- `ggpaired()` gains an opt-in `jitter` argument that spreads the paired points
  horizontally to reduce overlap. Each subject (`id`) gets a single offset, so
  its two points move together and the connecting line stays intact, and only
  the horizontal positions change (the values are never moved). Default
  `jitter = 0` leaves the plot unchanged (#407).

- `ggboxplot()`, `ggviolin()` and `ggstripchart()` gain an opt-in `show.n`
  argument. When `show.n = TRUE`, the number of observations (`"n = <count>"`)
  is displayed at the top of each group. When the groups are dodged (a
  `color`/`fill` grouping with a dodging `position`), one count is shown per
  group; otherwise a single count is shown per x-axis tick. Counts respect
  `select`/`remove` and are computed per facet. Default is `show.n = FALSE`, so
  existing plots are unchanged (#598, #627).

- `ggarrange()` gains a `spacing` argument to increase the gap between the
  arranged plots (a long-requested option). It adds a uniform margin, in
  text-line units, around each plot. Default is `0` (no extra space; each plot
  keeps its own margins, so existing arrangements are unchanged) (#151).

- `compare_means()` gains an `id` argument for paired comparisons. Without it, a
  paired test (`paired = TRUE`) pairs observations by row order, so the p-value is
  wrong when the data are not sorted so that the compared groups align by subject.
  Passing `id` (the name of a subject-identifier column) pairs the observations by
  id instead — row-order independent, using only the complete pairs (via
  `rstatix`, so the result matches a direct `rstatix::t_test()`/`wilcox_test()`
  call). It works for a two-group, a pairwise (more than two groups) and a
  `ref.group` comparison with `t.test`/`wilcox.test`. Default (`id = NULL`) is
  unchanged (#560).

- `stat_compare_means()` also gains the `id` argument, so a paired test can be
  aligned by subject id directly on a plot (e.g. `ggpaired(...) +
  stat_compare_means(paired = TRUE, id = "subject")`), displaying the correct
  p-value even when the data are not sorted by subject. Plots without `id` are
  unchanged (#560).

- `geom_pwc()` and `stat_pwc()` gain a `p.adjust.n` argument giving the number of
  comparisons to use for the p-value adjustment (passed as `n` to
  `stats::p.adjust()`). Default is `NULL` (adjust by the number of computed
  p-values, unchanged behavior); set it when the displayed comparisons are a
  subset of a larger family so the adjustment reflects that larger size. It must
  be at least the number of p-values being adjusted, and applies to the
  panel-level adjustment (`p.adjust.by = "panel"`) and the single-comparison
  case (#612).

- `annotate_figure()` gains `column.titles` and `row.titles` arguments to add a
  title above each column and to the left of each row of an arranged grid
  (useful for publication panels). Each accepts a character vector (one title
  per column / per row, rendered in bold) or a list of grobs (e.g. built with
  `text_grob()`) for full styling control; `row.titles` are rotated 90 degrees.
  The titles are placed in reserved space (they do not overlap the plots) and
  are evenly spaced, so they assume equal-sized columns / rows (the `ggarrange()`
  default). Default is `NULL` (no titles; existing figures are unchanged) (#573).

- `ggbarplot()` gains a `numeric.x.axis` argument (already available in
  `ggline()` and `ggerrorplot()`). With `numeric.x.axis = TRUE` the x variable
  is kept numeric instead of being coerced to a discrete factor, so bars are
  drawn at their numeric x positions (e.g. a time axis). Default is `FALSE`
  (unchanged behavior) (#463).

- `geom_bracket()` and `stat_pvalue_manual()` gain a `tip.length.ref` argument
  controlling what `tip.length` is a fraction of. With the default
  `tip.length.ref = "data"` the tips are a fraction of the data range (unchanged
  behavior). With `tip.length.ref = "axis"` the tips are a fraction of the
  y-axis range (`ylim`/`scale_y_*`), which renders at the same physical fraction
  across plots and therefore gives visually constant tip lengths regardless of
  the data range - useful to keep tips consistent across facets or across
  separate plots with different scales (#362). `stat_compare_means(comparisons =)`
  draws its brackets via `ggsignif::geom_signif()` and does not support
  `tip.length.ref`; passing it there now emits an informative message pointing to
  `stat_pvalue_manual(..., tip.length.ref = "axis")` instead of being silently
  ignored (#362).

- `stat_cor()` and `stat_regline_equation()` gain a `label.y.step` argument giving
  the vertical spacing (in text-line units) between the labels of successive
  groups. Default is `1.4` (unchanged behavior). Setting `label.y.step = 0` stops
  the per-group vertical shift so labels align across facet panels when a factor
  is mapped to an aesthetic that also defines the facets (the long-standing
  "labels climbing stairs across facets" behavior; #248). This control was
  inspired by the `vstep` argument in the ggpmisc package (by Pedro J. Aphalo),
  from which ggpubr's correlation/regression label-positioning logic was
  originally adapted; this is acknowledged via `@seealso` in `?stat_cor`.

- `ggviolin()` gains a `drop` argument (passed to `ggplot2::geom_violin()`),
  default `TRUE` (unchanged behavior). Previously the argument was silently
  dropped, so grouped violins where a sub-sample has too few points to draw a
  density (which `geom_violin()` removes, including from the dodge position)
  could not be kept aligned with added boxplots / dot plots. Setting
  `drop = FALSE` together with `position = position_dodge(0.8, preserve = "single")`
  now reserves the empty dodge lane so all geoms stay aligned (#381).

- `ggarrange()` gained the ability to choose which plot(s) supply the shared legend:
  `common.legend` now also accepts one or several plot indices. `common.legend = 2`
  uses the second plot's legend (handy when the first plot's legend is not
  representative, e.g. a group is missing in the first plot), and
  `common.legend = c(1, 2)` keeps and combines the legends of the listed plots into a
  single shared block (side by side for `legend = "top"`/`"bottom"`, stacked for
  `"left"`/`"right"`) - useful when the arranged plots genuinely need different
  legends. The documentation of `common.legend` was also clarified: `common.legend =
  TRUE` keeps the first plot's legend (it does not merge or validate legends), so plots
  should share a consistent scale for a single shared legend to be correct. Logical
  `TRUE`/`FALSE` behave exactly as before (#347).

- `ggdonutchart()` gained a `label.repel` argument (default `FALSE`). When `TRUE`, slice
  labels are placed with `ggrepel::geom_text_repel()` and connected to their slice with
  leader lines, so the labels of many small slices no longer overlap (or get dropped). The
  default output is unchanged (#655).

- `ggboxplot()` now accepts a `position` argument (e.g.
  `position = position_dodge(0.9)`), like `ggviolin()`/`ggdotplot()` already do.
  Previously passing `position` errored (`formal argument "position" matched by
  multiple actual arguments`) because the dodge was hardcoded; the default is
  unchanged (`position_dodge(0.8)`), so grouped box plots look the same unless
  you set it (#615).

- `ggsummarytable()` gains an `angle` argument to rotate the summary-table text;
  default (`angle = 0`) is unchanged (#595).

- `ggmaplot()` gains a `line.color` argument to set the threshold line color;
  default ("black") is unchanged (#322).

- `ggarrange()` gains a `byrow` argument (default `TRUE`) to fill the plot grid
  by column (`byrow = FALSE`) instead of by row; forwarded to
  `cowplot::plot_grid()` (#225).

- `stat_regline_equation()` gains `coef.digits` and `rr.digits` arguments to
  control the number of significant digits shown for the regression-equation
  coefficients and R2; defaults (`2`) reproduce the previous output (#312).

- Added formatting parameters to statistical helpers:
  - `p.format.style`
  - `p.digits`
  - `p.leading.zero`
  - `p.min.threshold`
  - `p.decimal.mark`
- Added `p.format.signif` support and related label handling paths.

- `stat_pvalue_manual()` gains a `p.digits` argument (default `3`) that formats
  **numeric** p-value label columns for display (e.g. `label = "p"` or
  `label = "p.adj"`), using the same `format_p_value()` engine as
  `stat_anova_test()`. This restores clean labels (e.g. `0.0156` instead of
  `0.015625`) with rstatix `>= 1.0.0`, which now returns full-precision pairwise
  p-values. Character labels (significance symbols, pre-formatted strings, glue
  expressions) are unaffected. Set `p.digits = NULL` to print the raw value.

- `stat_pvalue_manual()` additionally gains `p.format.style`, `p.leading.zero`,
  `p.min.threshold` and `p.decimal.mark`, matching the p-value formatting
  arguments already available in `stat_compare_means()`, `geom_pwc()` and
  `stat_anova_test()`. These are opt-in and default so that the rendered labels
  are unchanged; for example `p.min.threshold = 0.001` displays very small
  p-values as `< 0.001`, and `p.format.style = "nejm"` applies a journal style.

- `stat_cor()` gains two label-formatting arguments:
  - `r.leading.zero` — set to `FALSE` to drop the leading zero of the
    correlation coefficient (e.g. `.73` instead of `0.73`), completing
    APA-style reporting together with `p.leading.zero` (#540). The dropped
    leading zero is preserved through plotmath rendering (the value is quoted
    so it is not silently re-normalized back to `0.73` in the default
    `expression` output).
  - `p.coef.name` — symbol for the p-value label; use `"P"` for an uppercase
    p-value (#541).

- `stat_cor()` exposes two new computed variables, `rmse` and `rmse.label`, for
  the root mean square deviation (RMSE/RMSD) between `x` and `y` — useful for
  reporting agreement between paired measurements on the same scale (e.g.
  predicted vs. reference values). Display it with
  `aes(label = after_stat(rmse.label))`, or combine it with the correlation
  coefficient using `paste()`. The default label is unchanged (#458).

- `stat_cor()` can now display the confidence interval of the correlation
  coefficient, via the new computed variables `conf.int.low`, `conf.int.high`
  and `conf.int.label` (e.g. `"95% CI [0.21, 0.75]"`) and a `conf.level` argument
  (default `0.95`). Show it with `aes(label = after_stat(conf.int.label))`, or
  combine it with the coefficient using `paste()`. The confidence interval is
  available for `method = "pearson"` only; it is `NA` for Spearman/Kendall. The
  default label is unchanged (#418).

## Main changes

### Compatibility

- Added compatibility updates for modern `ggplot2`, `dplyr`, and `tidyr`.
- Updated legacy `size` usage to `linewidth` where required by recent `ggplot2`.
- Completed the `size` -> `linewidth` migration for the remaining line layers that
  still passed the deprecated `size` argument: the mean/median reference line added
  by `gghistogram(add = ...)` and `ggdensity(add = ...)`, and the connector segments
  of `ggdotchart(add = "segments")`. These no longer emit ggplot2's "`size` aesthetic
  for lines was deprecated" or "Ignoring empty aesthetic: `size`" warnings, and the
  requested line width is now applied via `linewidth`.
- `ggmaplot()` no longer emits an "Ignoring empty aesthetic: `size`" warning on its
  default call; the point layer sets `size` only when the user supplies a value.
- Replaced deprecated tidyverse APIs in affected helper functions.
- Added package startup lock-file checks and `clean_lock_files()` helper.
- Relaxed minimum `ggrepel` dependency to `>= 0.9.2` to keep Ubuntu oldrel
  (`R 4.4.x`) CI dependency resolution working.
- `geom_pwc()` now detects an absent `ref.group` in a grouped subset via the
  `rstatix_missing_ref_group` condition class raised by recent `rstatix`
  (walking the parent chain with `rlang::cnd_inherits()`), in addition to
  matching the error message. This keeps the "skip ref-less subsets" behaviour
  working after `rstatix` made that error message clearer (rstatix #153); older
  `rstatix` versions are still handled via the message fallback.
- The minimum required `rstatix` version is now `>= 1.0.0` (the version that
  introduced the `id` argument used by `compare_means(id = )`, and the
  full-precision pairwise p-values relied on by the p-value formatting).

### Validation and messaging

- Hardened sparse-group handling in `stat_compare_means()` and `geom_pwc()`.
- Non-comparable grouped subsets (insufficient levels/observations) are now skipped
  while preserving valid inferential comparisons in the same layer.
- Added per-group skip diagnostics in `geom_pwc()` so users can see which grouped
  subsets were skipped and why (e.g., missing `ref.group` or insufficient levels).
- Added regression tests for mixed comparable/non-comparable subsets and fully sparse
  subsets.
- Passing `ggtheme = NULL` to the plotting functions (e.g. `ggscatter()`,
  `ggboxplot()`, `ggline()`, `gghistogram()`, ...) now skips applying a ggpubr
  theme, so the plot keeps ggplot2's default theme or the theme set globally with
  `theme_set()`. Previously an explicit `ggtheme = NULL` was treated like an unset
  argument and `theme_pubr()` was still applied. Calls that omit `ggtheme` or pass
  a specific theme are unchanged (#561).
- `compare_means()` now adjusts p-values **within** each `group.by` level (and
  response) rather than pooling all groups together, so a grouped adjustment
  matches filtering to one group and adjusting there. This changes `p.adj` values
  for calls that use `group.by`; ungrouped calls are unchanged (#200).
- `stat_compare_means(comparisons = )` now emits a one-time message (and the docs
  note) clarifying that the displayed pairwise p-values are **not** adjusted for
  multiple comparisons, pointing to `geom_pwc()` / `stat_pvalue_manual()` +
  `compare_means(p.adjust.method = )` for corrected p-values (#293).

### Documentation

- Documented a recipe for labelling groups with significance letters (compact letter display):
  compute the pairwise comparisons and derive the letters with `rstatix::add_cld()`, then place
  them with `geom_text()` (see the "Significance letters" section in `?compare_means`). Also covers
  the per-control letters case (a = differs from control 1, b = differs from control 2) (#464, #434).
- Documented that a summarized `ggbarplot()` (e.g. `add = "mean_se"`) must be faceted with
  the `facet.by=` argument, not by appending `+ facet_wrap()`/`+ facet_grid()`: the summaries
  are pre-computed over `facet.by`, so a manually added facet pools the bars (and, for stacked
  bars, the error bars) across all panels (#739).
- Clarified documentation to match existing behavior: the `ggtheme` default per
  function, the p-value threshold/precision defaults, the `stat_compare_means()`
  label separator (test method name, not correlation coefficient), and the
  package Description's list of p-value formatting styles. Thanks to @erdeyl (#749).
- Expanded test coverage for formatting helpers and compatibility paths.
- Regenerated manuals and package documentation after source sync.
- Closes #663 (`stat_compare_means()` fatal warning/error in sparse grouped subsets with modern `tidyr`).
- Closes #334 (`stat_compare_means(label = "p.format")` rounding and formatting control).
- Related: #540, #626.

### Credits

- Contributor: Laszlo Erdey (Faculty of Economics and Business, University of Debrecen, Hungary).
- Thanks to @gumeo (#418), @byzheng (#608) and @JulesROBIN15 (#625) whose pull
  requests proposed features shipped in this release: the `stat_cor()`
  confidence interval, the `stat_cor()` RMSE label, and a manual number of
  comparisons for p-value adjustment in `geom_pwc()`/`stat_pwc()`.

## Minor changes

- Fixed ColorBrewer sequential palette parsing in `.get_brewer_pal()` so
  `"YlOrBr"` is recognized correctly.
- Updated `ggexport()` to respect `verbose = FALSE` by suppressing filename
  `print()` output in multi-file raster/vector exports.
- Hardened `format_p_value()` by validating non-NULL `p.min.threshold` as a
  single positive finite number.
- Updated `create_p_label()` to preserve `NA` values in `p.format` (returning
  `NA_character_` instead of stringifying to `"p = NA"`).
- `theme_pubr()` now draws the axis tick marks in black with linewidth `0.5`,
  matching the axis lines; previously the ticks inherited a lighter grey/thinner
  style, visibly inconsistent when zoomed (#668).
- `theme_pubr()` now sets `strip.clip = "off"` so the facet strip background
  border renders at its full `linewidth`. With the `ggplot2` (>= 3.5.0) default
  (`strip.clip = "on"`) the border was clipped to the strip area, cutting the
  outer half of the stroke so it looked thinner and misaligned with the panel
  when zoomed (follow-up to #668). Note: a facet label wider than its panel now
  overflows the strip instead of being truncated; restore clipping with
  `+ theme(strip.clip = "on")` if needed.
- `xticks.by`/`yticks.by` now anchor the axis breaks to round multiples of the
  step (e.g. 0, 100, 200) instead of the slightly-negative expanded axis minimum,
  which produced odd labels such as `-20, 80, 180` on bar plots with
  `ylim = c(0, 400)` (#313).
- Import the `%||%` operator from `rlang`; it is used in `geom_bracket()` and
  `geom_pwc()` but base R only provides it since R 4.4, so it could be unresolved
  on the R (>= 4.1) versions the package supports (#665).

## Bug fixes

- `stat_welch_anova_test(label = "as_detailed_italic")` and
  `stat_welch_anova_test(label = "as_detailed_expression")` now display the numeric
  F statistic instead of `FALSE`. Thanks to @erdeyl (#749).
- `ggbarplot()` now supports mapping `alpha` to a discrete grouping variable together
  with a summary (e.g. `alpha = clarity, add = "mean_ci", position = position_dodge()`).
  Previously this errored at draw time (`"alpha * 255": non-numeric argument to binary
  operator`). The mean/CI is now computed per subgroup, the bars are faded per the `alpha`
  variable, and the error bars are dodged to stay aligned with their bars. Bar plots
  without an `alpha` grouping variable are unchanged (#404).
- `ggpar()` no longer errors on `ggsurvplot` objects (or any plot whose theme uses
  `ggtext::element_markdown()`, e.g. survminer's risk-table strata labels). Such
  markdown label elements are now left intact instead of triggering an "Only elements
  of the same class can be merged" error; output for all other plots is unchanged (#382).
- `geom_pwc()` with an explicit list of `comparisons` no longer drops a whole facet/panel
  when a single requested pair cannot be tested (e.g. a group that is entirely `NA` or has
  fewer than two observations). The untestable comparison is skipped (with a message) and
  the remaining valid comparisons are still drawn (#542).
- `compare_means()` and `stat_compare_means()` now forward extra test options
  (e.g. `alternative = "greater"`) to the paired tests aligned by subject `id`,
  which were previously ignored on that path. Grouped and faceted paired-`id`
  comparisons also skip a subset that cannot be tested (fewer than two groups or
  no complete pairs) instead of failing the whole result/layer, while still
  reporting genuinely ambiguous data (e.g. duplicated ids). Thanks to
  @erdeyl (#732).
- `ggbarplot()` now places the error bars of a **stacked** bar chart correctly when the
  data mix positive and negative values (e.g. above/below-ground measurements). The
  stacked error bars are cumulated per sign, matching `position_stack()`, so a negative
  segment's error bar is drawn on its own side instead of being displaced to the other
  side (previously the misplacement also flipped with the factor-level order). Stacked
  charts with single-sign data are unchanged (#426).
- `ggviolin()` now keeps grouped violins aligned with their added box/dot layers by
  default when a sub-group is too sparse for `geom_violin()` to compute a density
  (a single data point). Previously the sparse sub-group was dropped from the dodge,
  so the remaining violin re-centered and no longer lined up. When `drop`/`position`
  are left at their defaults and such a one-point grouped cell is present, the empty
  dodge lane is now reserved automatically. Balanced, ungrouped, faceted, and
  legitimately-unbalanced plots are unchanged, and an explicit `drop`/`position`
  still takes precedence (#381).
- `geom_bracket()` (and `stat_pvalue_manual()`) now draw visible tips for a single
  bracket placed over a stat-computed `y` such as `geom_bar()`/`geom_histogram()`.
  Previously the bracket tips collapsed into a flat line because the y-axis range
  was trained only on the bracket's single `y.position`; the tips are now sized
  against the fully-trained panel range at draw time, so they render correctly even
  with `coord_cartesian(ylim = ...)`. Brackets with an already non-zero y range are
  unchanged (#631).
- `ggline()` now dodges the jittered points together with the line and error
  bars. Previously, in a grouped line plot with `add = "jitter"` and
  `position = position_dodge()`, only the line and summary (e.g. `mean_se`) were
  dodged while the jittered points stayed centered, so they no longer sat under
  their group. The jitter now dodges by the same width. Plots without a
  `position_dodge()` (the default `position = "identity"`) are unchanged (#436).
- `ggbarplot()` now lines the error bars up with the bars when
  `position = position_dodge2()` is used. `position_dodge2()` places elements
  according to their own width, so the thin error bars did not sit on the centre
  of the wider bars they belong to (most visible with `preserve = "single"` when
  one x group has fewer bars than another). The error bars are now drawn at the
  actual bar positions. Other positions (`identity`, `position_dodge()`,
  `position_stack()`) are unchanged (#363).
- `geom_bracket()` and `stat_pvalue_manual()` now place brackets correctly on a
  transformed y axis (e.g. `scale_y_log10()`). `y.position` is given in data units
  but was previously used directly in the scale's transformed space, so brackets
  landed far off (e.g. at `10^30`) and squashed the plot. The bracket `y.position`
  is now run through the scale's transformation; on an untransformed (identity)
  scale this is a no-op, so existing plots are unchanged (#342).
- The core plotting functions (`ggscatter()`, `ggboxplot()`, `ggviolin()`,
  `ggline()`, `ggbarplot()`, `gghistogram()`, `ggdensity()`, `ggdotplot()`,
  `ggstripchart()`, `ggerrorplot()`, `ggecdf()`, `ggdotchart()`, `ggpaired()`,
  `ggqqplot()`) now accept the British spelling `colour` as an alias for
  `color`. Previously `colour` was silently ignored (#317).
- `stat_regline_equation()` now displays the correct equation for orthogonal
  polynomial fits such as `formula = y ~ poly(x, 2)`. Previously the orthogonal
  basis coefficients were printed as if they were raw polynomial coefficients,
  giving a wrong equation that did not match the fitted curve. For a simple
  `poly(x, k)` term with an intercept, the equation is now computed from an
  equivalent `raw = TRUE` fit (identical curve, R², AIC, BIC). Linear,
  `raw = TRUE`, and `I(x^2)` formulas are unaffected; no-intercept models and
  transformed poly arguments (e.g. `poly(log(x), 2)`) keep their previous
  behavior (#653).
- `ggscatterhist()` now aligns the marginal plots with the main scatter plot's
  axes. The margins were built from the raw data, so anything that changed the
  scatter limits (`ellipse = TRUE`, `position` jitter, explicit `xlim`/`ylim`)
  left the marginal histograms/densities misaligned with the scatter. Each
  margin now inherits the scatter's axis range (#220, #420).
- `geom_pwc()` now places comparison brackets over the correct groups when an
  entire x-axis group has only `NA` values. Such a group is dropped before the
  statistical test runs; previously the surviving groups were renumbered from 1,
  which shifted the remaining brackets left. The bracket positions are now
  anchored to the discrete x scale so they stay aligned with the plotted groups
  (#575).
- `ggballoonplot()` now honors user-supplied `xlab`/`ylab` instead of always
  blanking the axis titles; axis titles are still blank by default (#639).
- `ggpar(legend.title = )` now also titles `size` and `alpha` legends, not just
  `colour`/`fill`/`linetype`/`shape` (#412).
- `facet()`/`panel.labs`: a NAMED `panel.labs` vector is now matched to the data
  levels by name, fixing mislabeled panels when the order differed; unnamed
  labels still map positionally (#643).
- `stat_compare_means()` no longer fails with a "`*.npc coord ...`" error when a
  grouped `x` value is `0` (or negative); `.group_coord()` now guards against a
  non-positive group index and falls back to the first label coordinate (#594).
- `ggpar()`/plot functions now apply `xticks.by` and `yticks.by` together; an
  internal `else if` previously dropped `xticks.by` whenever `yticks.by` was
  also supplied (#333).
- `stat_compare_means()` now forwards the `family` argument to the comparison
  bracket labels; it was previously ignored whenever `comparisons` was set
  (#592, #624).
- `ggline()` now supports two grouping variables at once (e.g. `color` and
  `linetype`, or an explicit `group` plus `color`); these are combined into a
  single interaction group so the right points are connected. Previously this
  errored with "the condition has length > 1" (#616, #375).
- A NAMED `palette` vector no longer emits a spurious "No shared levels found
  between `names(values)` ..." warning; the manual color/fill scale is now only
  applied to an aesthetic actually mapped in the plot. Unnamed palettes are
  unchanged, and a named palette whose names genuinely don't match still warns
  (#642).
- `ggviolin()` no longer crashes with `scales = "free"` (or `"free_x"`/`"free_y"`).
  The facet argument `scales` was being partial-matched to the violin `scale`
  parameter; it is now read with exact matching, so `scales` controls faceting
  and the violin `scale` keeps its default (#398).
- `xlim`/`ylim` are now honored with `orientation = "horizontal"` (and
  `rotate = TRUE`). The limits are passed to `coord_flip()` instead of a separate
  `coord_cartesian()` that was silently replaced, which also removes the
  "Coordinate system already present" warning (#646).
- `ggpar()` (and the plot functions) now honor `legend.direction` for all legend
  positions; it was previously ignored for `legend = "top"`/`"bottom"` (#652).
- `compare_means()` no longer errors ("Can't extract columns that don't exist")
  when a formula variable name contains a space (e.g. ``len ~ `spa ced` ``); the
  backticks that R adds to non-syntactic term names are now stripped before
  matching data columns (#385).
- `table_cell_font()` and `table_cell_bg()` can now style an individual header
  cell (`row = 1`), not only body cells; they previously matched only the
  `core-*` grobs and silently did nothing for the header (#535).
- `ggmaplot()` now draws the non-significant ("NS") points behind the significant
  ones, so the up/down-regulated hits are no longer hidden under the grey NS
  cloud; the legend order (Up, Down, NS) is unchanged (#365).
- `ggboxplot()` now forwards `coef` to `geom_boxplot()`, so `coef = 0` can be used
  to omit the whiskers; it was previously dropped by `geom_exec()` (#517).
- `tab_add_title()` / `tab_add_footnote()`: the `just` argument now actually
  positions the text — `"center"`/`"right"` anchor the title/footnote across the
  table width instead of around a fixed point — and the text is no longer clipped
  when it is wider than the table (#302).
- `annotate_figure()`: the figure label (`fig.lab`) now uses a length-independent
  horizontal justification, so labels of different lengths keep the same anchor
  and captions align across figures; previously a longer label was shifted away
  from the corner (#185).

# ggpubr 0.6.3

## Major changes

- Raised minimum R version to R >= 4.1.0 (from R >= 3.1.0) to match ggplot2 >= 3.5.2 requirement.
- Raised minimum dplyr version to dplyr >= 1.1.0 (from dplyr >= 0.7.1) to use modern `reframe()`, `slice_head()`, `slice_tail()`, `across()`, and `where()` functions.

## Minor changes

- Added `linewidth` parameter to `ggboxplot()`, `gghistogram()`, `ggviolin()`, and `ggdensity()` for ggplot2 3.4.0+ compatibility. The `size` parameter is deprecated for line width in these functions (#644, #645, #654, #656, @erdeyl).
- Added `adjust` parameter to `ggviolin()` to control bandwidth adjustment for kernel density estimation (#552, @erdeyl).
- Added `bw` and `adjust` parameters to `ggdensity()` for bandwidth control (#490, @erdeyl).
- `stat_cor()`, `stat_compare_means()`, and `stat_regline_equation()` now use `after_stat()` syntax instead of deprecated `..var..` notation in `default_aes` (#645, @erdeyl).
- `ggballoonplot()` example updated to use `guides(size = "none")` instead of deprecated `guides(size = FALSE)` (@erdeyl).
- Replaced deprecated `tidyr::gather()` with `tidyr::pivot_longer()` in `ggballoonplot()` and `compare_means()` internals (#536, @erdeyl).
- Replaced deprecated `dplyr::do()` with `dplyr::reframe()` in `compare_means()`, `desc_statby()`, and internal helpers. Replaced `dplyr::mutate_if()` with `dplyr::mutate(across(where()))` in `ggsummarytable()` (@erdeyl).

## Bug fixes

- Fixed `border()` deprecation warning by using `linewidth` instead of `size` in `element_rect()` (#644, #654, #656, @erdeyl).
- Fixed `size` deprecation warnings in `ggscatter()` (rug and star plots), `ggpaired()` (connecting lines), `ggecdf()` (ECDF line), `ggdensity()` (density lines), `geom_bracket()`, and `geom_pwc()` (#645, @erdeyl).
- Fixed `stat_cor()` parsing error when `options(OutDec = ",")` is set (European decimal separator) by using `decimal.mark = "."` in `formatC()` calls (#512, @erdeyl).
- Fixed `compare_means()` error "object 'group2' not found" when using `ref.group` with `method = "anova"` or `method = "kruskal.test"` (#572, @erdeyl).
- Reverted the `exact = FALSE` workaround from version 0.6.2 that forced non-default behavior on `wilcox.test()`. Tests now use flexible assertions to ensure compatibility across R versions (#649, #647).
- Fixed `.parse_font()` not recognizing decimal font sizes (e.g., `lab.font = c(2.4, "italic", "black")`), which caused label colors to render incorrectly (#659).

# ggpubr 0.6.2

## Bug fixes

- Fixed compatibility with R-devel r88748 (2025-08-31) which changed Wilcoxon test to provide exact conditional two-sample inference with ties. The `compare_means()` function now sets `exact = FALSE` for `wilcox.test()` and `pairwise.wilcox.test()` to maintain backward compatibility and consistent p-values across R versions (#647).



# ggpubr 0.6.1

## New features

- Added `outliers` parameter to `ggboxplot()` to control the display of outlier points. Set `outliers = FALSE` to remove the black dots representing outliers from box plots (#614, @hswl1314).

## Minor changes

- Enhanced automatic conversion of deprecated dot-dot notation (`..p.signif..`, `..eq.label..`) to modern `after_stat()` calls with proper namespace qualification.
-  Enhanced `ggline()` parameter handling for ggplot2 3.4.0+ compatibility:
    - Added `linewidth` parameter for line width
    - Deprecated `size` parameter for lines with helpful warning message
    - Maintained backward compatibility with existing `size` parameter
    - Prevents conflicts when both parameters are specified


## Bug fixes

- Fixed namespace resolution issues with `after_stat()` calls that were causing failures in reverse dependency packages (`bSi`, `PopComm`). The `convert_label_dotdot_notation_to_after_stat()` function now properly handles namespace qualification while maintaining backward compatibility (#638).
- Improved evaluation environment setup to ensure `ggplot2::after_stat()` is accessible during plot building, resolving "could not find function after_stat" errors in downstream packages.
- Fixed equation format in `stat_regline_equation()` to display in standard mathematical convention "y = mx + b" instead of "y = b + mx" (#559, @tshates, @mwaak).
- Fixed compatibility with ggplot2 4.0.0. Updated `gghistogram()` tests to handle changes in binning standardization introduced in ggplot2 4.0.0 (#635, @teunbrand).
- Fixed `stat_pvalue_manual()` failing when `fill` or other aesthetics are provided in the parent ggplot layer. The function now sets `inherit.aes = FALSE` by default to prevent conflicts between parent plot aesthetics and the p-value annotation data (#621, @fncokg).
- Fixed deprecation warnings for newer package versions:
  - Replaced deprecated `ggplot2::is.ggplot()` with `ggplot2::is_ggplot()` in `ggpar()`
  - Updated `.data$column` syntax to quoted column names in `geom_pwc()` for tidyselect 1.2.0+ compatibility
  - Added `all_of()` wrapper in `unnest()` utility function for tidyselect compatibility
  - Replaced the option `size` by `linewidth` in ggplot2 element_line() and element_rect() functions.
- Fixed deprecation warning in `stat_regline_equation()`  by automatically converting deprecated dot-dot notation (`..eq.label..`, `..adj.rr.label..`, `..p.signif..`, etc.) to `after_stat()` syntax for ggplot2 3.4.0+ compatibility (#623, @hinkyisme).
-  Fixed deprecation warnings in `add_summary()` and `ggerrorplot()` for ggplot2 compatibility:
    - Updated internal `stat_summary()` parameters to use `fun`, `fun.min`, and `fun.max` instead of deprecated `fun.y`, `fun.ymin`, and `fun.ymax` (#587, @vlonde).
    - Fixed line aesthetic parameters by using `linewidth` instead of `size` for line-based error plots



# ggpubr 0.6.0

## New features

- New function `ggadjust_pvalue()` added to adjust p-values produced by `geom_pwc()` on a ggplot (#522).
- New data added: `gene_expression`
- Global options: New available package options: `ggpubr.null_device`, whose value should be a function that creates an appropriate null device. These include: `cowplot::pdf_null_device`, `cowplot::png_null_device`, `cowplot::cairo_null_device` and `cowplot::agg_null_device`. Default is `cowplot::pdf_null_device`. This is used in functions like `as_ggplot()`, which need to open a graphics device to render ggplot objects into grid graphics objects. This function is used to open a null device to avoid displaying an unnecessary blank page when calling `ggarrange()` or `as_ggplot()` (#306 and #158).  The default option can be changed using, for example, `options(ggpubr.null_device = cowplot::png_null_device)`.


## Major changes

- `gadd()`: Restoring back random state after setting seed when adding jittered points. To do so, the seed number is just passed to `position_jitter()` and `position_jitterdodge()`, which preserve the initial random state ( #177 and #349) .
- `ggpubr` now requires a version of `ggrepel >= 0.9.2.9999`, which now restores the initial random state after set.seed(). See https://github.com/slowkow/ggrepel/issues/228
- `ggpubr` now requires a version of `cowplot >= 1.1.1`



## Minor changes

- `ggtexttable()`: doc updated with another example; text justification for individual cells/rows/columns (#335).
- `ggpie()`: setting the default of `clip = "off"` in `coord_polar()` so that `ggpie()` does not crop labels (#429)
- `as_ggplot()`: using null_device to avoid blank page #306 and #158
- `ggarrange()`: using null_device to avoid blank page #306 and #158
- Indexing variable in a data frame: using df[[x]] instead of df[, x] to make sure that the result is a vector even if the `df` is a tibble.
- `ggexport()`: support added for graphics device svg (#469)
- `ggpie()` and `ggdonutchart()` now fully reacts to the option `lab.font` (#502)
- Replacing deprecated `gather_()` in both internal (`.check_data()`) and exported functions (`compare_means()`) (#513)
- `stat_compare_means()`: The dot-dot notation (`..p.signif..`) was deprecated in ggplot2 3.4.0; `after_stat(p.signif)` should be used; updated so that `..p.signif..` is automatically converted into `after_stat()` format without warning for backward compatibility.
- Enable faceting by column names with spaces (#391)
- Licence changed to GPL (>= 2) (#482)
- `desc_statby()` doc updated to clarify the difference between SD (standard deviation) and SE (standard error) (#492)
- The message `geom_smooth() using formula 'y ~ x'` is now turned off in `ggscatter()`(#488)


## Bug fixes

- `ggtext()`: fix warning "`filter_()` was deprecated in dplyr 0.7.0".
- `ggqqplot()`: the argument `conf.int` is taken into account now when specified (#524).
- `ggqqplot()`: Fixing the warning: "The following aesthetics were dropped during statistical transformation: sample" (#523)
- Requiring `rstatix v >=0.7.1.999` for preserving factor class in `emmeans_test()` (#386)
- `ggmaplot()`: Suppressing ggmaplot warning: *Unlabeled data points (too many overlaps). Consider increasing max.overlaps* (#520)
- `compare_means()`: works now when the grouping variable levels contain the key words group2 or group1 (#450)
- `ggparagraph()` : fixing bug about minimum paragraph length (#408)
- `ggexport()`: the verbose argument is now considered when specified by user (#474)

# ggpubr 0.5.0


## New features

- New functions `stat_anova_test()`, `stat_kruskal_test()`, `stat_welch_anova_test()`, `stat_friedman_test()` and `geom_pwc()` added. These are flexible functions to add p-values onto ggplot with more options. The function `geom_pwc()` is for adding pairwise comparisons p-values to a ggplot; supported statistical methods include "wilcox_test", "t_test", "sign_test", "dunn_test", "emmeans_test", "tukey_hsd" and "games_howell_test".
- New functions to convert character vector coordinates into NPC (normalized parent coordinates) and data coordinates: `as_npc()`, `npc_to_data_coordinates()` and `get_coord()`.
- Global options:
    - New function `ggpubr_options()` to display allowed global options in ggpubr
    - New available package options: `ggpubr.parse_aes`. Logical indicating whether to parse aesthetic variable names. Default is `TRUE`. For example, if you want ggpubr to handle non-standard column names, like A-A, without parsing, then set this option to FALSE using `options(ggpubr.parse_aes = FALSE)`.


## Minor changes

- Minimum rstatix version needed is set to 0.7.1
- Minimum ggplot2 version needed is set to 3.4.0
- `stat_conf_ellipse`: ensure stat returns a data.frame for compatibility with ggplot2 v>=3.4.0
- `stat_compare_means()`:
    - Unit tests added
    - Updated to use `after_stat(p.signif)` as the dot-dot notation (`..p.signif..`) was deprecated in ggplot2 3.4.0 (#509).
- `ggdensity()` and `gghistogram()`: dot-dot notation (`..density..`, `..count..`) replaced by `after_stat(density)` and `after_stat(count)`, respectively for compatibility with ggplot2 3.4.0.
- `create_aes()`:
    - Default is now to parse its input, which can be an expression (#348). If you want ggpubr to handle non-standard column names (#229), like A-A, without parsing, then set this option to FALSE using `options(ggpubr.parse_aes = FALSE)`.
    - Supports space in column names like "Dimension 1"
    - Unit tests added
- Arguments (`digits` and `table.font.size`) added to `ggsummarystats()` for changing the summary table decimal place and text size (#341).
- In `stat_pvalue_manual()` the argument `hide.ns` can be either a logical value (TRUE or FALSE) or a character value ("p" or "p.adj" for filtering out non significant by p-value or adjusted p-values).
- Now, the x-axis tick label names correctly align with the corresponding ticks when the rotation angle of the texts is set to 90. This is automatically achieved by setting internally `vjust = 0.5` (#301).
- `Capital NS.` is no longer displayed by `stat_compare_means()` (#171)
- Unit tests added for`ggshistogram()` to make sure that it works when:
    - using `after_stat()`,
    - using after_stat() with trailing space inside parentheses.
- Unit tests added for`ggscatter()` to make sure that:
    - it works when there is spaces in variable names
    - it can handle non-standard column names when `ggpubr.parse_aes` global option is set to FALSE (#229)





# ggpubr 0.4.0


## New features

- New functions added to customize `ggtexttable()` (#125, #129 and #283):
    - `tab_cell_crossout()`: cross out a table cell.
    - `tab_ncol(), tab_nrow()`: returns, respectively, the number of columns and rows in a ggtexttable.
    - `tab_add_hline()`: Creates horizontal lines or separators at the top or the bottom side of a given specified row.
    - `tab_add_vline()`: Creates vertical lines or separators at the right or the left side of a given specified column.
    - `tab_add_border(), tbody_add_border(), thead_add_border()`: Add borders to table; tbody is for table body and thead is for table head.
    - `tab_add_title()` and `tab_add_footnote()` to add titles and footnotes (#243).
- ggpubr functions updated to handle non-standard column names, for example ("A-A") (#229).
- New function `create_aes()` added to create aes mapping from a list. Makes programming easy with ggplot2 (#229).
- New argument `coord.flip` added to support adding p-values onto horizontal ggplots (#179). When adding the p-values to a horizontal ggplot (generated using `coord_flip()`), you need to specify the option `coord.flip = TRUE`.
- New errorbar functions - `median_hilow_()` and `median_q1q3()` -  added ([@davidlorenz, #209](https://github.com/kassambara/ggpubr/issues/209)):
    - `median_hilow_()`: computes the sample median and a selected pair of outer quantiles having equal tail areas. This function is a reformatted version of `Hmisc::smedian.hilow()`. The confidence limits are computed as follows: `lower.limits = (1-ci)/2` percentiles; `upper.limits = (1+ci)/2` percentiles. By default (`ci = 0.95`), the 2.5th and the 97.5th percentiles are used as the lower and the upper confidence limits, respectively. If you want to use the 25th and the 75th percentiles as the confidence limits, then specify `ci = 0.5` or use the function `median_q1q3()`.
    - `median_q1q3()`: computes the sample median and, the 25th and 75th percentiles. Wrapper around the function median_hilow_() using ci = 0.5.
- New function `get_breaks()` added to easily create breaks for numeric axes. Can be used to increase the number of x and y ticks by specifying the option `n`. It's also possible to control axis breaks by specifying a step between ticks. For example, if by = 5, a tick mark is shown on every 5 ([@Chitanda-Satou, #258](https://github.com/kassambara/ggpubr/issues/258)).


## Major changes

- The following enhancement has been added to `ggscatterhist()` ([@juliechevalier, #176](https://github.com/kassambara/ggpubr/issues/176)):
    - the output of `ggscatterhist()` is now a list of ggplots, containing the main scatter plot (`sp`) and the marginal plots (`xplot` and `yplot`), which can be customized by the end user using the standard ggplot verbs
    - An S3 printing method is now available for an object of class ggscatterhist. The printing method displays the arranged final figure.

## Minor changes

- Now, when creating a box plot with error bars, color and fill arguments are taken into account in the errorbar function (#105).
- New argument `alternative` supported in `stat_cor()` (#276).
- New argument `position` in `ggline()` to make position "dodged" (#52).
- New argument `outlier.shape` in ggboxplot(). Default is 19. To hide outlier, specify outlier.shape = NA. When jitter is added, then outliers will be automatically hidden.
- Sorting can be now disabled in `ggdotchart()` using the option `sorting = "none"` (#115, #223).
- New argument `weight` added in `gghistogram()` for creating a weighted histogram (#215)
- Now `ggscaterhist()` takes into account the argument `position` in `margin.params` when marginal plot is a histogram (#286).
- `ggbarplot()` enhanced to better handle the creation of dodged bar plots combined with jitter points ([@aherholt, #176](https://github.com/kassambara/ggpubr/issues/282))
- New argument `bracket.shorten` added in `stat_pvalue_manual()` and `geom_bracket()`. a small numeric value in [0-1] for shortening the with of bracket (#285).
- New argument `bracket.nudge.y` added in `stat_pvalue_manual()` and `geom_bracket()`. Vertical adjustment to nudge brackets by. Useful to move up or move down the bracket. If positive value, brackets will be moved up; if negative value, brackets are moved down ([#281](https://github.com/kassambara/ggpubr/issues/281)).
- New argument `numeric.x.axis` added in `ggerrorplot()`; logical value, If TRUE, x axis will be treated as numeric. Default is FALSE ([#280](https://github.com/kassambara/ggpubr/issues/280)).
- The option `width` is now considered in `ggadd()` for plotting error bars ([#278](https://github.com/kassambara/ggpubr/issues/278)).
- New argument `linetype` in `ggpaired()`.
- `geom_exec()` used in `ggpaired()` to add lines between paired points.
- `ggmaplot()` now supports two input formats (#198):
    1. baseMean | log2FoldChange|padj: Here, we'll use log2(baseMean) as the x-axis variable
    2. baseMeanLog2 | log2FoldChange|padj: here, baseMeanLog2 is assumed to be the mean of logged values; so we'll use it as x-axis variable without any transformation.
- new arguments added in `ggmaplot()`:
    - `alpha` for controlling point transparency/density ([@apcamargo, #152](https://github.com/kassambara/ggpubr/issues/152)).
    - `label.select` to select specific genes to show on the plot ([@apastore, #70](https://github.com/kassambara/ggpubr/issues/70))
- In `ggadd()` the `fill` argument is considered for jitter points only when the point shape is in 21:25 ([@atakanekiz, #148](https://github.com/kassambara/ggpubr/issues/148)).
- New argument `parse` added in `ggscatter()` and in `ggtext()`. If TRUE, the labels will be parsed into expressions and displayed as described in ?plotmath (#250).
- New argument `stroke` supported in `ggscatter()` and in `ggline()`. Used only for shapes 21-24 to control the thickness of points border ([@bioguy2018, #258](https://github.com/kassambara/ggpubr/issues/236)).
- the `stat_cor()` function code has been simplified. New arguments `p.accuracy` and `r.accuracy` added; a real value specifying the number of decimal places of precision for the p-value and the correlation coefficient, respectively. Default is NULL. Use (e.g.) 0.01 to show 2 decimal places of precision ([@garthtarr, #186](https://github.com/kassambara/ggpubr/issues/186), [@raedevan6, #114](https://github.com/kassambara/ggpubr/issues/114), [#270](https://github.com/kassambara/ggpubr/issues/270)).


## Bug fixes

- `annotate_figure()` manual updated to show how to use of superscript/subscript in the axis labels (#165).
- `ggtextable()` now supports further customization when theme is specified (#283).
- the argument `font.family` is now correctly handled by `ggscatter()` (#149)
- `ggpar()` arguments are correctly applied using `ggpie()` (#277).
- `ggscatter()`: When `conf.int = FALSE`, fill color is set to "lightgray" for the regression line confidence band ([@zhan6073, #111](https://github.com/kassambara/ggpubr/issues/111)).
- Now, `gghistogram()` supports the parameter `yticks.by` ([@Chitanda-Satou, #258](https://github.com/kassambara/ggpubr/issues/258)).


# ggpubr 0.3.0


## New features

- New functions:
    - `ggsummarystats()` to create a GGPlot with summary stats table under the plot ( [#251](https://github.com/kassambara/ggpubr/pull/251)).
    - `clean_table_theme()` to clean the theme of a table, such as those created by `ggsummarytable()`
- `ggbarplot()` now supports stacked barplots with error bars ([#245](https://github.com/kassambara/ggpubr/pull/245)).



## Minor changes

- New arguments:
    - `vjsut` in `stat_compare_means()` to move the text up or down relative to the bracket.
    - `type` in `geom_bracket()` to specify label type. Can be "text" or "expression" (for parsing plotmath expression); [#253](https://github.com/kassambara/ggpubr/issues/253).
    - `labeller` to the function `facet()`
    - `position` in `get_legend()` to specify legend position
    - `legend.grob` in `ggarrange()` to specify a common legend you want to add onto the combined plot.
- Maintenance adaptation to dplyr new version by removing deprecated functions, such as group_by_, select_, arrange_, etc

## Bug fixes

- Now, Barplots are correctly labelled when custom labels are specified by users ([@sekharcu, #234](https://github.com/kassambara/ggpubr/issues/234))


# ggpubr 0.2.5

## Minor changes

- New arguments `cor.coef.name` in the function `stat_cor()`. Can be one of "R" (pearson coef), "rho" (spearman coef) and "tau" (kendall coef). Uppercase and lowercase are allowed ([@andhamel, #216](https://github.com/kassambara/ggpubr/issues/228)).
- New arguments `digits, r.digits, p.digits` in the function `stat_cor()`. Integer indicating the number of decimal places (round) or significant digits (signif) to be used for the correlation coefficient and the p-value ([@raedevan6, #216](https://github.com/kassambara/ggpubr/issues/114)).
- `compare_means()` adapted to tidyr v>= 1.0.0 by specifying cols in the unnest() function ([@Youguang, #216](https://github.com/kassambara/ggpubr/issues/216)).


# ggpubr 0.2.4


## Minor change

- unnest adapted to tidyr 1.0.0
- `stat_pvalue_manual()` can now handle an rstatix test result containing only one group column.

# ggpubr 0.2.3

## New features

- New function `stat_central_tendency()` to add central tendency measures (mean, median, mode) to density and histogram plots
- New function `stat_overlay_normal_density()` to overlay normal density plot (with the same mean and SD) to the density distribution of 'x'.

## Minor changes

- The option `exact = FALSE` is no longer used when computing correlation in `stat_cor()` ([@tiagochst, #205](https://github.com/kassambara/ggpubr/issues/205))


## Bug fixes

- `ggpie()` keeps now the default order of labels ([@WortJohn, #203](https://github.com/kassambara/ggpubr/pull/203))


# ggpubr 0.2.2

## New features

- New function `geom_bracket()` for adding brackets with label annotation to a ggplot. Helpers for adding p-value or significance levels to a plot.

## Minor changes

- `compare_means()` has been adapted to tidyr v1.0.0 ([@jennybc, #196](https://github.com/kassambara/ggpubr/pull/196))
- `geom_exec()` now handles `geom_bracket()` arguments
- New arguments `vjust`, `hide.ns`, `step.increase`, `step.group.by`, `color` and `linetype` added in `stat_pvalue_manual()`
- `stat_pvalue_manual()` can now guess automatically the significance label column.
- New argument `show.legend` added to `ggadd()` and `add_summary()` functions.

## Bug fixes

- Bug fixes in `gghistogram()`. Works now when the x variable is R keyword, such as var, mean, etc. ([#192](https://github.com/kassambara/ggpubr/issues/192))
- In `ggline()`, error bars now react automatically to grouping by line type ([#191](https://github.com/kassambara/ggpubr/issues/191))


# ggpubr 0.2.1

## Minor changes

- New arguments `step.increase` added in `stat_compare_means()` to avoid overlap between brackets.
- In `stat_pvalue_manual()` x axis variable is no longer automatically converted into factor. If your x variable is a factor, make sure that it is converted into factor.
- `stat_pvalue_manual()` can automatically handle the output of rstatix tests
- `ggbarplot()` and `ggviolin()` now automatically create error bars by groups when users forget the option `add.params = list(group = )` ([#183](https://github.com/kassambara/ggpubr/issues/183)).
- Now, `ggarrange()` works when either `ncol = 1` or `nrow = 1` ([@GegznaV, #141](https://github.com/kassambara/ggpubr/issues/144).
- When method = "wilcox.test", the function `compare_means()` set automatically the option `exact = FALSE`. This is no longer the case ([@stemicha, #141](https://github.com/kassambara/ggpubr/issues/141).
- `stat_pvalue_manual()` now supports dodged grouped plots ([@emcnerny, #104](https://github.com/kassambara/ggpubr/issues/104)).
- the argument `position` is now handled by `ggdotplot()` ([@Adam-JJJJJ, #178](https://github.com/kassambara/ggpubr/issues/178))

## Bug fixes

- Adding points works now for barplots grouped by fill color ([@elenichri](https://github.com/kassambara/ggpubr/issues/173))
- `label.sep` argument works now in `ggscatter()` and `stat_cor()` ([@sbbmu, #150](https://github.com/kassambara/ggpubr/issues/150))
- Fix in `ggscatter()` to avoid freezing when the `add` argument is incorrect ([@atakanekiz, #135](https://github.com/kassambara/ggpubr/issues/180)).


# ggpubr 0.2

## Bug fixes

- P-values for multiple comparisons by group (stat_compare_means()) are now correctly displayed ([@elisheva100, #135](https://github.com/kassambara/ggpubr/issues/135)).


# ggpubr 0.1.9

## Minor changes

- ggsci palettes have been updated to add new palettes: nejm, jama, ucscgb, d3, locuszoom, igv, startrek, tron, futurama, simpsons ([@cbrueffer, #118](https://github.com/kassambara/ggpubr/pull/127))


## Bug fixes

- The option `ref.group` was only considered when the grouping variable contains more than two levels. In that case, each level is compared against the specified reference group. Now, `ref.group` option is also considered in two samples mean comparisons ([@OwenDonohoe, #118](https://github.com/kassambara/ggpubr/issues/118))

- Now, `ggqqplot()` reacts to the argument `conf.int.level` ([@vsluydts, #123](https://github.com/kassambara/ggpubr/issues/123))
- Added error bar color is now inherited from the main plot ([@JesseRop, #109](https://github.com/kassambara/ggpubr/issues/109))


# ggpubr 0.1.8


## New features

- New arguments `bxp.errorbar` added to `ggboxplot()` for adding error bars at the top of the box plots ([@j3ypi, #105](https://github.com/kassambara/ggpubr/issues/105).
- New function `stat_pvalue_manual()` for adding p-values generated elsewhere ([@achamess, #81](https://github.com/kassambara/ggpubr/issues/81), [@grst, #65](https://github.com/kassambara/ggpubr/issues/65)).


## Minor changes

- `alpha` option added to `ggviolin()` [@mtmatter, #77](https://github.com/kassambara/ggpubr/pull/77)
- New argument `bracket.size` added to `stat_compare_means()` [@mtmatter, #43](https://github.com/kassambara/ggpubr/issues/43)
- Now, the function `stat_cor()` supports R^2 as an option [@philament, #32](https://github.com/kassambara/ggpubr/issues/32)
- New argument `position` added in `gghistogram()`. Allowed values include "identity", "stack", "dodge".
- New argument `ci` added in `ggerrorplot()` [@abrar-alshaer, #94](https://github.com/kassambara/ggpubr/issues/94)

## Bug fixes

- Now, `ggscatter()` can remove the letter 'a' from the legend, when the argument `show.legend.text = FALSE` specified [@atsyplenkov, #106](https://github.com/kassambara/ggpubr/issues/106).
- Now, adding a `size` option to ggscatter `add.params` is supported [@retrogenomics, #94](https://github.com/kassambara/ggpubr/issues/53).

# ggpubr 0.1.7

## New features

- New function `ggdonutchart()` added.

## Minor changes

- Significance levels can be now customized and passed to `stat_compare_means()` ([@jaison75, #45](https://github.com/kassambara/ggpubr/issues/30)).

- Editing pdf size is now supported in `ggexport()` ([@JauntyJJS, #45](https://github.com/kassambara/ggpubr/issues/63)).

## Bug fixes

- In `ggscatterhist()` the x variable was plotted two times, on both the plot x & y margins, instead of having, as expected, a) the x variable on the main plot x margin and 2) the y variable on the main plot y margin. This has been now fixed.
- In previous version, `ggdotchart()` sorted automatically within groups when the `color` argument is specified, even when groups = NULL. This default behaviour has been now removed. Sorting within groups is performed only when the argument `group` is specified ([@sfeds, #90](https://github.com/kassambara/ggpubr/issues/90)).
- Now, `yticks.by` and  `xticks.by` work with NAs ([@j3ypi, #89](https://github.com/kassambara/ggpubr/issues/89)).


# ggpubr 0.1.6

## New features

- New function `ggballoonplot()` added to visualize a contingency table.

- `ggdotchart()` can be now used to plot multiple groups with `position = position_dodge()` ([@ManuelSpinola, #45](https://github.com/kassambara/ggpubr/issues/45)).

- New function `ggscatterhist()` to create a scatter plot with marginal histograms, density plots and box plots.

- New theme `theme_pubclean()`: a clean theme without axis lines, to direct more attention to the data.

- New arguments in `ggarrange()` to customize plot labels ([@G-Thomson, #41](https://github.com/kassambara/ggpubr/issues/38)):
    - font.label
    - label.x and label.y
    - hjust and vjust

- New argument `method.args` added to `stat_compare_means()`. A list of additional arguments used for the test method. For example one might use method.args = list(alternative = "greater") for wilcoxon test ([@Nicktz, #41](https://github.com/kassambara/ggpubr/issues/41)).

- New argument `symnum.args` added to `stat_compare_means()`. A list of arguments to pass to the function symnum for symbolic number coding of p-values. For example, `symnum.args <- list(cutpoints = c(0, 0.0001, 0.001, 0.01, 0.05, 1), symbols = c("****", "***", "**", "*", "ns"))`

- New functions `table_cell_font()` and `table_cell_bg()` to easily access and change the text font and the background of `ggtexttable()` cells ([@ProbleMaker, #29](https://github.com/kassambara/ggpubr/issues/29)).

- New argument `numeric.x.axis` in `ggline()`. logical. If TRUE, x axis will be treated as numeric. Default is FALSE. ([@mdphan, #35](https://github.com/kassambara/ggpubr/issues/35))

- New argument `lab.nb.digits` in `ggbarplot()`. Integer indicating the number of decimal places (round) to be used ([#28](https://github.com/kassambara/ggpubr/issues/28)). Example: lab.nb.digits = 2.

- New argument `tip.length` in `stat_compare_means()`. Numeric vector with the fraction of total height that the bar goes down to indicate the precise column. Default is 0.03. Can be of same length as the number of comparisons to adjust specifically the tip length of each comparison. For example tip.length = c(0.01, 0.03).


## Minor changes

- Now `get_legend()` returns NULL when the plot doesn't have legend.


## Bug fixes

- Now data argument are supported in  `stat_compare_means()` when the option comparisons are specified ([@emcnerny, #48](https://github.com/kassambara/ggpubr/issues/48))

- Now `compare_means()` returns the same p-values as `stat_compare_means()` ([@wydty, #15](https://github.com/kassambara/ggpubr/issues/34)).
- `stat_compare_means()` now reacts to label = "p.format" when comparisons specified ([#28](https://github.com/kassambara/ggpubr/issues/28)).
- Now, the p.values are displayed correctly when ref.group is not the first group ([@sehufnkjesktgna, #15](https://github.com/kassambara/ggpubr/issues/27)).

# ggpubr 0.1.5

## Minor changes

- In `ggpar()`, now `legend.title` can be either a character vector, e.g.: legend.title = "Species" or a list, `legend.title = list(color = "Species", linetype = "Species", shape = "Species")`.

- New argument `ellipse.border.remove` in `ggscatter()` to remove ellipse border lines.

```r
ggscatter(mtcars, x = "mpg", y = "wt",
          color = "cyl",
          ellipse = TRUE, mean.point = TRUE,
          ellipse.border.remove = TRUE)
```

- In `ggscatter`(), the argument `mean.point` now reacts to fill color.
- Support for text justification added in `ggtexttable()` ([@cj-wilson, #15](https://github.com/kassambara/ggpubr/issues/18))

- The function `ggpie()` can now display japanese texts. New argument `font.family` in `ggpie`() and in `ggpar()` ([@tomochan001, #15](https://github.com/kassambara/ggpubr/issues/15)).

- Using time on x axis works know with `ggline()` and `ggbarplot()` ([@jcpsantiago, #15](https://github.com/kassambara/ggpubr/issues/17)).



## Bug fixes

- `stat_compare_means()` now reacts to `hide.ns` properly.
- `drawDetails.splitText()` exported so that the function `ggparagraph()` works properly.
- Now, ggpubr functions accept expression for label text
- In `ggbarplot()`, now labels correspond to the true size of bars ([@tdelhomme, #15](https://github.com/kassambara/ggpubr/issues/15)).
- `stat_compare_means()` now keeps the default order of factor levels ([@RoKant, #12](https://github.com/kassambara/ggpubr/issues/12)).


# ggpubr 0.1.4

## New features

- New helper functions:
    - `gradient_color()` and `gradient_fill()`: change gradient color and fill palettes.
    - `clean_theme()`: remove axis lines, ticks, texts and titles.
    - `get_legend()`: to extract the legend labels from a ggplot object.
    - `as_ggplot()`: Transform the output of `gridExtra::arrangeGrob()` and `gridExtra::grid.arrange()` to an object of class ggplot.
    - `ggtexttable()`: to draw a textual table.
    - `ggparagraph()`: to draw a paragraph of text.
    - fill_palette() and color_palette() to change the fill and color palette, respectively.
    - `annotate_figure()` to annotate (arranged) ggplots.
    - `text_grob()` to create easily a customized text graphical object.
    - `background_image()` to add a background image to a ggplot.

- New theme function `theme_transparent()` to create a ggplot with transparent background.

## Minor changes

- In `gghistogram()`, density curve and rug react to the fill color.
- `ggarrange()`:
    - New  argument `align` to specify whether graphs in the grid should be horizontally ("h") or vertically ("v") aligned.
    - New argument `legend` to remove or specify the legend position when arranging multiple plots.
    - New argument `common.legend` to create a common unique legend for multiple plots.


# ggpubr 0.1.3

## New features

- New functions:
    - `ggarrange()` to arrange multiple ggplots on the same page.
    - `ggexport()` to export one or multiple ggplots to a file (pdf, eps, png, jpeg).
    - `ggpaired()` to plot paired data.
    - `compare_means()` to compare the means of two or multiple groups. Returns a data frame.
    - `stat_compare_means()` to add p-values and significance levels to plots.
    - `stat_cor()` to add correlation coefficients with p-values to a scatter plot.
    - `stat_stars()` to add stars to a scatter plot.



- Now, the argument `y` can be a character vector of multiple variables to plot at once. This might be useful in genomic fields to plot the gene expression levels of multiple genes at once. see `ggboxplot()`, `ggdotplot()`, `ggstripchart()`, `ggviolin()`, `ggbarplot()` and `ggline`.

- The argument `x` can be a vector of multiple variables in `gghistogram()`, `ggdensity()`, `ggecdf()` and `ggqqplot()`.


- New functions to edit ggplot graphical parameters:
    - `font()` to change the appearance of titles and labels.
    - `rotate_x_text()` and `rotate_y_text()` to rotate x and y axis texts.
    - `rotate()` to rotate a ggplot for creating horizontal plot.
    - `set_palette()` or `change_palette()` to change a ggplot color palette.
    - `border()` to add/change border lines around a ggplot.
    - `bgcolor()` to change ggplot panel background color.
    - `rremove()` to remove a specific component from a ggplot.
    - `grids()` to add grid lines.
    - `xscale()` and `yscale()` to change axis scale.


- New helper functions:
    - `facet()` added to create multi-panel plots ([#5](https://github.com/kassambara/ggpubr/issues/5)).
    - `add_summary()` to add summary statistics.
    - `ggadd()` to add summary statistics or a geometry onto a ggplot.


- New data set added: `gene_citation`


- New arguments in `ggpar()`: `x.text.angle` and `y.text.angle`



## Major changes

- New arguments in ggpubr functions, see `ggboxplot()`, `ggdotplot()`, `ggstripchart()`, `ggviolin()`, `ggbarplot()` and `ggline`:
    - `combine` added to combine multiple y variables on the same graph.
    - `merge` to merge multiple y variables in the same plotting area.
    - `select` to select which item to display.
    - `remove` to remove a specific item from a plot.
    - `order` to order plot items.
    - `label, font.label, label.select, repel, label.rectangle` to add and customize labels
    - `facet.by, panel.labs and short.panel.labs`: support for faceting and customization of plot panels


- New argument `grouping.vars`  in `ggtext()`. Grouping variables to sort the data by, when the user wants to display the top n up/down labels.


- New arguments in `theme_pubr()`:
    - border,
    - margin,
    - legend,
    - x.text.angle


## Minor changes


- Now, the argument `palette` can be also a numeric vector of length(groups); in this case a basic color palette is created using the function `grDevices::palette()`.

## Bug fixes

- Now, `ggpar()` reacts to palette when length(palette) = 1 and palette is a color name [#3](https://github.com/kassambara/ggpubr/issues/3).

- `ggmaplot()` now handles situations, where there is only upregulated, or downregulated genes.


# ggpubr 0.1.2


## New features

- New function `get_palette()` to generate a palette of k colors from ggsci palettes, RColorBrewer palettes and custom color palettes. Useful to extend RColorBrewer and ggsci to support more colors.

## Minor changes

- Now the `ggpar()` function can handle a list of ggplots.
- Now the default legend position is `right`.
- New argument `show.legend.text` in the `ggscatter()` function. Use show.legend.text = FALSE to hide text in the legend.
- New arguments `title, submain, subtitle, caption, font.submain, font.subtitle, font.caption` in the `ggpar()` function.
- New argument `font.family` in `ggscatter()`.

## Bug fixed

- Group means for `ggdensity` (`gghistogram`) are now shown when data have NA values [@chunkaowang, #1](https://github.com/kassambara/ggpubr/issues/1)


# ggpubr 0.1.1


## New features

- New function `ggtext()` for textual annotation.
- New argument star.plot in `ggscatter()`. A logical value. If TRUE, a star plot is generated.
- New helper function `geom_exec()`. A helper function used by ggpubr functions to execute any geom_xx functions in ggplot2. Useful only when you want to call a geom_xx function without worrying about the arguments to put in `ggplot2::aes()`.
- New arguments sort.val and top in `ggbarplot()`.
    - sort.val: a string specifying whether the value should be sorted. Allowed values are "none" (no sorting), "asc" (for ascending) or "desc" (for descending).
    - top: a numeric value specifying the number of top elements to be shown.
- New function `theme_classic2()` added. Classic theme with axis lines.


## Minor changes

- `ggboxplot()`, `ggviolin()`, `ggdotplot()`, `ggstripchart()`, `gghistogram()`, `ggdensity()`, `ggecdf()` and `ggqqplot()` can now handle one single numeric vector.

```
# Example
ggboxplot(iris$Sepal.Length)
```

- Now, in `gghistogram()`, when add_density = TRUE, y scale remains = "..count..".
- Now, default theme changed to theme_classic2()
- Default point size and line size set to NULL



# ggpubr 0.1.0


## Plot one variable - X: Continuous

- ggdensity(): Density plot
- gghistogram(): Histogram plot
- ggecdf(): Empirical cumulative density function
- ggqqplot(): QQ plots


## Plot two variables - X & Y: Discrete X and Continuous Y

- ggboxplot(): Box plot
- ggviolin(): Violin plot
- ggdotplot(): Dot plot
- ggstripchart(): Stripchart (jitter)
- ggbarplot(): Bar plot
- ggline(): Line plot
- ggerrorplot(): Error plot
- ggpie(): Pie chart
- ggdotchart(): Cleveland's dot plots


## Plot two continuous variables

- ggscatter(): Scatter plot


## Graphical parameters

- ggpar(): Change graphical parameters
- show_line_type(): Line types available in R
- show_point_shapes(): Point shapes available in R
- theme_pubr(): Create a publication ready theme
- labs_pubr(): Format only plot labels to a publication ready style


## Genomics

- ggmaplot(): MA-plot from means and log fold changes


## Data

- diff_express: Differential gene expression analysis results


## Other

- desc_statby(): Descriptive statistics by groups
- stat_chull(): Plot convex hull of a set of points
- stat_conf_ellipse(): Plot confidence ellipses
- stat_mean(): Draw group mean points
