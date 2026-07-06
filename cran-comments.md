## Release summary
- ggpubr 1.0.0: a large feature release (new p-value formatting system and many new
  arguments/computed variables across the plotting and statistical helpers), plus
  numerous bug fixes. See NEWS.md. The version was moved to 1.0.0 to mark a stable
  milestone; nearly all changes are additive/opt-in with unchanged default output.
- Last CRAN version: 0.6.3.

## Test environments
- macOS Tahoe 26.x, R 4.6.0 (aarch64-apple-darwin, local)
- GitHub Actions: Ubuntu (devel/release/oldrel-1), macOS (release), Windows (release)
- win-builder (devel and release)

## R CMD check results
- 0 errors | 0 warnings | 1 note
- The only NOTE is "checking for future file timestamps ... unable to verify
  current time", a transient artifact of the check host being unable to reach the
  time server; it is not related to the package.

## Reverse dependencies
- ggpubr has 316 reverse dependencies (Depends/Imports/Suggests).
- Only one change in this release alters default output: `compare_means(group.by = )`
  now adjusts p-values within each group rather than pooling across groups (#200).
- We scanned the sources of all 316 reverse dependencies for use of the changed
  functions. Only 3 packages call the behavior-changing path `compare_means(group.by = )`
  (easynem, immunarch, UCSCXenaShiny), and none of them pin the adjusted p-values in
  code that `R CMD check` evaluates (tests, examples, or vignettes). All remaining
  changes are additive/opt-in with unchanged defaults, so callers using existing
  arguments obtain identical output.
- Conclusion: no new problems expected in reverse dependencies.

## Additional comments
- The package metadata requires ggplot2 >= 3.5.2; this check was also run against
  ggplot2 4.0.3.
- win-builder (R-devel and R-release) flags "APA", "NEJM" and "GraphPad" in the
  Description as possibly misspelled. These are journal / style names used by the new
  p-value formatting presets (APA, NEJM, Lancet, AMA, GraphPad styles), not
  misspellings.
