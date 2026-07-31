# ggpubr -- development helpers.
#
# Everything here is .Rbuildignore'd and never reaches the CRAN tarball.

all: test

test:
	Rscript --vanilla -e 'devtools::test()'

check:
	Rscript --vanilla -e 'devtools::check()'

document:
	Rscript --vanilla -e 'devtools::document()'

# Build the pkgdown site through the guard, which refuses to build while a root
# CLAUDE.md exists and fails closed if any private artifact survives into docs/.
# Never call pkgdown::build_site() directly -- it renders every root *.md into a
# public page and bakes the full text into search.json.
build_site:
	.github/scripts/build-site.sh

# Scrub + verify an existing docs/ tree without rebuilding.
scrub_site:
	.github/scripts/build-site.sh --scrub-only

.PHONY: all test check document build_site scrub_site
