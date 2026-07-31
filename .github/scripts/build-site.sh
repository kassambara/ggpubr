#!/usr/bin/env bash
#
# Build the pkgdown site, then guarantee that no maintainer-private file reaches
# it.
#
# WHY THIS EXISTS
# ---------------
# pkgdown's internal package_mds() globs EVERY *.md in the package root (and in
# .github/) and renders each one into the site. It excludes only a hardcoded
# list -- README / LICENSE / LICENCE / NEWS, issue_template.md,
# pull_request_template.md, cran-comments.md -- and it does NOT consult
# .Rbuildignore. There is no configuration option to exclude a file.
#
# So a private CLAUDE.md sitting in the package root silently becomes a public
# CLAUDE.html, gets listed in sitemap.xml, AND gets its full text baked into
# search.json (the site search index). Deleting the .html alone is not enough:
# the page 404s while the whole document stays searchable.
#
# Note the no_render list is matched CASE-SENSITIVELY against "issue_template.md",
# so this package's uppercase ISSUE_TEMPLATE.md renders as a page titled "NA".
# Verify what pkgdown would render with:
#     Rscript -e 'basename(pkgdown:::package_mds("."))'
#
# Two defences, in order:
#   1. STRUCTURAL -- the real file lives at .claude/CLAUDE.md, which pkgdown
#      never globs (it looks only at the package root and .github/). Nothing to
#      render, nothing to leak. This script refuses to build if a root CLAUDE.md
#      has reappeared.
#   2. FAIL-CLOSED -- after the build, purge any CLAUDE / ISSUE_TEMPLATE artifact
#      from every output (html, md, sitemap.xml, search.json, llms.txt) across
#      BOTH docs/ and docs/dev/, then VERIFY. If anything survives, exit non-zero
#      so a contaminated site is never deployed.
#
# This script lives in .github/scripts/ on purpose: a tools/ directory can be
# copied into the published site via `copy:` in _pkgdown.yml; .github/ is not.
#
# Usage:
#   .github/scripts/build-site.sh              # build, scrub, verify
#   .github/scripts/build-site.sh --scrub-only # scrub + verify an existing docs/
#
set -euo pipefail

cd "$(dirname "$0")/../.."
DOCS="docs"
SCRUB_ONLY=0
[ "${1:-}" = "--scrub-only" ] && SCRUB_ONLY=1

# --- Defence 1: the package root must stay free of CLAUDE.md ----------------
if [ -e "CLAUDE.md" ] || [ -e "CLAUDE.local.md" ]; then
  cat >&2 <<'EOF'
ERROR: a CLAUDE.md / CLAUDE.local.md exists in the package root.

pkgdown will render it into a PUBLIC page and index its full text in
search.json. Project instructions belong in .claude/CLAUDE.md, which pkgdown
cannot see (it globs only the package root and .github/) and which is an
official project-instructions location of equal scope.

Fix:  mv CLAUDE.md .claude/CLAUDE.md    then re-run this script.
EOF
  exit 1
fi

# --- Build -------------------------------------------------------------------
if [ "$SCRUB_ONLY" -eq 0 ]; then
  Rscript -e 'pkgdown::build_site(preview = FALSE, install = FALSE)'
fi

[ -d "$DOCS" ] || { echo "ERROR: $DOCS/ does not exist." >&2; exit 1; }

# --- Defence 2a: purge rendered pages ---------------------------------------
# Covers the release tree (docs/) and the dev tree (docs/dev/, produced when
# DESCRIPTION carries a .9000/.999 suffix under `development: mode: auto`).
find "$DOCS" \( -iname 'CLAUDE*.html' -o -iname 'CLAUDE*.md' \
             -o -iname 'ISSUE_TEMPLATE.html' -o -iname 'ISSUE_TEMPLATE.md' \) \
     -type f -print -delete

# --- Defence 2b: purge sitemap entries --------------------------------------
while IFS= read -r sm; do
  perl -0pi -e 's{\s*<url>\s*<loc>[^<]*(?:CLAUDE|ISSUE_TEMPLATE)[^<]*</loc>.*?</url>}{}gsi' "$sm"
done < <(find "$DOCS" -name 'sitemap.xml' -type f)

# --- Defence 2c: purge the search index -------------------------------------
# search.json embeds the full text of every page, so deleting the .html alone
# leaves the entire document publicly searchable. This is the surface that bit
# the sibling repo.
while IFS= read -r sj; do
  python3 - "$sj" <<'PY'
import json, sys
p = sys.argv[1]
with open(p, encoding="utf-8") as f:
    data = json.load(f)

def leaks(entry):
    if not isinstance(entry, dict):
        return False
    blob = " ".join(
        str(entry.get(k, "")) for k in ("path", "title", "what", "previous_headings")
    ).upper()
    return "CLAUDE" in blob or "ISSUE_TEMPLATE" in blob

if isinstance(data, list):
    kept = [e for e in data if not leaks(e)]
    if len(kept) != len(data):
        with open(p, "w", encoding="utf-8") as f:
            json.dump(kept, f, ensure_ascii=False, separators=(",", ":"))
        n = len(data) - len(kept)
        print("purged %d entr%s from %s" % (n, "y" if n == 1 else "ies", p))
PY
done < <(find "$DOCS" -name 'search.json' -type f)

# --- Defence 2d: purge llms.txt (pkgdown >= 2.2 page listing) ---------------
while IFS= read -r lt; do
  if grep -qi 'claude\|issue_template' "$lt"; then
    grep -vi 'claude\|issue_template' "$lt" > "$lt.tmp" && mv "$lt.tmp" "$lt"
    echo "purged lines from $lt"
  fi
done < <(find "$DOCS" -name 'llms.txt' -type f)

# --- Verify, fail closed -----------------------------------------------------
leftover_files=$(find "$DOCS" -iname '*claude*' -print)
leftover_text=$(grep -rIl -i 'claude' "$DOCS" 2>/dev/null || true)
leftover_tmpl=$(find "$DOCS" -iname 'ISSUE_TEMPLATE.*' -print)

if [ -n "$leftover_files" ] || [ -n "$leftover_text" ] || [ -n "$leftover_tmpl" ]; then
  echo "" >&2
  echo "ERROR: private content still present in $DOCS/ -- DO NOT DEPLOY." >&2
  [ -n "$leftover_files" ] && { echo "files:" >&2;    echo "$leftover_files" >&2; }
  [ -n "$leftover_text" ]  && { echo "content:" >&2;  echo "$leftover_text" >&2; }
  [ -n "$leftover_tmpl" ]  && { echo "template:" >&2; echo "$leftover_tmpl" >&2; }
  exit 1
fi

echo ""
echo "OK: $DOCS/ is clean -- no CLAUDE or ISSUE_TEMPLATE artifact in any page,"
echo "sitemap, search index or llms.txt, in either the release or dev tree."
echo "Deploy with a MIRRORING sync (rsync --delete). A plain re-upload leaves an"
echo "already-published page and a stale search.json in place on the server."
