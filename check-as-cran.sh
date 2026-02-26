#!/usr/bin/env bash
set -euo pipefail

# Run CRAN-style checks while disabling external time API validation.
# This avoids spurious "unable to verify current time" notes in restricted networks.

script_dir="$(cd "$(dirname "${BASH_SOURCE[0]}")" && pwd)"
cd "${script_dir}"

if [[ ! -f "DESCRIPTION" ]]; then
  echo "ERROR: DESCRIPTION not found in ${script_dir}" >&2
  exit 1
fi

tarball="$(Rscript -e 'd <- read.dcf("DESCRIPTION")[1, ]; cat(d["Package"], "_", d["Version"], ".tar.gz", sep = "")')"

echo "==> Building source tarball"
R CMD build .

if [[ ! -f "${tarball}" ]]; then
  echo "ERROR: Expected tarball not found: ${tarball}" >&2
  exit 1
fi

echo "==> Running CRAN check with _R_CHECK_SYSTEM_CLOCK_=FALSE"
_R_CHECK_SYSTEM_CLOCK_=FALSE R CMD check --as-cran "${tarball}" "$@"
