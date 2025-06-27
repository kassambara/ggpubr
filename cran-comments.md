## Test environments
* local Ubuntu 22.04.5 LTS install, R 4.4.1
* Github Action, set up using `usethis::use_github_action("check-standard")`

## R CMD check results
There were no ERRORs or WARNINGs.

## Downstream dependencies
I have also run R CMD check on downstream dependencies of ggpubr. 
All packages that I could install passed.

## Update

This is an updated version 0.6.1 (see NEWS.md). 

## Resubmission

This is a resubmission. In this version I have:
  
- Fixed namespace resolution issues with `after_stat()` calls that were causing failures in reverse dependency packages (`bSi` and `PopComm`). The issue has been resolved within ggpubr by ensuring proper evaluation environment setup, so no changes are required from the maintainers of affected packages.