## Test environments
* local OS X install, R 3.6.0
* win-builder 
* Travis

## R CMD check results
There were no ERRORs, WARNINGs or NOTEs. 

## Downstream dependencies
I have also run R CMD check on downstream dependencies of ggpubr. 
All packages that I could install passed.

## Update

This is an updated version 0.6.0 (see NEWS.md). 

## Resubmission


This is a resubmission of version 0.6.0 . In this version I have:

* fixed issues so that the downstream dependencies `tinyarray` passed R CMD check
* Note that the issue concerning the "VALERIE" package is not related to ggpubr. This issue is a false positive
