## Test environments
* local OS X install, R 3.6.0
* win-builder 
* Travis

## R CMD check results
There were no ERRORs, WARNINGs or NOTEs. 

## Downstream dependencies
I have also run R CMD check on downstream dependencies of ggpubr. 
All packages that I could install passed.

## Resubmission
  
This is a resubmission of version 0.5.0 . In this version I have:

* fixed issues so that the downstream dependencies `BinMat` passed R CMD check
