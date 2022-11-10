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

* fixed the following issue in 'NEWS.md': Cannot extract version info from the following section titles:Bug fixes
* simplified the examples in `stat_anova_test`, so that the execution < 10s
