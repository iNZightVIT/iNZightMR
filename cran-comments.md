## Resubmission
This is a resubmission. In this version I have:
* added executable examples to the Rd-files, and removed \dontrun{}
* refactored code to avoid global assignment (<<-)
* any necessary calls to par() have been followed immediately with on.exit
  to preserve the user's workspace
* there are no reference materials for the methods in this package
  (it was part of a student's unpublished research project)
* there are no external dependencies

In previous submissions, I have:
* specified in `Additional_repositories` where to find the 'iNZightPlots' package

## Test environments
* local ubuntu 18.04, R 3.6.1
* ubuntu 18.04 (on travis-ci) R oldrel, release, and devel
* macos (on travis-ci), R release
* win-builder (devel and release)

## R CMD check results

0 errors | 0 warnings | 2 notes

* This is a new submission

* [on winbuilder] checking package dependencies ... NOTE
  Package suggested but not available for checking: 'iNZightPlots'

## Downstream dependencies

There are currently no downstream dependencies for this package.
